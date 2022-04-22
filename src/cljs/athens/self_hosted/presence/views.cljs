(ns athens.self-hosted.presence.views
  (:require
    ["/components/PresenceDetails/PresenceDetails" :refer [PresenceDetails]]
    ["@chakra-ui/react" :refer [Avatar AvatarGroup Tooltip]]
    [athens.electron.utils :as electron.utils]
    [athens.router :as router]
    [athens.self-hosted.presence.events]
    [athens.self-hosted.presence.fx]
    [athens.self-hosted.presence.subs]
    [athens.util :as util]
    [clojure.string :as str]
    [re-frame.core :as rf]
    [reagent.core :as r]))


(defn user->person
  [{:keys [session-id username color]
    :page/keys [title]}]
  (when (and session-id username color)
    {:personId  session-id
     :username  username
     :color     color
     :pageTitle title}))


(defn copy-host-address-to-clipboard
  [host-address]
  (.. js/navigator -clipboard (writeText host-address))
  (util/toast (clj->js {:status "info"
                        :position "top-right"
                        :title "Host address copied to clipboard"})))


(defn copy-permalink
  []
  (let [selected-db @(rf/subscribe [:db-picker/selected-db])
        url (router/create-url-with-graph-param (:id selected-db))]
    (.. js/navigator -clipboard (writeText url))
    (util/toast (clj->js {:status "info"
                          :position "top-right"
                          :title "Copied permalink to clipboard"}))))


(defn go-to-user-block
  [all-users js-person]
  (let [{_block-uid :block/uid
         page-uid  :page/uid}
        (->> (js->clj js-person :keywordize-keys true)
             :personId
             (get all-users))]
    (if page-uid
      ;; TODO: if we support navigating to a block, it should be added here.
      (rf/dispatch [:navigate :page {:id page-uid}])
      (util/toast (clj->js {:title "User is not on any page"
                            :position "top-right"
                            :status "warning"})))))


(defn edit-current-user
  [js-person current-user]
  (let [{:keys [username color]} (js->clj js-person :keywordize-keys true)
        username-updated? (not= username (:username current-user))]
    (prn "updated?" username-updated?)
    (rf/dispatch [:settings/update :username username])
    (rf/dispatch [:settings/update :color color])
    (rf/dispatch [:presence/send-update {:username username
                                         :color    color}])
    (when username-updated?
      (rf/dispatch [:posthog/report-feature "presence.update.username"])
      (rf/dispatch [:posthog/identify! username]))))


;; Exports

(defn toolbar-presence-el
  []
  (r/with-let [selected-db            (rf/subscribe [:db-picker/selected-db])
               current-user           (rf/subscribe [:presence/current-user])
               all-users              (rf/subscribe [:presence/users-with-page-data])
               same-page              (rf/subscribe [:presence/same-page])
               diff-page              (rf/subscribe [:presence/diff-page])
               others-seq             #(->> (dissoc % (:session-id @current-user))
                                            vals
                                            (map user->person)
                                            (remove nil?))]
              (fn []
                (let [current-user'          (user->person @current-user)
                      current-page-members   (others-seq @same-page)
                      different-page-members (others-seq @diff-page)]
                  [:> PresenceDetails (merge
                                        {:current-user             current-user'
                                         :current-page-members     current-page-members
                                         :different-page-members   different-page-members
                                         :host-address             (:url @selected-db)
                                         :handle-copy-host-address copy-host-address-to-clipboard
                                         :handle-press-member      #(go-to-user-block @all-users %)
                                         :handle-update-profile    #(edit-current-user % current-user')
                                         ;; TODO: show other states when we support them.
                                         :connection-status        "connected"}
                                        (when-not electron.utils/electron?
                                          {:handle-copy-permalink copy-permalink}))]))))


;; inline

(defn inline-presence-el
  [uid]
  (let [users (rf/subscribe [:presence/has-presence (util/embed-uid->original-uid uid)])]
    (when (seq @users)
      (into
        [:> Tooltip {:label (->> @users (map user->person)
                                 (remove nil?)
                                 (map (fn [person] (:username person)))
                                 (str/join ", "))}
         [:> AvatarGroup {:max 1
                          :zIndex 2
                          :size "xs"
                          :cursor "default"
                          :gridArea "presence"}
          (->> @users
               (map user->person)
               (remove nil?)
               (map (fn [{:keys [personId] :as person}]
                      [:> Avatar {:key personId
                                  :bg (:color person)
                                  :name (:username person)}])))]]))))

