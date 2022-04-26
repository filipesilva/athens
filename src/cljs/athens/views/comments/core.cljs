(ns athens.views.comments.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [athens.data :as athens.data]
            [athens.views.comments.inline :as inline]
            [athens.views.comments.right-side :as right-side]
            [athens.common-events.graph.atomic :as atomic-graph-ops]
            [athens.common-events.graph.ops :as graph-ops]
            [athens.common-events.graph.composite :as composite-ops]
            [re-frame.core :as rf]
            [athens.bot :as bot]
            [athens.common-events :as common-events]
            [athens.common.utils :as common.utils]
            [athens.common-db :as common-db]
            [athens.db :as db]
            [athens.common-events.graph.composite :as composite]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

;; user presses "Comment" from context-menu
;; place to write comment appears
;; user writes their comment
;; user presses enter or "Comment"
;; comment appears in read-state


;; COMMENT STATES
;; write a comment
;; indicator that comments exist (but hidden)
;; list of comments and a place to write a comment

#_{:comments [{:uid "" ;; source
               :comments []}]}

(def comments-ratom (reagent.core/atom []))


(rf/reg-fx
  :add-comment
  (fn [x]
    (prn "fx" x)))

(rf/reg-sub
  :comment/show-comment-textarea?
  (fn [db [_ uid]]
    (= uid (:comment/show-comment-textarea db))))

(rf/reg-event-fx
  :comment/show-comment-textarea
  (fn [{:keys [db]} [_ uid]]
    {:db (assoc db :comment/show-comment-textarea uid)}))

(rf/reg-event-fx
  :comment/hide-comment-textarea
  (fn [{:keys [db]} [_ uid]]
    {:db (assoc db :comment/show-comment-textarea nil)}))

(defn show-comments-actively-block
  [db page-title last-block]
  (if (not= "Actively show comments on this page. NOTE: Please do not create a sibling block after this block."
            (:block/string last-block))
    (let [new-block-uid      (common.utils/gen-block-uid)
          new-block-op       (atomic-graph-ops/make-block-new-op new-block-uid {:page/title page-title
                                                                                :relation   :last})
          new-block-save-op  (graph-ops/build-block-save-op @db/dsdb new-block-uid "Actively show comments on this page. NOTE: Please do not create a sibling block after this block.")]
      [new-block-uid [new-block-op new-block-save-op]])
    nil))





(defn new-comment
  [db thread-uid str author time]
  (->> (athens.data/data->ops
        {:athens/position {:block/uid thread-uid
                           :relation  :last}
         :athens/blocks   [{:block/string str
                            :block/named-children
                            {":comments/author" {:block/string author}
                             ":comments/time"   {:block/string time}}}]}
        :db db)
       (composite/make-consequence-op {:op/type :new-comment})))


(defn new-comment-thread
  [thread-uid parent-uid]
  (atomic-graph-ops/make-block-new-op thread-uid
                                      {:block/uid parent-uid
                                       :relation  {:name ":comments/thread"}}))


(defn get-comment-thread-uid
  [db parent-uid]
  (-> (common-db/get-named-children db [:block/uid parent-uid])
      (get ":comments/thread")
      :block/uid))


(defn block->comment-map
  [{:block/keys [string named-children]}]
  {:string string
   :author (-> named-children (get ":comments/author") :block/string)
   :time   (-> named-children (get ":comments/time") :block/string)})



(defn get-thread-comments
  [db thread-uid]
  (->> (athens.data/get-eid-data db [:block/uid thread-uid])
       :block/children
       (map block->comment-map)))


(defn has-comments?
  [db uid]
  (boolean (get-comment-thread-uid db uid)))


(rf/reg-event-fx
  :comment/write-comment
  (fn [{db :db} [_ uid comment-string author]]
    (let [block                     (common-db/get-block @db/dsdb [:block/uid uid])
          existing-thread-uid       (get-comment-thread-uid @db/dsdb uid)
          thread-uid                (or existing-thread-uid
                                        (common.utils/gen-block-uid))
          ;; notif-ops                 (bot/create-notifs-ops db block author bot/athens-users comment-block-uid)
          active-comment-ops        (composite/make-consequence-op {:op/type :active-comments-op}
                                                                   (into (if existing-thread-uid
                                                                           []
                                                                           [(new-comment-thread thread-uid uid)])
                                                                         [(new-comment @db/dsdb thread-uid comment-string author "12:09 pm")
                                                                          ;; disabled while testing locally
                                                                          #_ notif-ops]))

          event                     (common-events/build-atomic-event active-comment-ops)]


      (println uid block)
      (println event)

      {:fx [[:dispatch [:resolve-transact-forward event]]
            #_[:dispatch [:prepare-message uid author :comment {:string comment-string}]]]})))


(println (common-db/get-block @db/dsdb [:block/uid "c5e8e8655"]))
(println (common-db/get-named-children @db/dsdb [:block/uid "c5e8e8655"]))
(println (get-comment-thread-uid @db/dsdb "c5e8e8655")) ;; eadbb2a55
(println (get-thread-comments @db/dsdb "eadbb2a55"))


(rf/reg-sub
  :comment/show-inline-comments?
  (fn [db [_]]
    (= true (:comment/show-inline-comments db))))

(rf/reg-sub
  :comment/show-right-side-comments?
  (fn [db [_]]
    (= true (:comment/show-right-side-comments db))))

(rf/reg-event-fx
  :comment/toggle-inline-comments
  (fn [{:keys [db]} [_]]
    (let [current-state (:comment/show-inline-comments db)]
      {:db (assoc db :comment/show-inline-comments (not current-state))})))

(rf/reg-event-fx
  :comment/toggle-right-side-comments
  (fn [{:keys [db]} [_]]
    (let [current-state (:comment/show-right-side-comments db)]
      {:db (assoc db :comment/show-right-side-comments (not current-state))})))


(def mock-data
  [{:string "[[Brandon Toner]] Agree with the jumpiness"}
   {:string "[[Matt Vogel]] Also experiencing this. Someone closed the parent of the block I was on (I was not zoomed in) and I got kicked out of the block"}])


(def mock-data-with-author-and-time
  [{:string "Agree with the jumpiness"
    :author "Brandon Toner"
    :time "12:30pm"}
   {:string "Also experiencing this. Someone closed the parent of the block I was on (I was not zoomed in) and I got kicked out of the block"
    :author "Matt Vogel"
    :time "12:35pm"}])


