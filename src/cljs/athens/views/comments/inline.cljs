(ns athens.views.comments.inline
  (:require [re-frame.core :as rf]
            ["/components/Icons/Icons" :refer [PencilIcon]] ;; should be a comment icon
            ["@chakra-ui/react" :refer [Button Input]]
            [athens.parse-renderer :as parse-renderer]
            [goog.events :as events])
  (:import
    (goog.events
      KeyCodes)))



(def comments-styles
  {:margin-left "30px"
   :background-color "#e7e7e7"
   :border-radius "5px"})


(def comment-styles
  {:padding "5px 10px 5px 10px"})


(def dropdown-style
  {:display "inline-flex"
   ;; :color (color :body-text-color)
   ;;:z-index (:zindex-dropdown ZINDICES)
   :padding "0.25rem"
   :border-radius "calc(0.25rem + 0.25rem)" ; Button corner radius + container padding makes "concentric" container radius
   :min-height "2em"
   :min-width "2em"
   :animation "dropdown-appear 0.125s"
   :animation-fill-mode "both"
   ;;:background (color :background-plus-2)
   ;;:box-shadow [[(:64 DEPTH-SHADOWS) ", 0 0 0 1px rgba(0, 0, 0, 0.05)"]]
   :flex-direction "column"})


(def menu-style
  {:display "grid"
   :grid-gap "0.125rem"
   :min-width "9em"
   :align-items "stretch"
   :grid-auto-flow "row"
   :overflow "auto"
   ;; ::stylefy/manual [[(selectors/& (selectors/not (selectors/first-child))) {:margin-block-start "0.25rem"}]
   ;;                   [(selectors/& (selectors/not (selectors/last-child))) {:margin-block-end "0.25rem"}]
   ;;                  [:button {:min-height "1.5rem"}]]

   })


(defn inline-comment-textarea
  [uid]
  (let [comment-string (reagent.core/atom "")]
    (fn [uid]
      (let [username @(rf/subscribe [:username])]
        [:div
         [:> Input            {:placeholder "Add a comment..." :style {:width "90%"
                                                                       :margin "10px"}
                               :on-change   (fn [e] (reset! comment-string (.. e -target -value)))
                               :value       @comment-string
                               :on-key-down (fn [e]
                                              (when (= (.. e -keyCode) KeyCodes.ENTER)
                                                (re-frame.core/dispatch [:comment/write-comment uid @comment-string username])
                                                (re-frame.core/dispatch [:comment/hide-comment-textarea])
                                                (reset! comment-string nil)))}]
         [:> Button {:style    {:float "right"
                                :margin-top "9px"
                                :margin-right "1px"}
                     :on-click (fn [_]
                                 (re-frame.core/dispatch [:comment/write-comment uid @comment-string username])
                                 (re-frame.core/dispatch [:comment/hide-comment-textarea])
                                 (reset! comment-string nil))}
          "Send"]]))))

(defn copy-comment-uid
  [comment-data state]
  (let [uid (:block/uid comment-data)
        ref (str "((" uid "))")]
    (.. js/navigator -clipboard (writeText ref))
    (swap! state update :comment/show? not)))

(defn show-comment-context-menu
  [comment-data state]
  (let [{:comment/keys [x y]} @state
        handle-click-outside  #(when (:comment/show? @state)
                                 (swap! state assoc :comment/show? false))]
    (reagent.core/create-class
      {:component-did-mount    (fn [_this] (events/listen js/document "mousedown" handle-click-outside))
       :component-will-unmount (fn [_this] (events/unlisten js/document "mousedown" handle-click-outside))
       :reagent-render         (fn [comment-data state]
                                 [:div {:style (merge dropdown-style
                                                      {:position "fixed"
                                                       :left x
                                                       :top  y})}
                                  [:div {:style menu-style}
                                   [:> Button {:on-mouse-down #(copy-comment-uid comment-data state)}
                                    "Copy comment uid"]]])})))

(def author-style {:font-size "80%" :margin-right "15px" :color "gray"})
(def content-style {:font-size "90%"})
(def time-style {:font-size "80%" :color "gray"})

(defn comment-el
  [item]
  (let [{:keys [string time author block/uid]} item
        linked-refs (athens.reactive/get-reactive-linked-references [:block/uid uid])
        linked-refs-count (count linked-refs)
        state (reagent.core/atom {:comment/show? false
                                  :comment/x     nil
                                  :comment/y     nil})]

    (fn [item]
      [:<>
       [:div.comment (merge
                       {:on-context-menu (fn [e]  (swap! state update :comment/show? not)
                                                  (swap! state assoc :comment/x (.-clientX e)
                                                                     :comment/y (.-clientY e)))
                        :on-click  #(swap! state assoc :comment/show? false)}
                       {:style  comment-styles})
        (when (:comment/show? @state)
          [show-comment-context-menu item state])

        [:span {:style author-style}
         author]
        [:span {:style content-style}
         ;; In future this should be rendered differently for reply type and ref-type
         [athens.parse-renderer/parse-and-render string uid]]
        [:div (merge {:style time-style}
                     {:style {:float "right"}})
         (when (pos? linked-refs-count)
           [:span {:style {:margin-right "30px"}} linked-refs-count])
         [:span time]]]])))

(defn inline-comments
  [data uid hide?]
  (let [state        (reagent.core/atom {:hide? hide?})
        num-comments (count data)
        first-comment (first data)
        {:keys [author string time]} first-comment]
    (fn [data uid]
      [:<>
       ;; add time, author, and preview
       [:div {}
        [:> Button {:sx       (merge comments-styles {:width "95%" :font-weight "normal"})
                    :on-click #(swap! state update :hide? not)}
         (if (:hide? @state)
           [:<>
            [:> PencilIcon]
            num-comments
            [:div {:style {:width "100%"}}
             [:span {:style {:margin-right "5px" :font-size "90%" :color "gray"}} (str "@" author)]
             [:span {:style (merge {:overflow      "hidden"
                            :text-overflow "ellipsis"
                            :white-space   "nowrap"}
                           content-style)} string]]
            [:span {:style time-style} time]]
           [:span {:style {:margin-left "30px" :font-style "90%" }} "Hide Comments"])]]

       (when-not (:hide? @state)
         [:div.comments {:style comments-styles}
          (for [item data]
            [comment-el item])
          [inline-comment-textarea uid]])])))

