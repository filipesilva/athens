(ns athens.data
  (:require
    [athens.common-db :as common-db]
    [athens.common-events.graph.atomic :as atomic]
    [athens.common-events.graph.ops :as graph-ops]
    [athens.common.utils :as utils]
    [clojure.walk :as walk]
    [datascript.core :as d]
    [hyperfiddle.rcf :refer [tests]]))


;; TODO: the name data, or even athens-data, isn't very good. iterate on it.
;; TODO: the data->ops is taken from athens.common-events.bfs/internal-representation->atomic-ops
;; and datascript->data is taken from athens.common-db/get-internal-representation.
;; Make this the new location, remove duplicated code, and wire things together.



;; test helpers

(defn- db-with
  [tx-data]
  (d/db-with (d/empty-db common-db/schema) tx-data))


(def ^:private uids
  ;; Infinite lazy seq of block uids
  (repeatedly utils/gen-block-uid))


(defn uid-n
  [n]
  (nth uids n))


;; athens data -> atomic ops

(defn- add-missing-block-uids
  [athens-data]
  (walk/postwalk
      (fn [x]
        (if (and (map? x)
                 (not (:block/uid x))
                 (not (:page/title x))
                 (or (:block/string x)
                     (:block/children x)
                     (:block/named-children x)))
          (assoc x :block/uid (utils/gen-block-uid))
          x))
      athens-data))


(defn- enhance-block
  [block previous parent]
  (merge block
         {:parent (let [page-ks (select-keys parent [:page/title])]
                    (if (empty? page-ks)
                      (select-keys parent [:block/uid])
                      page-ks))}
         {:previous (select-keys previous [:block/uid])}))


(defn- enhance-children
  [children parent]
  ;; Partition by 2 with nil first/last elements to get each [previous current] pair.
  ;; https://stackoverflow.com/a/41925223/2116927
  (->> (concat [nil] children [nil])
       (partition 2 1)
       (map (fn [[previous current]]
              (when current (enhance-block current previous parent))))
       (remove nil?)
       vec))


(defn- enhance-named-children
  [named-children parent]
  (into {} (map (fn [[k v]] [k (assoc v :parent parent :block/name k)])) named-children))


(defn- enhance-data
  "Enhance an athens datas' individual elements with a reference to parent and previous elements.
  Parents will be referenced by maps with either :page/title or :block/uid, previous will be :block/uid.
  Toplevel pages will be sorted after all toplevel blocks. "
  [athens-data]
  (let [{blocks true
         pages  false} (group-by (comp nil? :page/title) athens-data)
        ;; Enhance toplevel blocks as if they had a nil parent.
        ;; The first block will have neither a parent or a previous.
        blocks         (or blocks [])
        pages          (or pages [])
        blocks'        (enhance-children blocks nil)]
    (walk/postwalk
      (fn [x]
        (if (map? x)
          (let [{:block/keys [children named-children]} x]
            (cond-> x
              children       (assoc :block/children (enhance-children children x))
              named-children (assoc :block/named-children (enhance-named-children named-children x))))
          x))
      (concat blocks' pages))))


(defn enhanced-data->atomic-ops
  "Takes the enhanced athens data and creates :page/new or :block/new and :block/save atomic events.
  Throws if default-position is nil and position cannot be determined."
  [db default-position {:keys [block/uid block/string block/name block/open? page/title previous parent] :as eir}]
  (if title
    [(atomic/make-page-new-op title)]
    (let [{parent-title :page/title
           parent-uid   :block/uid} parent
          previous-uid              (:block/uid previous)
          name-relation             (when name {:name name})
          position                  (cond
                                      ;; There's a block before this one that we can add this one after.
                                      previous-uid {:block/uid previous-uid  :relation :after}
                                      ;; There's no previous block, but we can add it to the end of the parent
                                      ;; or under a name, if any.
                                      parent-title {:page/title parent-title :relation (or name-relation :last)}
                                      parent-uid   {:block/uid parent-uid    :relation (or name-relation :last)}
                                      ;; There's a default place where we can drop blocks, use it.
                                      default-position default-position
                                      :else (throw (ex-info "Cannot determine position for enhanced athens data" eir)))
          save-op                   (graph-ops/build-block-save-op db uid string)
          atomic-save-ops           (if (graph-ops/atomic-composite? save-op)
                                      (graph-ops/extract-atomics save-op)
                                      [save-op])]
      (cond-> (into [(atomic/make-block-new-op uid position)] atomic-save-ops)
        (= open? false) (conj (atomic/make-block-open-op uid false))))))


(defn move-save-ops-to-end
  [coll]
  (let [{save true
         not-save false} (group-by #(= (:op/type %) :block/save) coll)]
    (concat [] not-save save)))


(defn has-children?
  [{:block/keys [children named-children]}]
  (or children named-children))


(defn get-all-children
  [{:block/keys [children named-children]}]
  (concat children (vals named-children)))


(defn- data->ops-impl
  "Convert athens data to the vector of atomic operations that would create it.
  :block/save operations are grouped at the end so that any ref'd entities are already created."
  [db athens-data default-position]
  (->> athens-data
       add-missing-block-uids
       enhance-data
       (mapcat (partial tree-seq has-children? get-all-children))
       (map (partial enhanced-data->atomic-ops db default-position))
       flatten
       move-save-ops-to-end
       vec))


;; TODO: support shortcuts
;; TODO: require default-position for blocks
;; TODO: how should default-position work for named children?
;;       Might need a different model.
;;       Maybe make relation optional, don't add that block, default to last for children.
(defn data->ops
  [{:athens/keys [pages blocks position sidebar] :as _athens-data} & {:keys [db]}]
  ;; TODO: should work without db at all, but some graph ops need it even if empty.
  ;; TODO: resolvers should be ok with recreating a page, should not stop processing
  ;; whole op, otherwise this always needs the db.
  (let [db' (or db (d/empty-db common-db/schema))]
    (cond-> []
      pages                 (into (data->ops-impl db' pages nil))
      ;; TODO: sidebar
      (and pages sidebar)   []
      (and blocks position) (into (data->ops-impl db' blocks position)))))



(tests
  "two pages"
  (data->ops
   {:athens/pages [{:page/title "p1"}
                   {:page/title "p2"}]})
  :=
  [#:op{:type :page/new :atomic? true :args #:page{:title "p1"}}
   #:op{:type :page/new :atomic? true :args #:page{:title "p2"}}]


  #_
  "two pages, one in sidebar"


  "supports missing uids"
  (data->ops
   {:athens/pages [{:page/title "p1"
                    :block/children [{:block/string "b1"}]}]})
  :=
  #_:clj-kondo/ignore
  [#:op{:type :page/new :atomic? true :args #:page{:title "p1"}}
   #:op{:type    :block/new
        :atomic? true
        :args    #:block{:uid      ?uid
                         :position {:page/title "p1" :relation :last}}}
   #:op{:type    :block/save
        :atomic? true
        :args    #:block{:uid ?uid :string "b1"}}]


  "supports blocks given position with relation"
  (data->ops
   {:athens/blocks [{:block/string "b1"}
                    {:block/string "b2"}]
    :athens/position {:page/title "p1" :relation :last}})
  :=
  #_:clj-kondo/ignore
  [#:op{:type    :block/new
        :atomic? true
        :args    #:block{:uid      ?uid1
                         :position {:page/title "p1" :relation :last}}}
   #:op{:type    :block/new
        :atomic? true
        :args    #:block{:uid      ?uid2
                         :position {:block/uid ?uid1 :relation :after}}}
   #:op{:type    :block/save
        :atomic? true
        :args    #:block{:uid ?uid1 :string "b1"}}
   #:op{:type    :block/save
        :atomic? true
        :args    #:block{:uid ?uid2 :string "b2"}}]

  #_
  "supports blocks given position without relation"


  "one page with some nested blocks"
  (data->ops
   {:athens/pages [{:page/title "p1"
                    :block/children
                    [#:block{:string "b1"
                             :uid    (uid-n 0)
                             :children
                             [#:block{:string "b2"
                                      :uid    (uid-n 1)}
                              #:block{:string "b3"
                                      :uid    (uid-n 2)}]
                             :named-children
                             {"n1" #:block{:uid    (uid-n 3)
                                           :string "b4"}}}]}]})
  :=
  [#:op{:type :page/new :atomic? true :args #:page{:title "p1"}}
   #:op{:type    :block/new
        :atomic? true
        :args    #:block{:uid      (uid-n 0)
                         :position {:page/title "p1" :relation :last}}}
   #:op{:type    :block/new
        :atomic? true
        :args
        #:block{:uid      (uid-n 1)
                :position {:block/uid (uid-n 0) :relation :last}}}
   #:op{:type    :block/new
        :atomic? true
        :args
        #:block{:uid      (uid-n 2)
                :position {:block/uid (uid-n 1) :relation :after}}}
   #:op{:type    :block/new
        :atomic? true
        :args
        #:block{:uid (uid-n 3)
                :position
                {:block/uid (uid-n 0) :relation {:name "n1"}}}}
   #:op{:type    :block/save
        :atomic? true
        :args    #:block{:uid (uid-n 0) :string "b1"}}
   #:op{:type    :block/save
        :atomic? true
        :args    #:block{:uid (uid-n 1) :string "b2"}}
   #:op{:type    :block/save
        :atomic? true
        :args    #:block{:uid (uid-n 2) :string "b3"}}
   #:op{:type    :block/save,
        :atomic? true,
        :args    #:block{:uid (uid-n 3), :string "b4"}}])


;; datascript -> athens data

(def block-document-pull-vector-for-copy
  '[:node/title :block/uid :block/string :block/open :block/order :block/name
    {:block/children ...} {:block/_parent ...}])


(defn- dissoc-on-match
  [m [k f]]
  (if (f m)
    (dissoc m k)
    m))


(defn- xform-on-match
  [m [k f]]
  (if-let [v (get m k)]
    (assoc m k (f v))
    m))


(defn- vector->map
  [f v]
  (->> v
       (map (fn [i] [(f i) i]))
       (into {})))


(defn get-eid-data
  "Returns internal representation for eid in db."
  [db eid]
  (when (d/entity db eid)
    (let [rename-ks          {:block/open    :block/open?
                              :node/title    :page/title
                              :block/_parent :block/named-children}
          remove-ks-on-match [[:block/order (constantly true)]
                              ;; name is already in the map key
                              [:block/name  (constantly true)]
                              [:block/open? :block/open?]
                              [:block/uid   :page/title]]
          xform-ks           [[:block/named-children
                               (partial vector->map :block/name)]]]
      (->> (d/pull db block-document-pull-vector-for-copy eid)
           common-db/sort-block-children
           (walk/postwalk-replace rename-ks)
           (walk/prewalk (fn [node]
                           (if (map? node)
                             (reduce xform-on-match node xform-ks)
                             node)))
           (walk/prewalk (fn [node]
                           (if (map? node)
                             (reduce dissoc-on-match node remove-ks-on-match)
                             node)))))))


(defn get-all-page-eids
  [db]
  (->> (d/datoms db :aevt :node/title)
       (map first)))


(defn get-ordered-sidebar-titles
  [db]
  (->> (d/q '[:find ?order ?title
              :where
              [?e :page/sidebar ?order]
              [?e :node/title ?title]] db)
       seq
       (sort-by first)
       (mapv second)))


(defn datascript->data
  ([db]
   (merge
     {:athens/pages (->> (get-all-page-eids db)
                         (mapv (partial get-eid-data db)))}
     (let [v (get-ordered-sidebar-titles db)]
       (when (seq v)
         {:athens/sidebar v}))))

  ;; TODO: doit or maybe doit in another fn
  #_([db eid]
   ;; get subtree for eid
   ;; determine if eid is page or block
   ;; create athens-data structure with subtree in
   ;; if page and in sidebar, also include a sidebar entry
   (if true
     {:athens/pages   []
      :athens/sidebar []}
     {:athens/blocks []
      :athens/position
      ;; TODO: differentiate between title and other
      {:page/title "title"
       :relation   :first}})))


;; Arity 1
(tests
  "two pages"
  (datascript->data
    (db-with [{:node/title "p1"}
              {:node/title "p2"}]))
  :=
  {:athens/pages [{:page/title "p1"}
                  {:page/title "p2"}]}

  "two pages, one in sidebar"
  (datascript->data
    (db-with [{:node/title "p1"}
              {:node/title   "p2"
               :page/sidebar 0}]))
  :=
  {:athens/pages   [{:page/title "p1"}
                    {:page/title "p2"}]
   :athens/sidebar ["p2"]}

  "one page with some nested blocks"
  (datascript->data
    (db-with [{:node/title "p1"
               :block/children
               [#:block{:uid    (uid-n 0)
                        :string "b1"
                        :open?  false
                        :children
                        [#:block{:uid    (uid-n 1)
                                 :string "b2"
                                 :order  0}
                         #:block{:uid    (uid-n 2)
                                 :string "b3"
                                 :order  1}]
                        :_parent
                        [#:block {:uid    (uid-n 3)
                                  :string "b4"
                                  :name   "n1"}]}]}]))
  :=
  {:athens/pages [{:page/title "p1"
                   :block/children
                   [#:block{:string "b1"
                            :uid    (uid-n 0)
                            :children
                            [#:block{:uid    (uid-n 1)
                                     :string "b2"}
                             #:block{:uid    (uid-n 2)
                                     :string "b3"}]
                            :named-children
                            {"n1" #:block{:uid    (uid-n 3)
                                          :string "b4"}}}]}]})


(defn get-named-children-as-data
  [db eid])
