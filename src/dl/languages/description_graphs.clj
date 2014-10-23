;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns dl.languages.description-graphs
  "Implements description graphs and common operations on them."
  (:use conexp.main
        dl.framework.syntax
        dl.framework.boxes
        dl.framework.semantics
        dl.util.graphs)
  (:use clojure.pprint)
  (:import [java.util HashMap HashSet]))

;;;

(defrecord Description-Graph [vertices neighbours vertex-labels]
  Object
  (toString [this]
    (str (list 'Description-Graph
               vertices
               neighbours
               vertex-labels))))

(defn description-graph?
  "Predicate to decide whether something is a description graph or not."
  [something]
  (instance? Description-Graph something))

(defn vertices
  "Returns vertices of given description graph."
  [^Description-Graph description-graph]
  (.vertices description-graph))

(defn neighbours
  "Returns the function mapping vertices to sets of pairs of roles and names."
  [^Description-Graph description-graph]
  (.neighbours description-graph))

(defn vertex-labels
  "Returns the vertex labeling function of given description graph."
  [^Description-Graph description-graph]
  (.vertex-labels description-graph))

(defn make-description-graph
  "Creates and returns a description graph for the given arguments."
  [vertices neighbours vertex-labels]
  (Description-Graph. vertices neighbours vertex-labels))

;;; Normalizing

(defn- conjunctors
  "Returns the elements of the dl-expression connected by
  conjunction. If the dl-expression is not a conjunction the singleton
  set containing the expression is returned."
  [dl-expr]
  (if (and (compound? dl-expr)
           (= 'and (operator dl-expr)))
    (set (arguments dl-expr))
    (set [dl-expr])))

;; tboxes as hash-maps

(defn- uniquify-tbox-map
  "Renames all defined concepts in the given tbox-map to be globally
  unique symbols. Returns a pair of the result and the transformation
  map used."
  [tbox-map]
  (let [old->new  (map-by-fn (fn [A]
                               (make-dl-expression (expression-language A) (gensym)))
                             (keys tbox-map)),
        old->new* (reduce! (fn [map [A B]]
                             (assoc! map (expression-term A) B))
                           {}
                           old->new)]
    [(reduce! (fn [map [A def-A]]
                (assoc! map
                        (old->new A)
                        (set-of (substitute term old->new*)
                                [term def-A])))
              {}
              tbox-map)
     old->new]))

(defn- tbox->hash-map
  "Transforms given TBox to a hash-map of defined concepts to the sets
  of concepts in the top-level conjunction."
  [tbox]
  (let [language (tbox-language tbox)]
    (reduce! (fn [map def]
               (assoc! map
                       (make-dl-expression language (definition-target def))
                       (conjunctors (definition-expression def))))
             {}
             (vals (tbox-definition-map tbox)))))

(defn- hash-map->tbox
  "Transforms given hash-map to a TBox for the given language."
  [language tbox-map]
  (make-tbox language
             (reduce! (fn [map [A def-A]]
                        (assoc! map
                                (expression-term A)
                                (make-dl-definition language
                                                    (expression-term A)
                                                    (conjunction def-A))))
                      {}
                      tbox-map)))

;; storing names

(defn- new-names
  "Returns a fresh data structure for storing new names."
  []
  (atom {}))

(defn- add-name
  "Adds to names the set-of-concepts under name."
  [names name set-of-concepts]
  (swap! names conj [name set-of-concepts]))

(defn- add-names
  "Adds the tbox-map to names."
  [names tbox-map]
  (swap! names into tbox-map))

(defn- get-names
  "Returns all names and their definitions (i.e. their set of
  concepts) stored in names."
  [names]
  @names)

;; normalizing algorithm - preparing the tbox and introducing new definitions

(defn- normalize-for-goal
  "If term satisfies goal, regards it as being normalized. Otherwise
  handles term as tbox and finally introduces a new symbol defining
  term. New definitions go into the atom new-names."
  [term goal new-names]
  (cond
   (goal term) term,
   (tbox-target-pair? term)
   (let [[tbox target] (expression-term term),
         [tbox-map trans] (uniquify-tbox-map (tbox->hash-map tbox))]
     (add-names new-names tbox-map)
     (trans (make-dl-expression (expression-language term) target))),
   :else
   (let [new-sym (make-dl-expression (expression-language term) (gensym))]
     (add-name new-names new-sym (conjunctors term))
     new-sym)))

(defn- normalize-term
  "Normalizes conjunctor term. New definitions go into the atom new-names."
  [term new-names]
  (cond
   (and (compound? term)
        (contains? #{'bottom 'top} (operator term)))
   term,
   (and (compound? term)
        (= 'exists (operator term)))
   (let [[r B] (arguments term),
         norm  (normalize-for-goal B
                                   #(and (atomic? %)
                                         (not (tbox-target-pair? %))
                                         (not (primitive? %)))
                                   new-names)]
     (make-dl-expression (expression-language term)
                         (list 'exists r norm))),
   :else
   (normalize-for-goal term
                       #(and (atomic? %)
                             (not (tbox-target-pair? %)))
                       new-names)))

(defn- introduce-auxiliary-definitions
  "Introduces auxiliary definitions into the given tbox-map (as
  returned by tbox->hash-map), such that the top-level conjunctions of
  all defined concepts only consist of defined concepts, primitive
  concepts or existential restrictions of defined concepts."
  [tbox-map]
  (loop [tbox-map       tbox-map,
         normalized-map {}]
    (if (empty? tbox-map)
      normalized-map
      (let [new-names      (new-names),
            normalized-map (into normalized-map
                                 (reduce! (fn [map [A def-A]]
                                            (assoc! map A (set-of (normalize-term t new-names)
                                                                  [t def-A])))
                                          {}
                                          tbox-map)),
            new-map        (get-names new-names)]
        (recur new-map normalized-map)))))

;; normalizing algorithm -- squeezing the concept graph

(defn- concept-graph
  "Returns the concept graph of a tbox-map. The graph has the defined
  concepts of tbox as vertices and connects every two vertices C to D
  if D appears in the top-level conjunction of C."
  [tbox-map]
  (let [defined-concepts (set (keys tbox-map))]
    (make-directed-graph defined-concepts
                         (fn [C]
                           (filter #(contains? defined-concepts %)
                                   (tbox-map C))))))

(defn- squeeze-equivalent-concepts
  "Returns a tbox-map where all equivalent, defined concepts of
  tbox-map are squeezed into one. If A is such a concept, every
  equivalent defined concept used in other definitions is substituted
  by A."
  [tbox-map]
  (let [equivalent-concepts (scc (concept-graph tbox-map)),
        rename-map          (reduce! (fn [map concepts]
                                       (let [new-concept (first concepts)]
                                         (reduce (fn [map concept]
                                                   (assoc! map concept new-concept))
                                                 map
                                                 concepts)))
                                     {}
                                     equivalent-concepts),
        used-map            (reduce! (fn [map concepts]
                                       (assoc! map (first concepts) concepts))
                                     {}
                                     equivalent-concepts),
        new-tbox-map        (reduce! (fn [map target]
                                       (assoc! map
                                               target
                                               (disj (set (replace rename-map
                                                                   (mapcat tbox-map (used-map target))))
                                                     target)))
                                     {}
                                     (vals rename-map))]
    (map-by-fn (comp new-tbox-map rename-map)
               (keys tbox-map))))

(defn- replace-toplevel-concepts
  "Replaces any top-level defined concept in tbox-map by its definition."
  [tbox-map]
  (loop [deps (dependency-list (concept-graph tbox-map)),
         new-tbox-map {}]
    (if (empty? deps)
      new-tbox-map
      (let [next-concepts (first deps),
            new-defs      (map-by-fn (fn [target]
                                       (reduce (fn [result next-thing]
                                                 (if (set? next-thing)
                                                   (into result next-thing)
                                                   (conj result next-thing)))
                                                #{}
                                                (replace new-tbox-map (tbox-map target))))
                                     next-concepts)]
        (recur (rest deps)
               (merge new-tbox-map new-defs))))))


;; normalizing algorithm -- invokation point

(defn normalize-gfp
  "Normalizes given TBox with gfp-semantics."
  [tbox]
  (let [language   (tbox-language tbox)
        result-map (-> tbox
                       tbox->hash-map
                       introduce-auxiliary-definitions
                       squeeze-equivalent-concepts
                       replace-toplevel-concepts)]
    (hash-map->tbox language result-map)))


;;; Conversion to and from description graphs

(defn tbox->description-graph
  "Converts a tbox to a description graph. Normalization is done with gfp semantics."
  [tbox]
  (let [tbox          (normalize-gfp tbox),
        vertices      (defined-concepts tbox),
        neighbours    (memo-fn _ [target]
                        (let [definition (definition-expression (find-definition tbox target)),
                              args       (if (and (compound? definition)
                                                  (= 'and (operator definition)))
                                           (arguments definition)
                                           (list definition))]
                          (set-of (vec (map expression-term (arguments t)))
                                  [t args :when (compound? t)])))
        vertex-labels (memo-fn _ [target]
                        (let [definition (definition-expression (find-definition tbox target)),
                              args       (if (and (compound? definition)
                                                  (= 'and (operator definition)))
                                           (arguments definition)
                                           (list definition))]
                          (set-of (expression-term t)
                                  [t args :when (atomic? t)])))]
    (make-description-graph vertices neighbours vertex-labels)))

(defn description-graph->tbox
  "Converts a description graph to a tbox."
  [description-graph language]
  (let [labels      (vertex-labels description-graph),
        neighbours  (neighbours description-graph),

        definitions (map-by-fn (fn [A]
                                 (make-dl-definition
                                  A
                                  (make-dl-expression language
                                                      (conjunction (concat (labels A)
                                                                           (for [[r B] (neighbours A)]
                                                                             (list 'exists r B)))))))
                               (vertices description-graph))]
    (make-tbox language definitions)))

(defn interpretation->description-graph
  "Converts given interpretation to a description graph."
  [interpretation]
  (let [concept-names (interpretation-concept-names interpretation),
        role-names    (interpretation-role-names interpretation),
        int-func      (interpretation-function interpretation),
        vertices      (interpretation-base-set interpretation),

        neighbours    (memo-fn _ [x]
                        (set-of [r y] [r role-names,
                                       [_ y] (filter #(= (first %) x) (int-func r))])),
        vertex-labels (memo-fn _ [x]
                        (set-of P [P concept-names,
                                   :when (contains? (int-func P) x)]))]
    (make-description-graph vertices neighbours vertex-labels)))

(defn interpretation->tbox
  "Converts a given interpretation to its corresponding tbox."
  [interpretation language]
  (let [tbox (description-graph->tbox (interpretation->description-graph interpretation)
                                      language)
        tbox (if (not-empty (tbox-definitions tbox))
               (first (tidy-up-ttp [tbox (first (defined-concepts tbox))]))
               tbox)]
    tbox))

(defn EL-concept-description->description-tree
  "Given an EL concept description `concept-description', returns a vector [G v], where
  `G' is the description tree of `concept-description', and `v' is the root of `G'."
  [concept-description]
  (assert (dl-expression? concept-description)
          "Argument `concept-description' must be a concept description")
  (assert (subset? (set (constructors (expression-language concept-description)))
                   '#{and exists bottom})
          "Argument `concept-description' must be a EL concept description.")
  (let [root         (gensym),
        args         (if-not (atomic? concept-description)
                       (arguments concept-description)
                       (list concept-description)),
        names        (filter atomic? args),
        existentials (remove atomic? args),
        subtrees     (map (fn [existential]
                            [(nth (expression-term existential) 1),
                             (EL-concept-description->description-tree
                              (make-dl-expression (expression-language concept-description)
                                                  (nth (expression-term existential) 2)))])
                          existentials)]
    [(make-description-graph (apply union
                                    #{root}
                                    (map (fn [[r [tree v]]]
                                           (vertices tree))
                                         subtrees))
                             (apply merge
                                    {root (vec (map (fn [[r [tree v]]]
                                                      [r v])
                                                    subtrees))}
                                    (map (fn [[r [tree v]]]
                                           (neighbours tree))
                                         subtrees))
                             (apply merge
                                    {root (set names)}
                                    (map (fn [[r [tree v]]]
                                           (vertex-labels tree))
                                         subtrees))),
     root]))

(defn description-tree->EL-concept-description
  "Given a description tree `description-graph' and its root `root', returns the
  corresponding EL concept description.

  Note: This function does not check whether the given description graph is acylic.  If
  its not, and the cycle is reachable from root, then this function will not terminate. "
  [language description-graph root]
  (assert (description-graph? description-graph)
          "Argument `description-graph' must be a description graph.")
  (assert (contains? (set (vertices description-graph)) root)
          "Argument `root' is not a vertex of the given description graph.")
  (let [labels                 ((vertex-labels description-graph) root),
        role-successors        ((neighbours description-graph) root),
        arguments              (concat labels
                                       (map (fn [[r v]]
                                              (list 'exists
                                                    r
                                                    (expression-term
                                                     (description-tree->EL-concept-description
                                                      language
                                                      description-graph
                                                      v))))
                                            role-successors)),
        term                   (if (= 1 (count arguments))
                                 (first arguments)
                                 (cons 'and arguments))]
    (make-dl-expression language term)))

(defn prune-description-graph
  "Given a description graph `graph', a node `target' in this graph and an integer `k',
  returns the unraveling of the graph starting at target of depth at most k.  The result
  returned is a vector [G x] of two entries, where `G' is the unravelled description graph
  and `x' is the renamed root node corresponding to the initially given target."
  [k graph target]
  (let [renamed (atom {}),
        neighs  (atom {})]
    (letfn [(collect [node current-depth] ;constraint: returns new name of node
              (let [new-name (gensym (str node "-"))]
                (swap! renamed assoc new-name node)
                (when (< 0 current-depth)
                  (doseq [[r v] ((neighbours graph) node)]
                    (swap! neighs
                           update-in [new-name]
                           conj [r (collect v (dec current-depth))])))
                new-name))]
      (let [target    (if (>= k 0)
                        (collect target k)
                        nil),
            vertices  (keys @renamed),
            labels    (fn [x]
                        ((vertex-labels graph) (@renamed x)))]
        (if target
          [(make-description-graph vertices
                                   @neighs
                                   labels),
           target]
          [(make-description-graph '[A]
                                   {}
                                   {}),
           'A])))))

;;;

(defn graph-product
  "Returns the product of the two description graphs given. Returns the directed connected component
  of the graph product containing node, if given."
  ([graph-1 graph-2]
     (let [vertices      (cross-product (vertices graph-1)
                                        (vertices graph-2)),
           neighbours    (fn [[A B]]
                           (set-of [r [C D]] [[r C] ((neighbours graph-1) A),
                                              [s D] ((neighbours graph-2) B),
                                              :when (= r s)])),
           vertex-labels (fn [[A B]]
                           (intersection ((vertex-labels graph-1) A)
                                         ((vertex-labels graph-2) B)))]
       (make-description-graph vertices neighbours vertex-labels)))
  ([graph-1 graph-2 node]
     (let [vertices      (loop [verts #{node},
                                newvs #{node}]
                           (if (empty? newvs)
                             verts
                             (let [nextvs (set-of [v w] | [x y] newvs
                                                          [r v] ((neighbours graph-1) x)
                                                          [s w] ((neighbours graph-2) y)
                                                          :when (= r s))]
                               (recur (into verts nextvs)
                                      (difference nextvs verts))))),

           neighbours    (fn [[A B]]
                           (set-of [r [C D]] [[r C] ((neighbours graph-1) A),
                                              [s D] ((neighbours graph-2) B),
                                              :when (= r s)])),
           vertex-labels (fn [[A B]]
                           (intersection ((vertex-labels graph-1) A)
                                         ((vertex-labels graph-2) B)))]
       (make-description-graph vertices neighbours vertex-labels))))

(defn description-graph-component
  "Returns the directed connected component of desgraph containing node."
  [desgraph node]
  (let [new-vertices (loop [verts #{node},
                            newvs #{node}]
                       (if (empty? newvs)
                         verts
                         (let [nextvs (set-of v | w newvs [_ v] ((neighbours desgraph) w))]
                           (recur (into verts nextvs)
                                  (difference nextvs verts)))))]
    (make-description-graph new-vertices
                            (neighbours desgraph)
                            (vertex-labels desgraph))))

;;; simulations

(defn- HashMap->hash-map
  "Converts a Java HashMap to a Clojure hash-map."
  [^HashMap map]
  (reduce! (fn [map, ^java.util.Map$Entry entry]
             (assoc! map (.getKey entry) (.getValue entry)))
           {}
           (seq map)))

(defmacro- while-let
  "Runs body with binding in effect as long as x is non-nil.

  binding => [x xs]"
  [binding & body]
  `(loop []
     (when-let ~binding
       ~@body
       (recur))))

;; schematic

(defn schematic-simulator-sets
  "Returns for all vertices v in the description graph G-1 the sets of
  vertices (sim v) in G-2 such that there exists a simulation from v to
  every vertex in (sim v)."
  [G-1 G-2]
  (let [label-1      (vertex-labels G-1),
        label-2      (vertex-labels G-2),
        neighbours-1 (neighbours G-1),
        neighbours-2 (neighbours G-2),
        edge-2?      (fn [v r w]
                       (contains? (neighbours-2 v) [r w])),

        ^HashMap sim-sets (HashMap.)]

    (doseq [v (vertices G-1)]
      (.put sim-sets v (set-of w [w (vertices G-2)
                                  :when (subset? (label-1 v) (label-2 w))])))

    (while-let [[u w] (first (for [u     (vertices G-1),
                                   [r v] (neighbours-1 u),
                                   w     (.get sim-sets u)
                                   :when (not (exists [x (.get sim-sets v)]
                                                (edge-2? w r x)))]
                               [u w]))]
      (.put sim-sets u
            (disj (.get sim-sets u) w)))

    (HashMap->hash-map sim-sets)))


;; efficient simulator sets (by meng)

(defn- single-edge->double-edge-graph
  "Part of the implementation of efficient-simulator-sets.

  Given a single-edged graph G (i.e. a graph with a neighbours
  function on it) returns a structure with a :pre and a :post function
  on it."
  [G]
  (let [pre-map (apply merge-with union
                       (map (fn [v]
                              (zipmap ((neighbours G) v) (repeat #{v})))
                            (vertices G)))]
    {:base-set (vertices G),
     :labels   (vertex-labels G),
     :post     (memo-fn _ [v r]
                 (set-of w [[s w] ((neighbours G) v)
                            :when (= s r)])),
     :pre      (memo-fn _ [v r]
                 (set (get pre-map [r v] #{})))}))

(defn efficient-initialize
  "Returns tripel [sim, remove, pre*] as needed by
  efficient-simulator-sets. sim, remove and pre* are Java HashMaps."
  [edge-labels G-1 G-2]
  (let [^HashMap sim    (HashMap.),
        ^HashMap remove (HashMap.),
        ^HashMap pre*   (HashMap.),

        label-1    (:labels G-1),
        label-2    (:labels G-2),
        base-set-1 (:base-set G-1),
        base-set-2 (:base-set G-2),
        post-1     (:post G-1),
        post-2     (:post G-2),
        pre-2      (:pre G-2),

        R edge-labels]

    (doseq [v base-set-1]
      (.put sim v
            (set-of u [u base-set-2,
                       :when (and (subset? (label-1 v) (label-2 u))
                                  (forall [r R]
                                    (=> (empty? (post-2 u r))
                                        (empty? (post-1 v r)))))]))
      (doseq [r R]
        (.put remove [v r]
              (set-of w [w base-set-2,
                         :when (and (not-empty (post-2 w r))
                                    (empty? (intersection (post-2 w r)
                                                          (.get sim v))))]))))

    (doseq [w base-set-2]
      (.put pre* w
            (set-of [u r] [r R, u (pre-2 w r)])))

    [sim remove pre*]))

(defn graph-role-names
  "Returns the role-names used in the graph."
  [^Description-Graph description-graph]
  (let [neighs (neighbours description-graph)]
    (set-of r | v (vertices description-graph)
                [r _] (neighs v))))

(defn efficient-simulator-sets
  "Implements EL-gfp-EfficientSimilaritiy (for the maximal simulation
  between two graphs) and returns the corresponding simulator sets."
  [G-1 G-2]
  (let [R (graph-role-names G-1),

        G-1 (single-edge->double-edge-graph G-1),
        G-2 (single-edge->double-edge-graph G-2),

        vars            (efficient-initialize R G-1 G-2),
        ^HashMap sim    (nth vars 0),
        ^HashMap remove (nth vars 1),
        ^HashMap pre*   (nth vars 2),

        ^HashSet non-empty-removes (HashSet.),

        base-set-1 (:base-set G-1),
        post-2     (:post G-2),
        pre-1      (:pre G-1)]

    (doseq [v base-set-1,
            r R,
            :when (not-empty (.get remove [v r]))]
      (.add non-empty-removes [v r]))

    (while-let [[v r] (first non-empty-removes)]
      (let [remove-v-r (.get remove [v r])]
        (.put remove [v r] #{})
        (doseq [u (pre-1 v r),
                w remove-v-r]
          (when (contains? (.get sim u) w)
            (.put sim u
                  (disj (.get sim u) w))
            (doseq [[w* r*] (.get pre* w),
                    :let [sim-u (.get sim u)]]
              (when (empty? (intersection (post-2 w* r*) sim-u))
                (.put remove [u r*]
                      (conj (or (.get remove [u r*]) #{})
                            w*))
                (.add non-empty-removes [u r*])))))
        (when (empty? (.get remove [v r]))
          (.remove non-empty-removes [v r]))))

    (HashMap->hash-map sim)))


;; simulation invocation point

(defn simulates?
  "Returns true iff there exists a simulation from G-1 to G-2, where
  vertex v in G-1 simulates vertex w in G-2."
  [G-1 G-2 v w]
  (let [sim-sets (efficient-simulator-sets G-1 G-2)]
    (contains? (get sim-sets v) w)))

;;;

nil
