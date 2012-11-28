;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; A program to convert data from the dbpedia project to DL models

(ns dl.util.read-rdf-models
  (:use [conexp.io.util :only (with-in-reader)]
        [clojure.walk :only (walk)])
  (:use conexp.main
        dl.framework.syntax
        dl.framework.boxes
        dl.framework.semantics
        dl.languages.EL-gfp
        dl.languages.EL-gfp-exploration
        dl.languages.interaction))

(ns-doc
 "Utility functions to read DL models from DBpedia data files.")

;;;

(defn- rdf-line-to-pair
  "Converts RDF line to a pair [role [First Second]]."
  [line]
  (let [[A to B-1 B-2] (rest (re-find #"<(.*)> <(.*)> (?:<(.*)>|\"(.*)\")" line))]
    [to [A (or B-1 B-2)]]))

(defn- map-count
  "Counts overall entries in a map."
  [hash-map]
  (reduce + (map #(count (get hash-map %)) (keys hash-map))))

(defn- read-rdf-lines-from-file
  "From the given file reads in RDF triples and returns a map mapping relation-names to relations."
  ([file]
     (read-rdf-lines-from-file file (constantly true) (constantly true) (constantly true)))
  ([file interesting-role? interesting-A? interesting-B?]
     (with-in-reader file
       (binding [*in* (clojure.lang.LineNumberingPushbackReader. *in*)]
         (loop [map        {},
                line-count 0]
           (if-let [line (read-line)]
             (do
               ;; (when (zero? (mod line-count 10000))
               ;;   (println line-count (map-count map)))
               (let [[role [A B]] (rdf-line-to-pair line)]
                 (recur (if (and role A B
                                 (interesting-role? role)
                                 (interesting-A? A)
                                 (interesting-B? B))
                          (update-in map [role] conj [A B])
                          map)
                        (inc line-count))))
             (do
;;               (println line-count)
               map)))))))

(defn- capitalize
  "Capitalizes word."
  [word]
  (if (empty? word)
    word
    (apply str (Character/toUpperCase ^Character (first word)) (rest word))))

(defn- symbolify
  "Transforms every string in coll to a symbol, walking through sequential collectiones
  recursively."
  [coll]
  ((fn transform [thing]
     (cond
      (string? thing) (symbol (capitalize thing)),
      (or (sequential? thing)
          (map? thing)
          (set? thing))
      (walk transform identity thing),
      :else thing))
   coll))

(defn- prepare-for-conexp
  "Returns the given hash-map with modifications to be a valid for DL interpretations."
  [hash-map]
  (reduce! (fn [map [k v]]
             (assoc! map k (set v)))
           {}
           (symbolify hash-map)))

(defn- role-map->concept-map [role-map]
  (assert (= 1 (count role-map)))
  (loop [concept-map {},
         is-as (get role-map (first (keys role-map)))]
    (if (empty? is-as)
      concept-map
      (let [[A B] (first is-as)]
        (recur (update-in concept-map [B] conj A)
               (rest is-as))))))

(defn- collect
  "Returns the smallest connected subrelation of relation containing start."
  [start relation]
  (let [start   (set start),
        related (set (for [[x y] relation,
                           :when (or (contains? start x)
                                     (contains? start y)),
                           z [x y]]
                       z))]
    (if (superset? start related)
      start
      (recur related relation))))

(defn smallest-subinterpretation
  "Returns the smallest subinterpretation of interpretation containing the given individuals."
  [interpretation individuals]
  (let [relation (reduce union
                         #{}
                         (map #(interpret interpretation %)
                              (role-names (interpretation-language interpretation)))),
        base-set (collect (set individuals) relation),

        name-int (map-by-fn #(intersection base-set (interpret interpretation %))
                            (concept-names (interpretation-language interpretation))),
        role-int (map-by-fn #(set-of [x y] | [x y] (interpret interpretation %)
                                             :when (and (contains? base-set x)
                                                        (contains? base-set y)))
                            (role-names (interpretation-language interpretation))),
        interpr  (let [interpretation (merge name-int role-int)]
                   (select-keys interpretation
                                (remove #(empty? (interpretation %))
                                        (keys interpretation)))),

        concs    (intersection (concept-names (interpretation-language interpretation))
                               (set (keys interpr)))
        roles    (intersection (role-names (interpretation-language interpretation))
                               (set (keys interpr)))]
    (make-interpretation (restrict-language (interpretation-language interpretation)
                                            concs
                                            roles)
                         base-set
                         interpr)))

;;; Exploration Utilities

(defn number-of-counterexamples
  "Returns for a interpretation and a gci the number of counterexamples,
  i.e. the cardinality of the extension of the concept (and A
  (not B)), where the gci is of the form A -> B."
  [interpretation A B]
  (count (interpret interpretation (list 'and A (list 'not B)))))

(defn concept-support
  "Returns the support of the given concept, i.e. the cardinality of
  its extension in interpretation."
  [interpretation A]
  (count (interpret interpretation A)))

;;;

(defn gci-support
  "Returns the support of the given GCI in the given interpretation."
  [gci interpretation]
  (if (empty? (interpretation-base-set interpretation))
    1
    (/ (count (interpret interpretation (subsumee gci)))
       (count (interpretation-base-set interpretation)))))

(defn gci-confidence
  "Returns the confidence of the giben GCI in the given interpretation."
  [gci interpretation]
  (let [c (count (interpret interpretation (subsumee gci)))]
    (if (zero? c)
      1
      (/ (count (interpret interpretation (dl-expression (interpretation-language interpretation)
                                                         (and (subsumee gci) (subsumer gci)))))
         c))))

(defn concept-size
  "Returns the size of an EL-gfp concept description."
  [dl-expression]
  (let [counter (fn counter [term]
                  (cond
                   (sequential? term) (reduce + (map counter term)),
                   (tbox? term) (reduce + 1 (map #(+ 1
                                                     (counter (definition-target %))
                                                     (counter (expression-term (definition-expression %))))
                                                 (tbox-definitions term))),
                   :else 1))]
    (counter (expression-term dl-expression))))

(defn dubiousness
  "Returns some kind of measure for the dubiousness of the gci A -> B
  in interpretation."
  [interpretation A B]
  (/ (concept-size B)
     (concept-size A)
     (+ 1 (concept-support interpretation A))))


;;; DBpedia Model

(defn- read-dbpedia-triples
  "Reads model from wikipedia entries. roles can be any quoted sequence of child, father, mother,
  influenced, influencedBy, relation, relative, spouse, partner, opponent, ..."
  [properties instances roles]
  (let [relations (read-rdf-lines-from-file properties
                                            (set-of (str "http://dbpedia.org/ontology/" role)
                                                    [role roles])
                                            (constantly true)
                                            (constantly true)),
        ins       (set (flatten (vals relations))),
        concepts  (role-map->concept-map
                   (read-rdf-lines-from-file instances
                                             (constantly true)
                                             #(contains? ins %)
                                             #(not (re-find #"owl#Thing" %)))),

        transform (fn transform [thing]
                    (if (string? thing)
                      (capitalize
                       (second (re-find #"http://dbpedia.org/[^/]*/(.*)" thing)))
                      (walk transform identity thing))),

        concepts  (transform concepts),
        relations (transform relations)]
    [(prepare-for-conexp concepts), (prepare-for-conexp relations)]))

(defn- role-support
  "From properties reads in all RDF triples and returns for every role occuring the number of times
  it occured."
  [properties]
  (with-in-reader properties
    (binding [*in* (clojure.lang.LineNumberingPushbackReader. *in*)]
      (loop [map {},
             line-count 0]
        (if-let [line (read-line)]
          (do
            ;; (when (zero? (mod line-count 10000))
            ;;   (println line-count))
            (let [[role _] (rdf-line-to-pair line)]
              (recur (assoc map role (inc (get map role 0)))
                     (inc line-count))))
          (do
;;            (println line-count)
            map))))))

(defn read-dbpedia-model
  "For the given set of roles (as symbols) returns the smallest model
  containing the interpretations of roles in the data-set of dbpedia."
  [properties instances roles]
  (let [[concepts, roles] (read-dbpedia-triples properties instances roles)]
    (hash-map->interpretation concepts roles :base-lang EL-gfp)))


;;; Drug Model

(defn- role-to-concept
  ([map role triples]
     (role-to-concept map role triples (fn [x y] y)))
  ([map role triples modifier]
     ;; (println role)
     ;; (println (count (get triples role)))
     (reduce (fn [map [A B]]
               (let [B (modifier role B)]
                 (assoc map B (conj (get map B) A))))
             map
             (get triples role))))

(defn read-drug-model [file]
  (let [triples  (read-rdf-lines-from-file file),
        roles    {"possibleDiseaseTarget"
                  (map (fn [pair]
                         [(pair 1) (pair 0)])
                       (get triples "http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/possibleDiseaseTarget")),
                  "treatedBy"
                  (get triples "http://www4.wiwiss.fu-berlin.de/diseasome/resource/diseasome/diseaseSubtypeOf"),
                  "targets"
                  (get triples "http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/target")}
        concepts (-> {}
                     (role-to-concept "http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/drugCategory"
                                      triples)
                     (role-to-concept "http://www4.wiwiss.fu-berlin.de/diseasome/resource/diseasome/class"
                                      triples)
                     (role-to-concept "http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/goClassificationProcess"
                                      triples
                                      (fn [r A]
                                        (let [process (or (first-non-nil (map #(re-find (re-pattern %) A)
                                                                              (list "transport"
                                                                                    "regulation"
                                                                                    "metabolism"
                                                                                    "signal transduction"
                                                                                    "biosynthesis"
                                                                                    "catabolism"
                                                                                    "homeostasis"
                                                                                    "assembly"
                                                                                    "symbiosis"
                                                                                    "DNA.* replication")))
                                                          A)]
                                          (str "Process: " process "."))))),
        roles    (prepare-for-conexp roles),
        concepts (prepare-for-conexp concepts)]
    (hash-map->interpretation concepts
                              roles
                              :base-lang EL-gfp)))

;;; Programming Language Model

(defn read-programming-language-model
  "Reads the programming language model from the given dbpedia data sets."
  [properties instances]
  (let [[influenced influenced-by] (pvalues (read-rdf-lines-from-file properties
                                                                      #{"http://dbpedia.org/ontology/influenced"}
                                                                      (constantly true)
                                                                      (constantly true))
                                            (read-rdf-lines-from-file properties
                                                                      #{"http://dbpedia.org/ontology/influencedBy"}
                                                                      (constantly true)
                                                                      (constantly true))),

        influence (set (concat (first (vals influenced))
                                (map (fn [[a b]] [b a])
                                     (first (vals influenced-by))))),

        individuals (set (flatten (seq influence)))

        concepts  (role-map->concept-map
                   (read-rdf-lines-from-file instances
                                             (constantly true)
                                             #(contains? individuals %)
                                             #(re-find #"ProgrammingLanguage" %))),

        individuals (set (flatten (vals concepts))),

        concepts  (role-map->concept-map
                   (read-rdf-lines-from-file instances
                                             (constantly true)
                                             #(contains? individuals %)
                                             #(not (re-find #"owl#Thing" %)))),

        influence (set-of [x y] | [x y] influence :when (and (contains? individuals x)
                                                             (contains? individuals y)))]

    (hash-map->interpretation (prepare-for-conexp concepts)
                              (prepare-for-conexp {"influenced" influence})
                              :base-lang EL-gfp)))

;;;

nil
