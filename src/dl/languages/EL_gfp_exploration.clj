;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns dl.languages.EL-gfp-exploration
  (:use conexp.main
        dl.framework.syntax
        dl.framework.semantics
        dl.framework.reasoning
        dl.languages.interaction
        dl.languages.description-graphs
        dl.languages.EL-gfp
        dl.languages.EL-gfp-rewriting
        dl.util.concept-sets))

(ns-doc
 "Implements exploration for description logics EL and EL-gfp.")


;;; technical helpers

(defn- induced-context
  "Returns context induced by the set of concept descriptions and the
  given model."
  ([descriptions model]
     (induced-context descriptions model (make-context #{} #{} #{})))
  ([descriptions model old-context]
     (let [new-objects    (difference (interpretation-base-set model)
                                      (objects old-context)),
           new-attributes (difference (set descriptions)
                                      (attributes old-context)),
           new-incidence  (union (set-of [x y] [y new-attributes,
                                                x (interpret model y)])
                                 (if (empty? new-objects)
                                   (incidence old-context)
                                   (set-of [x y] [y (attributes old-context),
                                                  x (interpret model y)])))]
       (make-context (union (objects old-context) new-objects)
                     (union (attributes old-context) new-attributes)
                     new-incidence))))

(defn- obviously-true?
  "Returns true iff the given subsumption is obviously true."
  [subsumption]
  (subsumed-by? (subsumee subsumption) (subsumer subsumption)))

;;; actual exploration algorithm

(defn explore-model
  "Model exploration algorithm."
  ([initial-model]
     (explore-model initial-model (concept-names (interpretation-language initial-model))))
  ([initial-model initial-ordering]
     (with-memoized-fns [EL-expression->rooted-description-graph,
                         interpret,
                         model-closure,
                         subsumed-by?,
                         interpretation->tbox]
       (let [language (interpretation-language initial-model)]

         (when (and (not= (set initial-ordering) (concept-names language))
                    (not= (count initial-ordering) (count (concept-names language))))
           (illegal-argument "Given initial-ordering for explore-model must consist "
                             "of all concept names of the language of the given model."))

         (loop [M_k   (make-concept-set (map #(dl-expression language %) ;the set of constructed concepts
                                             initial-ordering)),
                Pi_k  [],               ;the sequence of pseudo-intents found
                P-map {},               ;a map mapping pseudo-intents P to [all-P, all-P-closure]
                P_k   #{},              ;the current pseudo-intent
                model initial-model,    ;working model
                implications #{},
                background-knowledge #{}]

           (if P_k
             ;; then search for next implication
             (let [all-P_k      (make-dl-expression language (cons 'and P_k)),
                   next-model   (loop [model model]
                                  (let [susu (abbreviate-subsumption (make-subsumption all-P_k
                                                                                       (model-closure model all-P_k))
                                                                     (union implications background-knowledge))]
                                    (if (or (obviously-true? susu)
                                            (not (expert-refuses? susu)))
                                      model
                                      (recur (extend-model-by-contradiction model susu))))),
                   all-P_k-closure
                                (model-closure next-model all-P_k),

                   new-concepts (when (forall [[_ Q-closure] (vals P-map)]
                                        (not (equivalent? all-P_k-closure Q-closure))),
                                  (for [r (role-names language)]
                                    (dl-expression language (exists r all-P_k-closure)))),
                   next-M_k     (apply add-concepts! M_k new-concepts),

                   P-map        (assoc P-map
                                  P_k [all-P_k all-P_k-closure]),
                   next-Pi_k    (conj Pi_k P_k),

                   implications (let [new-impl (make-implication P_k
                                                                 (set-of D | D (seq M_k)
                                                                             :when (subsumed-by? all-P_k-closure D))),
                                      impls    (if-not new-concepts
                                                 implications
                                                 (set-of impl | old-impl implications
                                                                :let [P (premise old-impl),
                                                                      Q (conclusion old-impl),
                                                                      P-closure (second (get P-map P)),
                                                                      impl (make-implication
                                                                            P
                                                                            (into Q
                                                                                  (set-of D | D new-concepts
                                                                                              :when (subsumed-by? P-closure D))))]
                                                                :when (not-empty (conclusion impl))))]
                                  (if (not-empty (conclusion new-impl))
                                    (conj impls new-impl)
                                    impls)),

                   background-knowledge
                                (minimal-implication-set next-M_k),

                   next-P_k     (next-closed-set (seq next-M_k)
                                                 (clop-by-implications (union implications background-knowledge))
                                                 P_k)]
               (recur next-M_k next-Pi_k P-map next-P_k next-model implications background-knowledge))

             ;; else return set of implications
             (let [implicational-knowledge (union implications background-knowledge)]
               (doall ;ensure that this sequence is evaluated with our bindings in effect
                (for [P Pi_k
                      :let [[all-P all-P-closure] (get P-map P)]
                      :when (not (subsumed-by? all-P all-P-closure))
                      :let [susu (abbreviate-subsumption (make-subsumption all-P all-P-closure)
                                                         implicational-knowledge)]
                      :when (not-empty (arguments (subsumer susu)))]
                  susu)))))))))

;;; gcis

(defn model-gcis
  "Returns a complete and sound set of gcis holding in model. See
  explore-model for valid args."
  [model & args]
  (binding [expert-refuses? (constantly false)]
    (apply explore-model model args)))

(defn model-gcis-naive
  "Naive implementation of model-gcis."
  [model]
  (let [language    (interpretation-language model),
        M_i (concat (map #(make-dl-expression language %)
                         (concept-names language))
                    (mapcat (fn [objs]
                              (let [msc (expression-term (most-specific-concept model objs))]
                                (map #(make-dl-expression language (list 'exists % msc))
                                     (role-names language))))
                            (all-closed-sets (interpretation-base-set model)
                                             #(interpret model (most-specific-concept model %))))),
        K   (induced-context M_i model),
        S   (minimal-implication-set (make-concept-set M_i)),
        sb  (stem-base K S),
        su  (set-of (make-subsumption pre clc)
                    [impl sb
                     :let [pre (make-dl-expression language (cons 'and (premise impl))),
                           clc (make-dl-expression language (cons 'and (conclusion impl)))]
                     :when (not (subsumed-by? pre clc))])]
    su))

;;;

nil
