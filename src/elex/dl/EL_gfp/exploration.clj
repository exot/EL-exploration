;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns elex.dl.EL-gfp.exploration
  "Implements exploration for description logics EL and EL-gfp."
  (:use conexp.main
        elex.dl.syntax
        elex.dl.semantics
        elex.dl.reasoning
        elex.dl.EL.description-graphs
        elex.dl.EL-gfp
        elex.dl.EL-gfp.rewriting))

;;; technical helpers

(defn induced-context
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

(defn essential-concept-descriptions
  "Returns the list of all \"essential concept descriptions\" (the set M_{\\mathcal{I}}) of
  the given interpretation."
  [language interpretation]
  (with-memoized-fns [EL-expression->rooted-description-graph,
                      EL-gfp-mmsc,
                      interpretation->tbox,
                      interpretation->description-graph]
    (let [i    interpretation,
          M_I  (concat (list (make-dl-expression language '(bottom)))
                       (map #(make-dl-expression language %)
                            (concept-names language))
                       (mapcat (fn [objs]
                                 (let [msc (expression-term (EL-gfp-mmsc language i objs))]
                                   (for [r (role-names language)]
                                     (make-dl-expression language (list 'exists r msc)))))
                               (drop-while empty?
                                           (all-closed-sets (seq (interpretation-base-set i))
                                                            #(interpret i (EL-gfp-mmsc language i %))))))]
      (doall M_I))))

;;; actual exploration algorithm

(defn model-gcis
  "Returns base of given interpretation."
  ;;
  ([model]
     (model-gcis model (interpretation-concept-names model)))
  ;;
  ([model initial-ordering]
     (with-memoized-fns [EL-expression->rooted-description-graph,
                         subsumed-by?,
                         interpretation->tbox,
                         interpretation->description-graph]
       (let [language      (make-dl (gensym "DL-")
                                    (interpretation-concept-names model)
                                    (interpretation-role-names model)
                                    '[]
                                    :extends EL-gfp),
             model-closure (memoize (fn [concept-description]
                                      (EL-gfp-mmsc language model (interpret model concept-description)))),
             interpret     (memoize (fn [C]
                                      (interpret model C))),
             bigsqcap      (memoize (fn [P]
                                      (make-dl-expression language (conjunction P))))]

         (when (and (not= (set initial-ordering) (concept-names language))
                    (not= (count initial-ordering) (count (concept-names language))))
           (illegal-argument "Given initial-ordering for model-gcis must consist "
                             "of all concept names of the language of the given model."))

         (loop [ ;;the set of constructed concepts
                M                     (map #(dl-expression language %)
                                           (conj initial-ordering '(bottom))),
                ;; the sequence of concept-descriptions defined by pseudo-intents found
                pseudo-descriptions   [],
                ;; the current pseudo-intent
                P                     #{},
                ;; found implications
                implications          #{},
                ;; accumulated background knowledge
                background-knowledge  #{}]

           (if (not= P (set M))
             ;; then search for next implication
             (let [all-P                (bigsqcap P),

                   all-P-closure        (model-closure all-P),

                   new-concepts         (when (and (not= (expression-term all-P-closure) '(bottom))
                                                   (forall [Q pseudo-descriptions]
                                                     (not (equivalent? all-P-closure (model-closure Q)))))
                                          (for [r (role-names language)]
                                            (dl-expression language (exists r all-P-closure)))),

                   next-M               (concat new-concepts M),

                   pseudo-descriptions  (conj pseudo-descriptions all-P),

                   ;; amend implications
                   implications         (if-not new-concepts
                                          implications
                                          (set-of (make-implication
                                                   X
                                                   (into Y (filter (fn [D]
                                                                     (subset? (interpret (bigsqcap X))
                                                                              (interpret D)))
                                                                   new-concepts)))
                                                  | old-impl implications
                                                  :let [X (premise old-impl),
                                                        Y (conclusion old-impl)])),

                   implications         (conj implications
                                              (make-implication
                                               P
                                               (set-of D | D next-M,
                                                       :when (subset? (interpret (bigsqcap P))
                                                                      (interpret D)))))

                   background-knowledge (union background-knowledge
                                               (set-of (make-implication #{C} #{D})
                                                       | C M, D new-concepts
                                                       :when (subsumed-by? C D))
                                               (set-of (make-implication #{C} #{D})
                                                       | C new-concepts, D M
                                                       :when (subsumed-by? C D))),

                   ;; compute next pseudo-intent
                   next-P               (next-closed-set next-M
                                                         (clop-by-implications
                                                          (union implications background-knowledge))
                                                         P)]
               ;; redo!
               (recur next-M pseudo-descriptions next-P implications background-knowledge))

             ;; else return set of implications
             (let [implicational-knowledge (union implications background-knowledge)]
               (doall  ;ensure that this sequence is evaluated with our bindings in effect
                (for [all-P pseudo-descriptions,
                      :let [all-P-closure (model-closure all-P)]
                      :when (not (subsumed-by? all-P all-P-closure))]
                  (abbreviate-subsumption (make-subsumption all-P all-P-closure)
                                          implicational-knowledge))))))))))

(defn model-gcis-naive
  "Naive implementation of model-gcis."
  [model]
  (with-memoized-fns [EL-expression->rooted-description-graph,
                      interpretation->tbox]

    (let [language (make-dl (gensym "DL-")
                            (interpretation-concept-names model)
                            (interpretation-role-names model)
                            '[]
                            :extends EL-gfp),
          M_i      (essential-concept-descriptions language model),
          K        (induced-context M_i model),
          S        (canonical-base-from-base
                    (set-of (impl C ==> D) | C M_i, D M_i, :when (subsumed-by? C D)))
          sb       (canonical-base K S),
          su       (set-of (make-subsumption pre clc)
                           [impl sb
                            :let [pre (make-dl-expression language (conjunction (premise impl))),
                                  clc (EL-gfp-mmsc language model (interpret model pre))]
                            :when (not (subsumed-by? pre clc))])]
      su)))

;;;

nil
