;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns dl.languages.EL-gfp
  "Definitions for the description logic EL-gfp."
  (:use conexp.main
        dl.framework.syntax
        dl.framework.semantics
        dl.framework.boxes
        dl.framework.reasoning
        dl.languages.description-graphs
        dl.languages.EL-gfp-rewriting))

;;; EL-gfp

(define-dl EL-gfp [] [] [exists and bottom])

(defn EL-gfp-model-interpretation
  "For a given tbox-target-pair returns the interpretation of the
  target in the gfp-model of tbox in model."
  [interpretation [tbox target]]
  (let [tbox-graph  (tbox->description-graph tbox),
        inter-graph (interpretation->description-graph interpretation)]
    ((efficient-simulator-sets tbox-graph inter-graph) target)))

(define-base-semantics EL-gfp
  [interpretation dl-expression]
  ;; quit, if dl-expression is not a tbox with target
  (when (not (tbox-target-pair? dl-expression))
    (illegal-argument "No base semantics defined for " (print-str dl-expression) "."))
  ;; compute gfp-model and interpret target
  (let [[tbox, target] (expression-term dl-expression)]
    (EL-gfp-model-interpretation interpretation [tbox target])))


;;; subsumption

(defn ensure-EL-gfp-concept
  "Ensures dl-expression to be a pair of a tbox and a target."
  [dl-expression]
  (let [expr (expression-term dl-expression)]
    (if (and (vector? expr)
             (= 2 (count expr)))
      dl-expression
      (let [language (expression-language dl-expression),
            target   (gensym)]
        (make-dl-expression-nc language
                               [(make-tbox language
                                           {target (make-dl-definition target dl-expression)}),
                                target])))))

(defn EL-expression->rooted-description-graph
  "Returns for a given EL expression or a tbox-target-pair its
  description graph together with the root corresponding to the
  target."
  [dl-expr]
  (let [[tbox target] (expression-term (ensure-EL-gfp-concept dl-expr))]
    [(tbox->description-graph tbox), target]))

(define-subsumption EL-gfp
  [C D]
  (cond (= '(bottom) (expression-term C))
        true
        ;;
        (= '(bottom) (expression-term D))
        false
        ;;
        true
        (let [[G-C C-target] (EL-expression->rooted-description-graph C)
              [G-D D-target] (EL-expression->rooted-description-graph D)]
          (simulates? G-D G-C D-target C-target))))

;;; lcs and msc

(defn EL-gfp-lcs
  "Returns the least common subsumer (in EL-gfp) of A and B in tbox."
  [tbox concepts]
  (let [language (tbox-language tbox)]
    (when (empty? concepts)
      (illegal-argument "EL-gfp-lcs called with no concepts."))
    (loop [new-tbox tbox,
           concepts (sort-by #(let [dl-expr (definition-expression (find-definition tbox %))]
                                (or (and (atomic? dl-expr) 1)
                                    (count (filter compound? (arguments dl-expr)))))
                             concepts)]
      (if (= 1 (count concepts))
        (make-dl-expression-nc language [new-tbox (first concepts)])
        (let [A          (first concepts),
              B          (second concepts),
              [tbox-A A] (clarify-ttp (tidy-up-ttp (clarify-ttp [new-tbox A]))),
              [tbox-B B] (clarify-ttp (tidy-up-ttp (clarify-ttp [tbox B]))),
              G_T_A      (tbox->description-graph tbox-A),
              G_T_B      (tbox->description-graph tbox-B),
              G-x-G      (graph-product G_T_A G_T_B [A,B]),
              [T target] (uniquify-ttp [(description-graph->tbox G-x-G language) [A,B]])]
          (if (= #{}
                 (set ((vertex-labels G-x-G) [A,B]))
                 (set ((neighbours G-x-G) [A,B])))
            (make-dl-expression-nc language [T target])
            (recur T (conj (nthnext concepts 2) target))))))))

(defn EL-gfp-mmsc
  "Returns the model based most specific concept of objects in model."
  [language model objects]
  (let [mmsc (if (not-empty objects)
               (let [tbox (interpretation->tbox model language)]
                 (EL-gfp-lcs tbox objects))
               (make-dl-expression language '(bottom)))]
    (if (= '(bottom) (expression-term mmsc))
      mmsc
      (let [[tbox target] (-> mmsc
                              expression-term
                              clarify-ttp
                              tidy-up-ttp
                              reduce-ttp
                              normalize-EL-gfp-term)]
        (if (acyclic? tbox)
          (definition-expression (first (tbox-definitions tbox)))
          (make-dl-expression-nc language [tbox target]))))))

;;;

nil
