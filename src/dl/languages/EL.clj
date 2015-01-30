;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns dl.languages.EL
  "Definitions for the description logic EL-gfp."
  (:use conexp.main
        dl.syntax
        dl.semantics
        dl.boxes
        dl.reasoning
        dl.languages.description-graphs)
  (:require [clojure.core.reducers :as r]))

;;; EL

(define-dl EL [] [] [exists and bottom])

;;; subsumption

(define-subsumption EL
  [C D]
  (cond (= '(bottom) (expression-term C))
        true
        ;;
        (= '(bottom) (expression-term D))
        false
        ;;
        true
        (let [[G-C C-target] (EL-concept-description->description-tree C)
              [G-D D-target] (EL-concept-description->description-tree D)]
          (simulates? G-D G-C D-target C-target))))

;;; lcs and mmsc

(defn- cleanup-rooted-description-graph [language graph root]
  (let [tbox          (description-graph->tbox graph language)
        [tbox target] (-> [tbox root]
                          uniquify-ttp
                          clarify-ttp
                          tidy-up-ttp
                          reduce-ttp)
        graph         (tbox->description-graph tbox)]
    [graph target]))

(def ^:dynamic *lcs-folding* "Parameter to control folding in EL-lcs"
  32)

(defn EL-lcs
  "Returns the least common subsumer in EL of all concept descriptions in `concepts'."
  [language concepts]
  (if (empty? concepts)
    (make-dl-expression language '(bottom))
    (let [ ;; reducer function for r/fold
          reducef       (fn
                          ;; neutral tree
                          ([] [(make-description-graph [1]
                                                       {1 (set-of [r 1] | r (role-names language))}
                                                       {1 (concept-names language)})
                               1])
                          ;; cleanup and product of intermediate results
                          ([[tree-1 root-1] [tree-2 root-2]]
                           (let [product-tree (graph-product tree-1 tree-2 [root-1 root-2])]
                             (cleanup-rooted-description-graph language product-tree [root-1 root-2])))),
          [product root] (r/fold *lcs-folding*
                                 reducef
                                 reducef
                                 (r/map EL-concept-description->description-tree (vec concepts)))]
      (description-tree->EL-concept-description language product root))))

(defn EL-mmsc-with-role-depth-bound
  "Returns the model based most specific concept in EL of `objects' in `model' with given
  role-depth bound `d'."
  [language d model objects]
  (let [interpretation-graph (interpretation->description-graph model)]
    (EL-lcs language
            (pmap (fn [x]
                    (apply description-tree->EL-concept-description
                           language
                           (prune-description-graph d interpretation-graph x)))
                  objects))))

;;;

nil
