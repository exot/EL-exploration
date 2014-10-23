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
        dl.languages.description-graphs))

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

(defn EL-lcs
  "Returns the least common subsumer in EL of all concept descriptions in `concepts'."
  [language concepts]
  (if (empty? concepts)
   (make-dl-expression language '(bottom))
   (let [[product root]
         (reduce (fn [[product-tree product-root] concept-description]
                   (let [[tree root] (EL-concept-description->description-tree concept-description)]
                     [(graph-product product-tree tree [product-root root]),
                      [product-root root]]))
                 (EL-concept-description->description-tree (first concepts))
                 (rest concepts))]
     (description-tree->EL-concept-description product root))))

(defn EL-mmsc-with-role-depth-bound
  "Returns the model based most specific concept in EL of `objects' in `model' with given
  role-depth bound `d'."
  [d model objects]
  (let [interpretation-graph (interpretation->description-graph model)]
    (EL-lcs (interpretation-language model)
            (map (fn [x]
                   (apply description-tree->EL-concept-description
                          (prune-description-graph d interpretation-graph x)))
                 objects))))

;;;

nil
