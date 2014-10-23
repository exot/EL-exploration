;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tests.dl.languages.description-graphs
  (:use conexp.main
        dl.framework.syntax
        dl.framework.boxes
        dl.framework.semantics
        dl.languages.description-graphs
        tests.dl.examples)
  (:use clojure.test))

;;;

(def paper-tbox (tbox FamilyDL
                      A-1 (and Male A-2 (exists HasChild (exists MarriedTo A-3)))
                      A-2 (and Female A-3 (exists MarriedTo (exists HasChild A-1)))
                      A-3 (and Mother A-2 (exists HasChild (and Male Female)))))

(defn- normalized?
  "Tests a TBox on being normalized."
  [tbox]
  (forall [def (tbox-definitions tbox)]
    (forall [term (let [definition (definition-expression def)]
                    (if (and (compound? definition)
                             (= 'and (operator definition)))
                      (arguments definition)
                      (list definition)))]
      (or (primitive? term)
          (and (compound? term)
               (= 'exists (operator term))
               (let [operand (second (arguments term))]
                 (and (not (primitive? operand))
                      (not (tbox-target-pair? operand))
                      (atomic? operand))))))))

(deftest test-normalize-gfp
  (are [testing-tbox] (normalized? (normalize-gfp testing-tbox))
       parent
       some-normal-tbox
       some-tbox
       paper-tbox
       all-tbox)
  (are [norm-count testing-tbox] (= norm-count (count (tbox-definitions (normalize-gfp testing-tbox))))
       3 parent
       3 some-normal-tbox
       6 some-tbox
       6 paper-tbox
       1 all-tbox)
  (are [language model testing-tbox target] (= (interpret model
                                                          (make-dl-expression language
                                                                              [testing-tbox target]))
                                               (interpret model
                                                          (make-dl-expression language
                                                                              [(normalize-gfp testing-tbox)
                                                                               target])))
       SimpleDL some-model some-tbox 'Grandfather
       SimpleDL some-model some-tbox 'Grandmother
       SimpleDL some-model some-normal-tbox 'A
       SimpleDL some-model some-normal-tbox 'B
       SimpleDL some-model some-normal-tbox 'T
       SimpleDL some-model all-tbox 'All
       FamilyDL family-model parent 'Self
       FamilyDL family-model parent 'Partner
       FamilyDL family-model parent 'Child
       FamilyDL family-model paper-tbox 'A-1
       FamilyDL family-model paper-tbox 'A-2
       FamilyDL family-model paper-tbox 'A-3))

(deftest test-simulator-sets
  (are [model] (let [graph (interpretation->description-graph model)]
                 (= (schematic-simulator-sets graph graph)
                    (efficient-simulator-sets graph graph)))
    some-model
    small-model
    family-model
    grandparent-model)
  (are [tbox] (let [graph (tbox->description-graph tbox)]
                (= (schematic-simulator-sets graph graph)
                   (efficient-simulator-sets graph graph)))
    some-tbox
    paper-tbox
    all-tbox
    some-normal-tbox
    parent))

(deftest test-EL-concept-description<->description-tree
  (let [simple-dl       (make-dl "SimpleDL" '[A B C] '[R S] '[and exists bottom])
        sample-dl-graph (make-description-graph '#{a b c}
                                                '{a [[R b], [S c]]}
                                                '{a [A B],
                                                  b [A C],
                                                  c [B]})]
    (is (= (expression-term (description-tree->EL-concept-description simple-dl sample-dl-graph 'a))
           '(and A B (exists R (and A C)) (exists S B))))
    (is (= (expression-term (description-tree->EL-concept-description simple-dl sample-dl-graph 'c))
           'B))
    (is (= (expression-term (description-tree->EL-concept-description simple-dl sample-dl-graph 'b))
           '(and A C)))
    (doseq [x '[a b c]]
      (is (= (apply description-tree->EL-concept-description
                    simple-dl
                    (EL-concept-description->description-tree
                     (description-tree->EL-concept-description simple-dl sample-dl-graph x)))
             (description-tree->EL-concept-description simple-dl sample-dl-graph x))))))

;;;

nil
