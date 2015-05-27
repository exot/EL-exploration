;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns elex.tests.dl.EL
  (:use conexp.main
        elex.dl.syntax
        elex.dl.boxes
        elex.dl.semantics
        elex.dl.reasoning
        elex.dl.EL.description-graphs
        elex.dl.EL)
  (:require [elex.tests.dl.examples :as x])
  (:use clojure.test))

;;;

(deftest test-subsumption
  (is (subsumed-by? (make-dl-expression x/SimpleDL '(and Mother Father))
                    (make-dl-expression x/SimpleDL 'Father)))
  (is (not (subsumed-by? (make-dl-expression x/SimpleDL 'Father)
                         (make-dl-expression x/SimpleDL '(and Mother Father))))))

(deftest test-EL-mmsc-with-role-depth-bound
  (is (equivalent? (EL-mmsc-with-role-depth-bound x/SimpleDL 2 x/paper-model '#{Linda Michelle})
                   (make-dl-expression x/SimpleDL '(and Mother Female (exists HasChild (and))))))
  (doseq [[language model] [[x/SimpleDL x/paper-model]
                            [x/SimpleDL x/some-model]
                            [x/FamilyDL x/family-model]]
          X (subsets (interpretation-base-set model))]
    (let [mmsc (memoize (fn [d]
                          (EL-mmsc-with-role-depth-bound language d model X)))]
      (doseq [d [0 1 2 3]]
        (is (subsumed-by? (mmsc (inc d)) (mmsc d)))
        (is (subset? X (interpret model (mmsc d))))))))

;;;

nil
