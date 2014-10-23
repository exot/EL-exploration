;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tests.dl.framework.semantics
  (:use conexp.main
        dl.framework.syntax
        dl.framework.boxes
        dl.framework.semantics
        tests.dl.examples)
  (:use clojure.test))

;;;

(def ^:private b-tbox (tbox FamilyDL, B [parent Self]))
(def ^:private a-tbox (tbox FamilyDL, A [b-tbox B]))

(deftest test-interpret
  (are [language expected testing-model expr] (= 'expected
                                                 (interpret testing-model
                                                            (dl-expression language expr)))
       SimpleDL #{} some-model all-cpt,
       SimpleDL #{John} some-model [some-tbox Grandfather],
       SimpleDL #{Marry} some-model [some-tbox Grandmother],
       FamilyDL #{John Linda Michelle Paul} family-model [parent Self],
       FamilyDL #{John Linda Michelle Paul} family-model [parent Partner]
       SimpleDL #{Marry} some-model Mother,
       SimpleDL #{John Peter} some-model Father,
       SimpleDL #{Jana Marry} some-model Female,
       SimpleDL #{Marry} some-model (nominal Marry),
       SimpleDL #{Marry John} some-model (nominal Marry John),
       SimpleDL #{Marry} some-model (nominal Marry Fred),
       SimpleDL #{} some-model (bottom),
       SimpleDL #{John Marry Peter Jana} some-model (top),
       FamilyDL #{John Linda Michelle Paul} family-model [a-tbox A]
       SimpleDL #{John Marry} some-model dl-exp
       SimpleDL #{John} some-model ext-dl-exp
       SimpleDL #{John} some-model ext-dl-exp-2
       SimpleDL #{John Mackenzie Michelle} small-model [(tbox SimpleDL
                                                              A A),
                                                        A]
       SimpleDL #{John Mackenzie Michelle} small-model [(tbox SimpleDL
                                                              A B,
                                                              B C,
                                                              C D,
                                                              D E,
                                                              E A),
                                                        A]))

(deftest test-gfp-lfp-model
  (are [mymodel mytbox] (let [gfp (gfp-model mytbox mymodel)]
                          (forall [def (tbox-definitions mytbox)]
                            (= (interpret gfp (make-dl-expression (tbox-language mytbox)
                                                                  (definition-target def)))
                            (interpret gfp (definition-expression def)))))
     small-model (tbox SimpleDL A A),
     small-model (tbox SimpleDL A A, B B, C C)
     small-model (tbox SimpleDL A B, B C, C D, D E, E A)))

;;;

nil
