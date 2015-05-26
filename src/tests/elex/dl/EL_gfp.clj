;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tests.dl.EL-gfp
  (:use conexp.main
        dl.syntax
        dl.boxes
        dl.semantics
        dl.EL.description-graphs
        dl.EL-gfp
        tests.dl.examples)
  (:use clojure.test))

;;;

(deftest test-lcs
  (are [language model tbox targets] (let [lcs     (EL-gfp-lcs tbox 'targets),
                                           lcs-int (interpret model lcs)]
                                       (forall [target 'targets]
                                         (subset? (interpret model
                                                             (make-dl-expression-nc language
                                                                                    [tbox target]))
                                                  lcs-int)))
       SimpleDL some-model some-tbox [Grandfather]
       SimpleDL some-model some-tbox [Grandmother]
       SimpleDL some-model some-tbox [Grandmother Grandfather]
       SimpleDL paper-model some-tbox [Grandfather Grandmother]
       SimpleDL small-model some-tbox [Grandfather Grandmother]
       FamilyDL family-model parent [Partner Self]
       FamilyDL family-model parent [Partner Self Child]
       FamilyDL family-model parent [Self Self Self Self])
  (are [language model tbox target] (= (interpret model (EL-gfp-lcs tbox '[target]))
                                       (interpret model (make-dl-expression-nc language [tbox 'target])))
       SimpleDL some-model some-tbox Grandfather
       SimpleDL some-model some-tbox Grandmother
       SimpleDL some-model all-tbox All
       FamilyDL family-model parent Self))

(deftest test-msc
  (are [language testing-model objects] (subset? 'objects
                                                 (interpret testing-model
                                                            (EL-gfp-mmsc language testing-model 'objects)))
       SimpleDL some-model #{}
       SimpleDL some-model #{John}
       SimpleDL some-model #{John Marry}
       SimpleDL some-model #{John Marry Jana}
       SimpleDL some-model #{John Peter}
       SimpleDL some-model #{Jana Marry}
       FamilyDL family-model #{}
       FamilyDL family-model #{Paul Linda Mackenzie}
       FamilyDL family-model #{Linda}
       FamilyDL family-model #{Michelle}
       FamilyDL family-model #{Paul Linda James John Michelle Mackenzie}
       RidingDL riding-model #{}
       RidingDL riding-model #{RechtesVorderrad LinkesHinterrad}
       RidingDL riding-model #{MeinFahrrad})
  (are [language testing-model objects mmsc]
    (= (make-dl-expression language 'mmsc)
       (EL-gfp-mmsc language testing-model 'objects))
    ;;
    SimpleDL some-model #{} (bottom)
    SimpleDL paper-model #{} (bottom)
    SimpleDL paper-model #{James} Male
    SimpleDL paper-model #{John} (and Father Male (exists HasChild Female))
    FamilyDL family-model #{} (bottom)
    RidingDL riding-model #{} (bottom)))

;;;

nil
