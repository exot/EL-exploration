;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tests.dl.languages.EL-gfp-exploration
  (:use conexp.main
        dl.framework.syntax
        dl.framework.boxes
        dl.framework.semantics
        dl.framework.reasoning
        dl.languages.EL-gfp-exploration
        tests.dl.examples)
  (:use clojure.test))

;;;

(defn equivalent-gcis? [gci-1 gci-2]
  (and (equivalent? (subsumee gci-1) (subsumee gci-2))
       (equivalent? (subsumer gci-1) (subsumer gci-2))))

(deftest- model-gcis-returns-correct-result
  (are [model result]
       (let [cnts (filter #(not (exists [gci result] (equivalent-gcis? % gci)))
                          (model-gcis model))]
         (when-not (empty? cnts)
           (println cnts))
         (empty? cnts))
       ;;
       paper-model (with-dl SimpleDL
                     (list (subsumption (and Female Male)
                                        (and [(tbox All (and Father Mother (exists HasChild All))),
                                              All]))
                           (subsumption (and Father)
                                        (and (exists HasChild (and)) Male))
                           (subsumption (and Mother)
                                        (and Female (exists HasChild (and))))
                           (subsumption (and (exists HasChild (and)) Male)
                                        (and Father))
                           (subsumption (and Female (exists HasChild (and)))
                                        (and Mother))
                           (subsumption (and (exists HasChild (and Female))
                                             (exists HasChild (and Male)))
                                        (and [(tbox All (and Father Mother (exists HasChild All))),
                                              All]))
                           (subsumption (and (exists HasChild (and (exists HasChild (and)))))
                                        (and [(tbox All (and Father Mother (exists HasChild All))),
                                              All])))),
       ;;
       small-model (with-dl SimpleDL
                     (list (subsumption (and Mother)
                                        (and Female (exists HasChild (and Female))))
                           (subsumption (and Male)
                                        (and Father))
                           (subsumption (and Father)
                                        (and Male))
                           (subsumption (and (exists HasChild (and)))
                                        (and (exists HasChild (and Female))))
                           (subsumption (and (exists HasChild (and Female)) Female)
                                        (and Mother))
                           (subsumption (and Father Mother)
                                        (and [(tbox All (and Father Mother (exists HasChild All))),
                                              All]))
                           (subsumption (and (exists HasChild (and (exists HasChild (and Female)))))
                                        (and [(tbox All (and Father Mother (exists HasChild All))),
                                              All]))))
       ;;
       ))

(deftest- model-gcis-returns-correct-count
  (are [model gci-count] (let [gcis (model-gcis model)]
                           (and (= gci-count (count gcis))
                                (forall [gci gcis]
                                  (holds-in-interpretation? model gci))))
       some-model  9,
       riding-model 7,
       family-model 19,
       more-family-model 21,
       grandparent-model 32))

(defn test-ns-hook []
  (model-gcis-returns-correct-result)
  (model-gcis-returns-correct-count))

;;;

nil
