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
                                        (bottom))
                           (subsumption Mother
                                        (and Female (exists HasChild (and))))
                           (subsumption Father
                                        (and (exists HasChild (and)) Male))
                           (subsumption (and (exists HasChild (and)) Female)
                                        Mother)
                           (subsumption (and (exists HasChild (and)) Male)
                                        Father)
                           (subsumption (and (exists HasChild (and Female))
                                             (exists HasChild (and Male)))
                                        (bottom))
                           (subsumption (and (exists HasChild (and (exists HasChild (and)))))
                                        (bottom)))),
       ;;
       small-model (with-dl SimpleDL
                     (list (subsumption Male Father)
                           (subsumption Mother
                                        (and (exists HasChild Female) Female))
                           (subsumption Father
                                        (and Male))
                           (subsumption (exists HasChild (and))
                                        (exists HasChild Female))
                           (subsumption (and (exists HasChild (and)) Female)
                                        Mother)
                           (subsumption (and Father Mother)
                                        (bottom))
                           (subsumption (exists HasChild (and (exists HasChild (and Female))))
                                        (bottom))))
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
