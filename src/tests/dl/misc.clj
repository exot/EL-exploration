;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tests.dl.misc
  "Defines additional test cases, which do not fit into the strict linear setup of the
  basic test cases."
  (:use conexp.main
        dl.framework.syntax
        dl.framework.boxes
        dl.framework.semantics
        tests.dl.examples)
  (:use clojure.test))

;;;

(deftest test-print-dup
  (are [dl] (= dl (find-dl (language-name dl)))
       SimpleDL
       FamilyDL))

;;;

nil
