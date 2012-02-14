;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;;;

(defproject EL-exploration (.trim #=(slurp "VERSION"))
  :min-lein-version "1.6.1.1"
  :description "A prototypical implementation of Distel's EL Exploration Algorithm"
  :url "http://www.math.tu-dresden.de/~borch/conexp-clj/"
  :dependencies [[conexp-clj "0.0.7-alpha-SNAPSHOT"]]
  :keep-non-project-classes true
  :jvm-opts ["-server", "-Xmx1g"]
  :warn-on-reflection true)

;;;

nil
