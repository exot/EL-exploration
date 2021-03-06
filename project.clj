;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;;;

(defproject elex "1.0.0"
  :min-lein-version "1.3.0"
  :description "A prototypical implementation of Distel's EL Exploration Algorithm"
  :repositories {"math" "http://www.math.tu-dresden.de/~borch/repos/mvn/"}
  :dependencies [[conexp-clj/conexp-clj "0.0.7-alpha-SNAPSHOT"]]
  :keep-non-project-classes true
  :jvm-opts ["-server", "-Xmx4g", "-Xss1g"]
  :global-vars {*warn-on-reflection* false}
  ;:test-paths ["src/tests/"]
  :license "epl-v10"
  :url ""
  :test-paths ["tests"]
  )

;;;

nil
