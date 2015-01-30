;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tests.dl
  (:use conexp.main))

;;;

(tests-to-run tests.dl.semantics
              tests.dl.EL.description-graphs
              tests.dl.EL
              tests.dl.EL-gfp
              tests.dl.reasoning
              tests.dl.EL-gfp.rewriting
              tests.dl.EL.exploration
              tests.dl.EL-gfp.exploration
              tests.dl.misc)

;;;

nil
