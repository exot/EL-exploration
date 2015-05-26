;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tests
  (:use conexp.main))

;;;

(tests-to-run tests.elex.dl.semantics
              tests.elex.dl.EL.description-graphs
              tests.elex.dl.EL
              tests.elex.dl.EL-gfp
              tests.elex.dl.reasoning
              tests.elex.dl.EL-gfp.rewriting
              tests.elex.dl.EL.exploration
              tests.elex.dl.EL-gfp.exploration
              tests.elex.dl.misc)

;;;

nil
