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

(tests-to-run tests.dl.framework.semantics
              tests.dl.languages.description-graphs
              tests.dl.languages.EL-gfp
              tests.dl.framework.reasoning
              tests.dl.util.concept-sets
              tests.dl.languages.EL-gfp-rewriting
              tests.dl.languages.EL-gfp-exploration)

;;;

nil
