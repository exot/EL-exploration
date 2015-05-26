Axiomatizing Finite Interpretations in the Description Logic EL
===============================================================

This is a prototypical implementation of the axiomatization algorithm from Baader and
Distel.  The main reference is the PhD Thesis by Felix Distel, available from
[Qucosa](http://www.qucosa.de/recherche/frontdoor/cache.off?tx_slubopus4frontend%5Bid%5D=7019).

How to Use
----------

The main step in using this implementation is in representing your data as an
interpretation.  If the data itself is not very large, you can do it directly:

```clj
(use 'elex.dl.syntax
     'elex.dl.semantics)

(def some-model (interpretation '[Father Mother Male Female]
                                '[HasChild]
                                #{John Marry Peter Jana}
                                Mother #{Marry},
                                Father #{John, Peter},
                                Male   #{John, Peter},
                                Female #{Marry, Jana},
                                HasChild #{[John Peter], [Marry Peter], [Peter Jana]}))
```

However, your data may be much to large to do this (by hand.)  There is some limited
functionality for extracting interpretations from XML-serialized RDF Triples.  Suppose you
have some triples from DBpedia and you want to extract an interpretation that represents
the `child`-relation in this data set.  Suppose that wikiprops.nt contains triples
representing properties, and that wikiinstances.nt contains triples representing the
actual `child`-relation (among others.)  Then you can do

```clj
(use 'elex.util.read-rdf-models)

(def dbpedia-model
  (read-dbpedia-model "wikiprops.nt" "wikiinstances.nt" '[child]))
```

Note however, that this functionality is specific for the data sets from DBpedia.  See
[read_rdf_models.clj](https://github.com/exot/EL-exploration/blob/master/src/elex/util/read_rdf_models.clj)
for more examples on this.

As soon as you have your interpretation, you can get a base of it like this

```clj
(use 'elex.dl.EL-gfp.exploration)

(model-gcis some-model)
(model-gcis dbpedia-model)
```

If you are only interested in bases withing a maximal quantifier depth, you can also do the following

```clj
(use 'elex.dl.EL.exploration)

(model-gcis d some-model)
(model-gcis d dbpedia-model)
```

where `d` is an integer denoting the maximal quantifier depth.

License
-------

Copyright ⓒ Daniel Borchmann

The use and distribution terms for this software are covered by the Eclipse Public License
1.0 (http://opensource.org/licenses/eclipse-1.0.php).  By using this software in any
fashion, you are agreeing to be bound by the terms of this license.  You must not remove
this notice, or any other, from this software.
