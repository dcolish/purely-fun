Purely Fun!
-----------

This is a playground for me to experiment with Haskell and Ocaml while reading through
Chris Okasaki's book `"Purely Functional Data Structures"`_. I am also using `Markus
Mottl's work`_ as a reference to this endeavor. 

All code is translation from SML examples originally written by Chris Okasaki
(c) 1998. Parts of the Ocaml translation are thanks to Markus's Mottl
(c) 2009. All the rest is mine, (c) 2011. If you look at Markus's work it should
be clear where in the Ocaml translation I borrowed from his code; if there's
confusion I will happily clarify. (I really recommend reading his notes, he's
done an excellent job!)

.. _`Purely Functional Data Structures`: http://amzn.to/lTnskq
.. _`Markus Mottl's work`: http://hg.ocaml.info/release/pure-fun/summary


A Few Notes
-----------

I am working side by side in both Haskell and Ocaml to not only learn both
languages, but also to review the differences between strict and lazy evaluation
in languages. You'll probably note that the examples in Chapter 2 & 3 are not
defined to be lazy, but as they exist in the Haskell translation are in fact
lazy. The repercussions on the performance of these implementations due to the
existence of lazy evaluation by default is not yet known to me. I have yet to
decide if I should also produce strict Haskell versions of the code in these chapters.

I am making an effort to use the interface features of both languages in
accordance with Okasaki's approach in SML. While this style is very common in
Ocaml, I see much less use of custom type classes in most Haskell code.
