Programming Assingment 4 
-- Nick Skelsey
-- Michael Paris

--The revenge of the typ--
    The first and most important design decision we made for this assignment was that we chose to do it in ocaml. This led us to use (and subsequently abuse) ocaml's disjoint unions. Our entire abstract syntax tree was stored using this data structure. We recursively built our parse tree from and generally had a good time. 

    Challenges emerged after having written our tree without including space for annotations. This turned out to be quite a terrible decision since our tree and everything that matched on its nodes needed to be updated after the update. Regardless the rebellion soldiered and our design evolved from there. The construction of the class map, implementation map and parent map proceeded as you might imagine.

    But if you can't imagine, the class map and implementation map were built after establishing a valid topological ordering of the class hierarchy. We simply extracted the nessecary methods and attributes from each class node and added the inherited features as went down the ordering. All in all it worked out well and was concise. The parent map was just built from that topological ordering.

    Using these three heroic data structures we were ready to type check expressions while simulataneously building our annotated tree. Using a set of mutually recursive functions we tried our best to mimic the structure and form of the stated type rules in cool manual. What followed was a deep dive into the murky waters of the human soul and cool's SELF_TYPE. And because we had decided earlier to store type annotations as strings much hackery was needed. 
    
    In principle you do not need an ocaml user defined type to store information about cool types. This was the banner upon which we made our charge and after wavering mere hours before the deadline we stuck with it. After putting grandiose visions of programming aside, our type checker was able to pass many more testcases and in the end was over 1500 lines of ocaml. 

    The good testcase we included was the original cool program we had to write for pa1. In order to get a handle on the language I tried to use most of its features and as a result we were very happy when our type checker could process this file. Our bad cases were small examples we created for pa4t. The first one deals with variable scoping within expressions. The second example breaks the type checker with an incorrect return type on static dispatch. The third negative testcase has a little esoteric bug in case statements. It is merely the use of the same type twice in two branches.
