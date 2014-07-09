Concurrent_Datastructures
=========================

 Concurrent Datastructures for Haskell - Gsoc 2014

My proposal is to implement the following ticket: https://ghc.haskell.org/trac/summer-of-code/ticket/1617 The purpose is to have highly performant, library quality concurrent datastructures for haskell. These are to be implemented without locking using atomic operations. Specifically I want to implement an Hash Map. Probably: Lock free hash Table by Cliff Click


The Objective of my proposal is to have concurrent datastructures (hash maps, also queues, bags stacks, counters, some of witch are already implemented, I'll be only working on hash-maps ); as are already described in literature, or implemented in other languages. They will be implemented lock free, using the cas (compare and swap) atomic operation. This will achieve good performance with more accessing threads and under higher loads, than would be possible using datastructures employing locks. Basically what is described in this article: Shavit:Data Structures in the Multicore Age http://people.csail.mit.edu/shanir/publications/p76-shavit.pdf

https://ghc.haskell.org/trac/summer-of-code/ticket/1617
