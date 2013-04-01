A whole lotta weasels
======

A joint effort to write the [Dawkins' Weasel algorithm][wikipedia] in as many languages as possible, using the language's specific idioms.

Algorithm described [here][algorithm].

+ Each implementation should only be one file and placed in the root folder.
+ Each file should be named by the language in which it was written.
+ The program should print the fittest string of each generation, as well as the number of generations it took.
+ Mutation rate should be 4%, generation size 100.


Benchmarking (using testharness) for fun and profit
------

These tests are as unscientific as it gets and can't be relied on for anything.

+ Node: 62 ms
+ Java: 238 ms
+ Python: 179 ms
+ Clojure 800 ms
+ Go 17 ms
+ Haskell 150 ms
+ F# 320 ms
+ D 13 ms

[wikipedia]: http://en.wikipedia.org/wiki/Weasel_program
[algorithm]: http://rationalwiki.org/wiki/Dawkins_weasel
