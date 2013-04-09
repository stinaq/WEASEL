A whole lotta weasels
======

A joint effort to write the [Dawkins' Weasel algorithm][wikipedia] in as many languages as possible, using the language's specific idioms.

Algorithm described [here][algorithm].

+ Each implementation should only be one file and placed in the root folder.
+ Each file should be named by the language in which it was written.
+ The program should print the fittest string of each generation, as well as the number of generations it took.
+ Mutation rate should be 4%, generation size 100.


Each implementation should, as far as it is feasable, capture the charachteristics of that language. The goal is readable and enjoyable implementations in as many languages as possible.

[wikipedia]: http://en.wikipedia.org/wiki/Weasel_program
[algorithm]: http://rationalwiki.org/wiki/Dawkins_weasel

### Building the examples:

#### (Linuxes)

##### Ubuntu

###### Go:
        apt-get install go
        go run go.go
###### D:
        apt-get install gdc
        gdc d.d
        ./a.out

###### F#:
- first install mono:
        apt-get install mono
- clone the github repo and follow the instructions there (make && make install)
- then:
        fsharpc fsharp.fs
        mono fsharp.exe
