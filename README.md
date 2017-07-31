[![Build Status](https://travis-ci.org/dehun/bce.svg?branch=master)](https://travis-ci.org/dehun/bce)
# Intro #
Bce is BlockChain experiment intended to grasp the essence of blockchain technology.
It is basically a bitcoin copycat with some divergencies in protocols, store, etc.

# Running #

Run in 2 different terminals on the same machine:

    ./dist/build/bce/bce "(127,0,0,1)" 3666 "(127,0,0,1)" 3999
    ./dist/build/bce/bce "(127,0,0,1)" 3999 "(127,0,0,1)" 3888
    
Where first pair "(127,0,0,1)" 3666 is about on which address to bind.
Second one "(127,0,0,1)" 3999 is p2p seed node.
