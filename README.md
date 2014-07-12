cod2stats
=========

Executing
---------

To initialise environment:

    cabal sandbox init
    cabal sandbox add-source ../path/to/patched/postgresql-simple
    
To build:

    cabal install --only-dependencies
    cabal build

To run:

    ./dist/build/cod2stats/cod2stats


