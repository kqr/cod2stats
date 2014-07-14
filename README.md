cod2stats
=========

Executing
---------

To initialise environment:

    cabal sandbox init
    
To build:

    cabal install --only-dependencies
    cabal build

To run:

    export PORT=3000
    export DATABASE_URL="postgres://user:password@host:port/databasename"
    foreman run


