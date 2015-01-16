# Installation

Recommended that you use cabal sandbox, so that nothing you install here gets into your general cabal directory:

    cabal sandbox init

You may want to make sure you have the latest and greatest, though cabal will probably warn you if you need to do this:

    cabal update

Now build everything:

    cabal install --dependencies-only
    cabal build

# Setup

There's a directory called `training`. Put however many files you want in there, ending in `.txt`. These will be used as training text.

# Running

    ./random-chain
