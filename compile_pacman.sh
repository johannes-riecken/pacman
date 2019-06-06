g++ -O -fPIC -o libpacman.so -shared Pacman.cpp `perl -MExtUtils::Embed -e ccopts -e ldopts`
ghc -dynamic -package-db ~/.cabal/store/ghc-8.6.5/package.db Pacman.hs /home/jo/haskell/pacman/libpacman.so 
LD_LIBRARY_PATH=. ./Pacman
