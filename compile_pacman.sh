gcc -O -fPIC -o libpacman.so -shared Pacman.c `perl -MExtUtils::Embed -e ccopts -e ldopts`
ghc -dynamic Pacman.hs libpacman.so 
LD_LIBRARY_PATH=. ./Pacman
