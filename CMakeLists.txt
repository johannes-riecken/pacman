cmake_minimum_required (VERSION 2.6)
project (Pacman)
# The version number.
set (Pacman_VERSION_MAJOR 1)
set (Pacman_VERSION_MINOR 0)

include_directories("/usr/lib/perl5/5.28/core_perl/CORE" "/usr/local/include")

link_directories("/usr/local/lib" "/usr/lib/perl5/5.28/core_perl/CORE")

add_library(pacman SHARED Pacman.c)
target_link_libraries (pacman perl pthread dl m crypt util c)
