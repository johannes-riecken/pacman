// vim: set path+=/usr/lib/perl5/5.28/core_perl/CORE :
#include <array>
#include <gsl/gsl>
#include <iostream>

#include "Pacman.h"
#include <EXTERN.h>
#include <perl.h>

using gsl::span;
using std::array;
using std::cout;
using std::endl;
using std::string;

static PerlInterpreter *my_perl;
static const int PIXEL_NEIGHBORS = 8;

extern "C" {
Point *filterCorners(span<Point> points, size_t *outlen) {
  array<char *, 2> my_argv{"", "Pacman.pm"};
  array<char **, 1> my_argv2{&my_argv[0]};
  int argc = 1;
  array<char *, 1> env = {nullptr};
  array<char **, 1> env2 = {&env[0]};
  PERL_SYS_INIT3(&argc, &my_argv2[0], &env2[0]);
  my_perl = perl_alloc();
  perl_construct(my_perl);
  perl_parse(my_perl, nullptr, 2, &my_argv[0], static_cast<char **>(nullptr));
  PL_exit_flags |= static_cast<unsigned int>(PERL_EXIT_DESTRUCT_END);
  perl_run(my_perl);
  dSP;
  ENTER;
  SAVETMPS;
  PUSHMARK(SP);
  AV *pnts_in = newAV();
  for (auto &point : points) {
    SV *x = newSViv(point.x);
    SV *y = newSViv(point.y);
    AV *pnt_av = newAV();
    av_push(pnt_av, x);
    av_push(pnt_av, y);
    SV *pnt = newRV_inc(reinterpret_cast<SV *>(pnt_av));
    av_push(pnts_in, pnt);
  }
  SV *param = newRV_inc(reinterpret_cast<SV *>(pnts_in));
  XPUSHs(sv_2mortal(param));
  PUTBACK;
  call_pv("Pacman::filterCorners", G_SCALAR);
  SPAGAIN;
  SV *popped = POPs;
  AV *popped_av = reinterpret_cast<AV *>(SvRV(popped));
  *outlen = static_cast<size_t>(av_top_index(popped_av) + 1);
  // static Point ret[*outlen];
  gsl::owner<Point *> ret =
      reinterpret_cast<Point *>(malloc(sizeof(Point) * *outlen));
  for (size_t i = 0; i < *outlen; i++) {
    SV *pnt_rv = av_pop(popped_av);
    AV *pnt_av = reinterpret_cast<AV *>(SvRV(pnt_rv));
    SV *pnt_y_sv = av_pop(pnt_av);
    SV *pnt_x_sv = av_pop(pnt_av);
    int pnt_y = SvIV(pnt_y_sv);
    int pnt_x = SvIV(pnt_x_sv);
    Point pnt = {pnt_x, pnt_y};
    ret[*outlen - i - 1] = pnt; // TODO pointer-arithmetic
  }
  PUTBACK;
  FREETMPS;
  LEAVE;
  perl_destruct(my_perl);
  perl_free(my_perl);
  return ret;
}

Point *neighborsImpl(int x, int y, int dir_x, int dir_y) {
  array<char *, 2> my_argv{"", "Pacman.pm"};
  array<char **, 1> my_argv2{&my_argv[0]};
  int argc = 1;
  array<char *, 1> env = {nullptr};
  array<char **, 1> env2 = {&env[0]};
  PERL_SYS_INIT3(&argc, &my_argv2[0], &env2[0]);
  my_perl = perl_alloc();
  perl_construct(my_perl);
  perl_parse(my_perl, nullptr, 2, &my_argv[0], static_cast<char **>(nullptr));
  PL_exit_flags |= static_cast<unsigned int>(PERL_EXIT_DESTRUCT_END);
  perl_run(my_perl);
  dSP;                                   /* initialize stack pointer      */
  ENTER;                                 /* everything created after here */
  SAVETMPS;                              /* ...is a temporary variable.   */
  PUSHMARK(SP);                          /* remember the stack pointer    */
  XPUSHs(sv_2mortal(newSViv(x)));        /* push the base onto the stack  */
  XPUSHs(sv_2mortal(newSViv(y)));        /* push the exponent onto stack  */
  XPUSHs(sv_2mortal(newSViv(dir_x)));    /* push the exponent onto stack  */
  XPUSHs(sv_2mortal(newSViv(dir_y)));    /* push the exponent onto stack  */
  PUTBACK;                               /* make local stack pointer global */
  call_pv("Pacman::neighbors", G_ARRAY); /* call the function             */
  SPAGAIN;                               /* refresh stack pointer         */
  array<Point, PIXEL_NEIGHBORS> pnts{};
  for (int i = PIXEL_NEIGHBORS - 1; i >= 0; i--) {
    SV *tmp = POPs;
    AV *pnt = reinterpret_cast<AV *>(SvRV(tmp));
    SV *sv0 = av_pop(pnt);
    int pnt_y = SvIV(sv0);
    // int pnt_y = SvIV(av_pop(pnt));
    SV *sv1 = av_pop(pnt);
    // sv_dump(sv0);
    int pnt_x = SvIV(sv1);
    // int pnt_x = SvIV(av_pop(pnt));
    Point p_tmp = {pnt_x, pnt_y};
    pnts[i] = p_tmp; // TODO constant-array-index
  }
  PUTBACK;
  FREETMPS; /* free that return value        */
  LEAVE;    /* ...and the XPUSHed "mortal" args.*/
  perl_destruct(my_perl);
  perl_free(my_perl);
  //   PERL_SYS_TERM();
  return &pnts[0];
}

// TODO: fill in details
Point *walkLine(__attribute__((unused)) int **img, int x, int y,
                __attribute__((unused)) Point *seen) {
  array<char *, 2> my_argv{"", "Pacman.pm"};
  array<char **, 1> my_argv2{&my_argv[0]};
  int argc = 1;
  array<char *, 1> env = {nullptr};
  array<char **, 1> env2 = {&env[0]};
  PERL_SYS_INIT3(&argc, &my_argv2[0], &env2[0]);
  my_perl = perl_alloc();
  perl_construct(my_perl);
  perl_parse(my_perl, nullptr, 2, &my_argv[0], static_cast<char **>(nullptr));
  PL_exit_flags |= static_cast<unsigned int>(PERL_EXIT_DESTRUCT_END);
  perl_run(my_perl);
  dSP;
  ENTER;
  SAVETMPS;
  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSViv(x)));
  XPUSHs(sv_2mortal(newSViv(y)));
  PUTBACK;
  call_pv("Pacman::walkLine", G_ARRAY);
  SPAGAIN;
  static Point pnts[PIXEL_NEIGHBORS];
  for (int i = PIXEL_NEIGHBORS - 1; i >= 0; i--) {
    SV *tmp = POPs;
    AV *pnt = reinterpret_cast<AV *>(SvRV(tmp));
    SV *sv0 = av_pop(pnt);
    int pnt_y = SvIV(sv0);
    // int pnt_y = SvIV(av_pop(pnt));
    SV *sv1 = av_pop(pnt);
    // sv_dump(sv0);
    int pnt_x = SvIV(sv1);
    // int pnt_x = SvIV(av_pop(pnt));
    Point p_tmp = {pnt_x, pnt_y};
    pnts[i] = p_tmp; // TODO constant array index
  }
  PUTBACK;
  FREETMPS;
  LEAVE;
  perl_destruct(my_perl);
  perl_free(my_perl);
  //   PERL_SYS_TERM();
  return pnts; // TODO array no pointer decay
}

int main(__attribute__((unused)) int argc, __attribute__((unused)) char **argv,
         __attribute__((unused)) char **env) {
  setvbuf(stdout, nullptr, _IONBF, 0);
  // neighbors(1,1,1,1);
  Point *ns = neighborsImpl(0, 0, 1, 1);
  for (int i = 0; i < PIXEL_NEIGHBORS; i++) {
    cout << ns[i].x << ", " << ns[i].y << endl;
  }
  Point *ms = neighborsImpl(1, 1, 1, 1);
  for (int i = 0; i < PIXEL_NEIGHBORS; i++) {
    cout << ms[i].x << ", " << ms[i].y << endl;
  }
  span<Point> empty{};
  size_t outlen;
  Point *res = filterCorners(empty, &outlen);
  cout << res->x << ", " << res->y << endl;
}
}
