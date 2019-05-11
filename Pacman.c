// vim: set path+=/usr/lib/perl5/5.28/core_perl/CORE :
#include "Pacman.h"
#include <EXTERN.h>
#include <perl.h>

static PerlInterpreter *my_perl;
#define PIXEL_NEIGHBORS 8

Point *filterCorners(Point *points, size_t len, size_t *outlen) {
  char *my_argv[] = { "", "Pacman.pm" };
  char **my_argv2[] = { my_argv };
  int argc = 1;
  char *env[] = { NULL };
  char **env2[] = { env };
  PERL_SYS_INIT3(&argc,my_argv2,env2);
  my_perl = perl_alloc();
  perl_construct( my_perl );
  perl_parse(my_perl, NULL, 2, my_argv, (char **)NULL);
  PL_exit_flags |= (unsigned int)PERL_EXIT_DESTRUCT_END;
  perl_run(my_perl);
  dSP;
  ENTER;
  SAVETMPS;
  PUSHMARK(SP);
  AV *pnts_in = newAV();
  for (size_t i = 0; i < len; i++) {
    SV *x = newSViv(points[i].x);
    SV *y = newSViv(points[i].y);
    AV *pnt_av = newAV();
    av_push(pnt_av, x);
    av_push(pnt_av, y);
    SV *pnt = newRV_inc((SV*) pnt_av);
    av_push(pnts_in, pnt);
  }
  SV *param = newRV_inc((SV*) pnts_in);
  XPUSHs(sv_2mortal(param));
  PUTBACK;
  call_pv("Pacman::filterCorners", G_SCALAR);
  SPAGAIN;
  SV *popped = POPs;
  AV *popped_av = (AV*) SvRV(popped);
  *outlen = (size_t) av_top_index(popped_av) + 1;
  // static Point ret[*outlen];
  Point *ret = malloc(sizeof(Point) * *outlen);
  for (size_t i = 0; i < *outlen; i++) {
    SV *pnt_rv = av_pop(popped_av);
    AV *pnt_av = (AV*) SvRV(pnt_rv);
    SV *pnt_y_sv = av_pop(pnt_av);
    SV *pnt_x_sv = av_pop(pnt_av);
    int pnt_y = SvIV(pnt_y_sv);
    int pnt_x = SvIV(pnt_x_sv);
    Point pnt = { pnt_x, pnt_y };
    ret[*outlen - i - 1] = pnt;
  }
  PUTBACK;
  FREETMPS;
  LEAVE;
  perl_destruct(my_perl);
  perl_free(my_perl);
  return ret;
}

Point*  neighborsImpl(int x, int y, int dir_x, int dir_y) {
  char *my_argv[] = { "", "Pacman.pm" };
  char **my_argv2[] = { my_argv };
  int argc = 1;
  char *env[] = { NULL };
  char **env2[] = { env };
  PERL_SYS_INIT3(&argc,my_argv2,env2);
  my_perl = perl_alloc();
  perl_construct( my_perl );
  perl_parse(my_perl, NULL, 2, my_argv, (char **)NULL);
  PL_exit_flags |= (unsigned int)PERL_EXIT_DESTRUCT_END;
  perl_run(my_perl);
  dSP;                            /* initialize stack pointer      */
  ENTER;                          /* everything created after here */
  SAVETMPS;                       /* ...is a temporary variable.   */
  PUSHMARK(SP);                   /* remember the stack pointer    */
  XPUSHs(sv_2mortal(newSViv(x))); /* push the base onto the stack  */
  XPUSHs(sv_2mortal(newSViv(y))); /* push the exponent onto stack  */
  XPUSHs(sv_2mortal(newSViv(dir_x))); /* push the exponent onto stack  */
  XPUSHs(sv_2mortal(newSViv(dir_y))); /* push the exponent onto stack  */
  PUTBACK;                      /* make local stack pointer global */
  call_pv("Pacman::neighbors", G_ARRAY);      /* call the function             */
  SPAGAIN;                        /* refresh stack pointer         */
  static Point pnts[PIXEL_NEIGHBORS];
  for (int i = PIXEL_NEIGHBORS - 1; i >= 0; i--) {
    SV *tmp = POPs;
    AV *pnt = (AV*) SvRV(tmp);
    SV *sv0 = av_pop(pnt);
    int pnt_y = SvIV(sv0);
    // int pnt_y = SvIV(av_pop(pnt));
    SV *sv1 = av_pop(pnt);
    // sv_dump(sv0);
    int pnt_x = SvIV(sv1);
    // int pnt_x = SvIV(av_pop(pnt));
    Point p_tmp = { pnt_x, pnt_y };
    pnts[i] = p_tmp;
  }
  PUTBACK;
  FREETMPS;                       /* free that return value        */
  LEAVE;                       /* ...and the XPUSHed "mortal" args.*/
  perl_destruct(my_perl);
  perl_free(my_perl);
//   PERL_SYS_TERM();
  return pnts;
}

// TODO: fill in details
Point*  walkLine(__attribute__((unused)) int **img, int x, int y, __attribute__((unused)) Point *seen) {
  char *my_argv[] = { "", "Pacman.pm" };
  char **my_argv2[] = { my_argv };
  int argc = 1;
  char *env[] = { NULL };
  char **env2[] = { env };
  PERL_SYS_INIT3(&argc,my_argv2,env2);
  my_perl = perl_alloc();
  perl_construct( my_perl );
  perl_parse(my_perl, NULL, 2, my_argv, (char **)NULL);
  PL_exit_flags |= (unsigned int)PERL_EXIT_DESTRUCT_END;
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
    AV *pnt = (AV*) SvRV(tmp);
    SV *sv0 = av_pop(pnt);
    int pnt_y = SvIV(sv0);
    // int pnt_y = SvIV(av_pop(pnt));
    SV *sv1 = av_pop(pnt);
    // sv_dump(sv0);
    int pnt_x = SvIV(sv1);
    // int pnt_x = SvIV(av_pop(pnt));
    Point p_tmp = { pnt_x, pnt_y };
    pnts[i] = p_tmp;
  }
  PUTBACK;
  FREETMPS;
  LEAVE;
  perl_destruct(my_perl);
  perl_free(my_perl);
//   PERL_SYS_TERM();
  return pnts;
}

int main (__attribute__((unused)) int argc, __attribute__((unused)) char **argv, __attribute__((unused)) char **env)
{
  setvbuf(stdout, NULL, _IONBF, 0);
  // neighbors(1,1,1,1);
  Point *ns = neighborsImpl(0,0,1,1);
  for (int i = 0; i < PIXEL_NEIGHBORS; i++) {
    printf("%d, %d\n", ns[i].x, ns[i].y);
  }
  Point *ms = neighborsImpl(1,1,1,1);
  for (int i = 0; i < PIXEL_NEIGHBORS; i++) {
    printf("%d, %d\n", ms[i].x, ms[i].y);
  }
  Point *empty = NULL;
  size_t outlen;
  Point *res = filterCorners(empty, 0, &outlen);
  printf("%d, %d\n", res->x, res->y);
}

