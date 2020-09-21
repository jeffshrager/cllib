/* Double precision version of routines in ERF from the netlib SPECFUN */
/* library. Translated by f2c and modified. */

#include "xlisp.h"
#include "xlstat.h"
#include "linalg.h"

static VOID calerf P3C(double, arg, double *, result, int, jint)
{
  /* ------------------------------------------------------------------ */
  /* This packet evaluates  erf(x),  erfc(x),  and  exp(x*x)*erfc(x) */
  /*   for a real argument  x.  It contains three FUNCTION type */
  /*   subprograms: ERF, ERFC, and ERFCX (or DERF, DERFC, and DERFCX), */
  /*   and one SUBROUTINE type subprogram, CALERF.  The calling */
  /*   statements for the primary entries are: */

  /*                   Y=ERF(X)     (or   Y=DERF(X)), */

  /*                   Y=ERFC(X)    (or   Y=DERFC(X)), */
  /*   and */
  /*                   Y=ERFCX(X)   (or   Y=DERFCX(X)). */

  /*   The routine  CALERF  is intended for internal packet use only, */
  /*   all computations within the packet being concentrated in this */
  /*   routine.  The function subprograms invoke  CALERF  with the */
  /*   statement */

  /*          CALL CALERF(ARG,RESULT,JINT) */

  /*   where the parameter usage is as follows */

  /*      Function                     Parameters for CALERF */
  /*       call              ARG                  Result          JINT */

  /*     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0 */
  /*     ERFC(ARG)     ABS(ARG) .LT. XBIG        ERFC(ARG)         1 */
  /*     ERFCX(ARG)    XNEG .LT. ARG .LT. XMAX   ERFCX(ARG)        2 */

  /*   The main computation evaluates near-minimax approximations */
  /*   from "Rational Chebyshev approximations for the error function" */
  /*   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This */
  /*   transportable program uses rational functions that theoretically */
  /*   approximate  erf(x)  and  erfc(x)  to at least 18 significant */
  /*   decimal digits.  The accuracy achieved depends on the arithmetic */
  /*   system, the compiler, the intrinsic functions, and proper */
  /*   selection of the machine-dependent constants. */

  /* ******************************************************************* */
  /* ******************************************************************* */

  /* Explanation of machine-dependent constants */

  /*   XMIN   = the smallest positive floating-point number. */
  /*   XINF   = the largest positive finite floating-point number. */
  /*   XNEG   = the largest negative argument acceptable to ERFCX; */
  /*            the negative of the solution to the equation */
  /*            2*exp(x*x) = XINF. */
  /*   XSMALL = argument below which erf(x) may be represented by */
  /*            2*x/sqrt(pi)  and above which  x*x  will not underflow. */
  /*            A conservative value is the largest machine number X */
  /*            such that   1.0 + X = 1.0   to machine precision. */
  /*   XBIG   = largest argument acceptable to ERFC;  solution to */
  /*            the equation:  W(x) * (1-0.5/x**2) = XMIN,  where */
  /*            W(x) = exp(-x*x)/[x*sqrt(pi)]. */
  /*   XHUGE  = argument above which  1.0 - 1/(2*x*x) = 1.0  to */
  /*            machine precision.  A conservative value is */
  /*            1/[2*sqrt(XSMALL)] */
  /*   XMAX   = largest acceptable argument to ERFCX; the minimum */
  /*            of XINF and 1/[sqrt(pi)*XMIN]. */

  /*   Approximate values for some important machines are: */

  /*                          XMIN       XINF        XNEG     XSMALL */

  /*  CDC 7600      (S.P.)  3.13E-294   1.26E+322   -27.220  7.11E-15 */
  /*  CRAY-1        (S.P.)  4.58E-2467  5.45E+2465  -75.345  7.11E-15 */
  /*  IEEE (IBM/XT, */
  /*    SUN, etc.)  (S.P.)  1.18E-38    3.40E+38     -9.382  5.96E-8 */
  /*  IEEE (IBM/XT, */
  /*    SUN, etc.)  (D.P.)  2.23D-308   1.79D+308   -26.628  1.11D-16 */
  /*  IBM 195       (D.P.)  5.40D-79    7.23E+75    -13.190  1.39D-17 */
  /*  UNIVAC 1108   (D.P.)  2.78D-309   8.98D+307   -26.615  1.73D-18 */
  /*  VAX D-Format  (D.P.)  2.94D-39    1.70D+38     -9.345  1.39D-17 */
  /*  VAX G-Format  (D.P.)  5.56D-309   8.98D+307   -26.615  1.11D-16 */


  /*                          XBIG       XHUGE       XMAX */

  /*  CDC 7600      (S.P.)  25.922      8.39E+6     1.80X+293 */
  /*  CRAY-1        (S.P.)  75.326      8.39E+6     5.45E+2465 */
  /*  IEEE (IBM/XT, */
  /*    SUN, etc.)  (S.P.)   9.194      2.90E+3     4.79E+37 */
  /*  IEEE (IBM/XT, */
  /*    SUN, etc.)  (D.P.)  26.543      6.71D+7     2.53D+307 */
  /*  IBM 195       (D.P.)  13.306      1.90D+8     7.23E+75 */
  /*  UNIVAC 1108   (D.P.)  26.582      5.37D+8     8.98D+307 */
  /*  VAX D-Format  (D.P.)   9.269      1.90D+8     1.70D+38 */
  /*  VAX G-Format  (D.P.)  26.569      6.71D+7     8.98D+307 */

  /* ******************************************************************* */
  /* ******************************************************************* */

  /* Error returns */

  /*  The program returns  ERFC = 0      for  ARG .GE. XBIG; */

  /*                       ERFCX = XINF  for  ARG .LT. XNEG; */
  /*      and */
  /*                       ERFCX = 0     for  ARG .GE. XMAX. */


  /* Intrinsic functions required are: */

  /*     ABS, AINT, EXP */


  /*  Author: W. J. Cody */
  /*          Mathematics and Computer Science Division */
  /*          Argonne National Laboratory */
  /*          Argonne, IL 60439 */

  /*  Latest modification: March 19, 1990 */

  /* ------------------------------------------------------------------ */
  double xden, xnum;
  int i;
  double x, y, del, ysq;
  /* ------------------------------------------------------------------ */
  /*  Mathematical constants */
  /* ------------------------------------------------------------------ */
  static double four = 4.;
  static double one = 1.;
  static double half = .5;
  static double two = 2.;
  static double zero = 0.;
  static double sqrpi = .56418958354775628695;
  static double thresh = .46875;
  static double sixten = 16.;
  /* ------------------------------------------------------------------ */
  /*  Machine-dependent constants */
  /* ------------------------------------------------------------------ */
#ifdef IEEEFP
  static double xinf = 1.79e308;
  static double xneg = -26.628;
  static double xsmall = 1.11e-16;
  static double xbig = 26.543;
  static double xhuge = 6.71e7;
  static double xmax = 2.53e307;
#else
#ifdef CRAYCC
  static double xinf = 5.45e2465;
  static double xneg = -75.345;
  static double xsmall = 7.11e-15;
  static double xbig = 75.326;
  static double xhuge = 8.39e6;
  static double xmax = 5.45e2465;
#else /* use IBM 196 values */
  static double xinf = 7.23e75;
  static double xneg = -13.190;
  static double xsmall = 1.39e-17;
  static double xbig = 13.306;
  static double xhuge = 1.90e8;
  static double xmax = 7.23e75;
#endif /* CRAYCC */
#endif /* IEEEFP */
  /* ------------------------------------------------------------------ */
  /*  Coefficients for approximation to  erf  in first interval */
  /* ------------------------------------------------------------------ */
  static double a[5] = { 3.1611237438705656,113.864154151050156,
			   377.485237685302021,3209.37758913846947,
			   .185777706184603153 };
  static double b[4] = { 23.6012909523441209,244.024637934444173,
			   1282.61652607737228,2844.23683343917062 };
  /* ------------------------------------------------------------------ */
  /*  Coefficients for approximation to  erfc  in second interval */
  /* ------------------------------------------------------------------ */
  static double c[9] = { .564188496988670089,8.88314979438837594,
			   66.1191906371416295,298.635138197400131,
			   881.95222124176909,1712.04761263407058,
			   2051.07837782607147,1230.33935479799725,
			   2.15311535474403846e-8 };
  static double d[8] = { 15.7449261107098347,117.693950891312499,
			   537.181101862009858,1621.38957456669019,
			   3290.79923573345963,4362.61909014324716,
			   3439.36767414372164,1230.33935480374942 };
  /* ------------------------------------------------------------------ */
  /*  Coefficients for approximation to  erfc  in third interval */
  /* ------------------------------------------------------------------ */
  static double p[6] = { .305326634961232344,.360344899949804439,
			   .125781726111229246,.0160837851487422766,
			   6.58749161529837803e-4,.0163153871373020978 };
  static double q[5] = { 2.56852019228982242,1.87295284992346047,
			   .527905102951428412,.0605183413124413191,
			   .00233520497626869185 };
  /* ------------------------------------------------------------------ */
  x = arg;
  y = abs(x);
  if (y <= thresh) {
    /* ------------------------------------------------------------------ */
    /*  Evaluate  erf  for  |X| <= 0.46875 */
    /* ------------------------------------------------------------------ */
    ysq = zero;
    if (y > xsmall) {
      ysq = y * y;
    }
    xnum = a[4] * ysq;
    xden = ysq;
    for (i = 1; i <= 3; ++i) {
      xnum = (xnum + a[i - 1]) * ysq;
      xden = (xden + b[i - 1]) * ysq;
    }
    *result = x * (xnum + a[3]) / (xden + b[3]);
    if (jint != 0) {
      *result = one - *result;
    }
    if (jint == 2) {
      *result = exp(ysq) * *result;
    }
    return;
    /* ------------------------------------------------------------------ */
    /*  Evaluate  erfc  for 0.46875 <= |X| <= 4.0 */
    /* ------------------------------------------------------------------ */
  }
  else if (y <= four) {
    xnum = c[8] * y;
    xden = y;
    for (i = 1; i <= 7; ++i) {
      xnum = (xnum + c[i - 1]) * y;
      xden = (xden + d[i - 1]) * y;
    }
    *result = (xnum + c[7]) / (xden + d[7]);
    if (jint != 2) {
      ysq = floor(y * sixten) / sixten;
      del = (y - ysq) * (y + ysq);
      *result = exp(-ysq * ysq) * exp(-del) * *result;
    }
    /* ------------------------------------------------------------------ */
    /*  Evaluate  erfc  for |X| > 4.0 */
    /* ------------------------------------------------------------------ */
  }
  else {
    *result = zero;
    if (y >= xbig) {
      if (jint != 2 || y >= xmax) {
	goto L300;
      }
      if (y >= xhuge) {
	*result = sqrpi / y;
	goto L300;
      }
    }
    ysq = one / (y * y);
    xnum = p[5] * ysq;
    xden = ysq;
    for (i = 1; i <= 4; ++i) {
      xnum = (xnum + p[i - 1]) * ysq;
      xden = (xden + q[i - 1]) * ysq;
    }
    *result = ysq * (xnum + p[4]) / (xden + q[4]);
    *result = (sqrpi - *result) / y;
    if (jint != 2) {
      ysq = floor(y * sixten) / sixten;
      del = (y - ysq) * (y + ysq);
      *result = exp(-ysq * ysq) * exp(-del) * *result;
    }
  }
  /* ------------------------------------------------------------------ */
  /*  Fix up for negative argument, erf, etc. */
  /* ------------------------------------------------------------------ */
 L300:
  if (jint == 0) {
    *result = half - *result + half;
    if (x < zero) {
      *result = -(*result);
    }
  }
  else if (jint == 1) {
    if (x < zero) {
      *result = two - *result;
    }
  }
  else {
    if (x < zero) {
      if (x < xneg) {
	*result = xinf;
      }
      else {
	ysq = -floor(-(x * sixten)) / sixten;
	del = (x - ysq) * (x + ysq);
	y = exp(ysq * ysq) * exp(del);
	*result = y + y - *result;
      }
    }
  }
  return;
}

#ifdef DEBUG
double derf_ P1C(double *, x)
{
  int jint;
  double result;

  /* This subprogram computes approximate values for erf(x). */
  /*   (see comments heading CALERF). */
  /*   Author/date: W. J. Cody, January 8, 1985 */

  jint = 0;
  calerf(*x, &result, jint);
  return result;
}

double derfc_ P1C(double *, x)
{
  int jint;
  double result;

  /* This subprogram computes approximate values for erfc(x). */
  /*   (see comments heading CALERF). */
  /*   Author/date: W. J. Cody, January 8, 1985 */

  jint = 1;
  calerf(*x, &result, jint);
  return result;
}

double derfcx_ (double *, x)
{
  int jint;
  double result;

  /* This subprogram computes approximate values for exp(x*x) * erfc(x). */
  /*   (see comments heading CALERF). */
  /*   Author/date: W. J. Cody, March 30, 1987 */

  jint = 2;
  calerf(*x, &result, jint);
  return result;
}
#endif /* DEBUG */

#define SQRT2 1.414213562373095049

VOID normbase P2C(double *, x, double *, phi)
{
  double y;

  y = *x / SQRT2;
  if (y < 0) {
    calerf(-y, phi, 1);
    *phi = 0.5 * *phi;
  }
  else {
    calerf(y, phi, 1);
    *phi = 1.0 - 0.5 * *phi;
  }
}
