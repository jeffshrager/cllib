/*
  Translated from RATFOR lowess code of W. S. Cleveland as obtained from NETLIB
*/

#include "xlisp.h"
#include "xlstat.h"
#ifdef MACINTOSH
# undef fmax
# define fmax Fmaxx
#endif

/* forward declarations */
static double pow2 P1H(double);
static double pow3 P1H(double x);
static double fmax P2H(double, double);
static VOID sort P2H(double *, int);
static VOID lowest P11H(double *, double *, int, double, double *,
                        int, int, double *, int, double *, int *);


static double pow2 P1C(double, x) { return(x * x); }
static double pow3 P1C(double, x) { return(x * x * x); }
static double fmax P2C(double, x, double, y) { return (x > y ? x : y); }

int lowess P9C(double *, x, double *, y, int, n, double, f, int, nsteps, double, delta,
               double *, ys, double *, rw, double *, res)
{
  int iter, ns, ok, nleft, nright, i, j, last, m1, m2;
  double d1, d2, denom, alpha, cut, cmad, c9, c1, r;
  
  if (n < 2) { ys[0] = y[0]; return(1); }
  ns = max(min((int) (f * n), n), 2);  /* at least two, at most n points */
  for(iter = 1; iter <= nsteps + 1; iter++){      /* robustness iterations */
    nleft = 0; nright = ns - 1;
    last = -1;        /* index of prev estimated point */
    i = 0;   /* index of current point */
    do {
      while(nright < n - 1){
	/* move nleft, nright to right if radius decreases */
	d1 = x[i] - x[nleft];
	d2 = x[nright + 1] - x[i];
	/* if d1 <= d2 with x[nright+1] == x[nright], lowest fixes */
	if (d1 <= d2) break;
	/* radius will not decrease by move right */
	nleft++;
	nright++;
      }
      lowest(x, y, n, x[i], &ys[i], nleft, nright, res, (iter > 1), rw, &ok);
      /* fitted value at x[i] */
      if (! ok) ys[i] = y[i];
      /* all weights zero - copy over value (all rw==0) */
      if (last < i - 1) { /* skipped points -- interpolate */
	denom = x[i] - x[last];    /* non-zero - proof? */
	for(j = last + 1; j < i; j = j + 1){
	  alpha = (x[j] - x[last]) / denom;
	  ys[j] = alpha * ys[i] + (1.0 - alpha) * ys[last];
	}
      }
      last = i;        /* last point actually estimated */
      cut = x[last] + delta;     /* x coord of close points */
      for(i=last + 1; i < n; i++) {     /* find close points */
	if (x[i] > cut) break;     /* i one beyond last pt within cut */
	if(x[i] == x[last]) {      /* exact match in x */
	  ys[i] = ys[last];
	  last = i;
	}
      }
      i = max(last + 1,i - 1);
      /* back 1 point so interpolation within delta, but always go forward */
    } while(last < n - 1);
    for (i = 0; i < n; i++)      /* residuals */
      res[i] = y[i] - ys[i];
    if (iter > nsteps) break; /* compute robustness weights except last time */
    for (i = 0; i < n; i++) 
      rw[i] = fabs(res[i]);
    sort(rw,n);
    m1 = 1 + n / 2; m2 = n - m1 + 1;
    cmad = 3.0 * (rw[m1] + rw[m2]);      /* 6 median abs resid */
    c9 = .999 * cmad; c1 = .001 * cmad;
    for (i = 0; i < n; i++) {
      r = fabs(res[i]);
      if(r <= c1) rw[i] = 1.0;      /* near 0, avoid underflow */
      else if(r > c9) rw[i] = 0.0;  /* near 1, avoid underflow */
      else rw[i] = pow2(1.0 - pow2(r / cmad));
    }
  }
  return(0);
}

static VOID lowest P11C(double *, x, double *, y, int, n, double, xs, double *, ys,
                        int, nleft, int, nright, double *, w, int, userw, double *, rw, int *, ok)
{
  double range, h, h1, h9, a, b, c, r;
  int j, nrt;

  range = x[n - 1] - x[0];
  h = fmax(xs - x[nleft], x[nright] - xs);
  h9 = .999 * h;
  h1 = .001 * h;

  /* compute weights (pick up all ties on right) */
  a = 0.0;        /* sum of weights */
  for(j = nleft; j < n; j++) {
    w[j]=0.0;
    r = fabs(x[j] - xs);
    if (r <= h9) {    /* small enough for non-zero weight */
      if (r > h1) w[j] = pow3(1.0-pow3(r/h));
      else w[j] = 1.0;
      if (userw) w[j] = rw[j] * w[j];
      a += w[j];
    }
    else if (x[j] > xs) break;  /* get out at first zero wt on right */
  }
  nrt = j - 1;  /* rightmost pt (may be greater than nright because of ties) */
  if (a <= 0.0) *ok = FALSE;
  else { /* weighted least squares */
    *ok = TRUE;

    /* make sum of w[j] == 1 */
    for (j = nleft; j <= nrt; j++) w[j] = w[j] / a;

    if (h > 0.0) {     /* use linear fit */

      /* find weighted center of x values */
      for (j = nleft, a = 0.0; j <= nrt; j++) a += w[j] * x[j];

      b = xs - a;
      for (j = nleft, c = 0.0; j <= nrt; j++) 
	c += w[j] * (x[j] - a) * (x[j] - a);

      if(sqrt(c) > .001 * range) {
	/* points are spread out enough to compute slope */
	b = b/c;
	for (j = nleft; j <= nrt; j++) 
	  w[j] = w[j] * (1.0 + b*(x[j] - a));
      }
    }
    for (j = nleft, *ys = 0.0; j <= nrt; j++) *ys += w[j] * y[j];
  }
}
 
#ifdef ANSI
static int compar(const void *pa, const void *pb)
#else
static int compar(pa, pb)
     ALLOCTYPE *pa, *pb;
#endif
{
  double a = *((double *) pa), b = *((double *) pb);
  if (a < b) return(-1);
  else if (a > b) return(1);
  else return(0);
}

static VOID sort P2C(double *, x, int, n)
{
  qsort(x, n, sizeof(double), compar);
}

