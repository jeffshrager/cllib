#include "xlisp.h"
#include "xlstat.h"

#ifndef ROOT2PI
#define ROOT2PI 2.50662827463100050241
#endif

static double kernel P4C(double, x, double, y, double, w, int, type)
{
  double z, k;
  
  if (w > 0.0) {
    z = (x - y) / w;
    switch (type) {
    case 'B':
      w = 2.0 * w;
      z = 0.5 * z;
      if (-0.5 < z && z < 0.5) {
        z = (1.0 - 4 * z * z);
        k = 15.0 * z * z / 8.0;
      }
      else k = 0.0;
      break;
    case 'G':
      w = 0.25 * w;
      z = 4.0 * z;
      k = exp(- 0.5 * z * z) / ROOT2PI; 
      break;
    case 'U':
      w = 1.5 * w;
      z = .75 * z;
      k = (fabs(z) < 0.5) ? 1.0 : 0.0;
      break;
    case 'T': 
      if (-1.0 <= z && z < 0.0) k = 1.0 + z;
      else if (0.0 <= z && z < 1.0) k = 1.0 - z;
      else k = 0.0;
      break;
    default:  k = 0.0; break;
    }
    k = k / w;
  }
  else k = 0.0;

  return(k);
}

int kernel_smooth P10C(double *, x, double *, y, int, n, double, width,
                       double *, wts, double *, wds, double *, xs, double *, ys,
                       int, ns, int, ktype)
{
  int i, j;
  double wsum, ysum, lwidth, lwt, xmin, xmax;

  if (n < 1) return(1);
  if (width <= 0.0) {
    if (n < 2) return(1);
    for (xmin = xmax = x[0], i = 1; i < n; i++) {
      if (xmin > x[i]) xmin = x[i];
      if (xmax < x[i]) xmax = x[i];
    }
    width = (xmax - xmin) / (1 + log((double) n));
  }

  for (i = 0; i < ns; i++) {
    for (j = 0, wsum = 0.0, ysum = 0.0; j < n; j++) {
      lwidth = (wds != NULL) ? width * wds[j] : width;
      lwt = kernel(xs[i], x[j], lwidth, ktype);
      if (wts != NULL) lwt *= wts[j];
      wsum += lwt;
      if (y != NULL) ysum += lwt * y[j];
    }
    if (y != NULL) ys[i] = (wsum > 0.0) ? ysum / wsum : 0.0;
    else ys[i] = wsum / n;
  }
  return(0);
}

