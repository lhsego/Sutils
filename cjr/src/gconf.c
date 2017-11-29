#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Simulate the confidence for GCJR

void gconf(int *ireps,
           int *in1,
           int *inh,
           int *in2,
           int *iN,
           double *dbeta,
           double *drho,
           double *dp,
           int *inUnacceptable,
           double *dconf) {


  int i, iK=0, iV, iW, iZ;
  
  // Make necessary doubles:
  double dn1 = *in1, dn2 = *in2, dV, dW, dtheta_h, dtheta_l, dreps = *ireps, dK;

  // Get the minimum of n1, nh and assign it to nb
  int inb = *in1;
 
  if (inb > *inh)
    inb = *inh;
 
  GetRNGstate();

  for (i=0; i < *ireps; i++) {
   
    // Draw V from Binomial(min(n1,nh), p), the number of judmental samples correctly chosen from high risk cells
    iV = rbinom(inb, *dp);

    // Draw W from Hyper(nh-V, (N-nh)-(n1-V), n2), the number of random samples chosen from high-risk cells
    iW = rhyper(*inh-iV, *iN - *inh - *in1 + iV, *in2);

    // Draw the posterior theta_h from Beta(1, ((n1 - V) + (n2 - W))/rho + V + W + beta)
    // Draw the posterior theta_l from Beta(1, ((n1 - V) + (n2 - W)) + rho * (1 + V + W + beta) - 1)
    dV = iV; 
    dW = iW;
    
    dtheta_h = rbeta(1, (dn1 - dV + dn2 - dW) / *drho + dV + dW + *dbeta);
    dtheta_l = rbeta(1, (dn1 - dV + dn2 - dW) + *drho * (1 + dV + dW + *dbeta) - 1);

    // Draw the number of unacceptable cells among the unsampled high-risk cells
    iZ = rbinom(*inh - iV - iW, dtheta_h);

    // Draw the number of unacceptable cells among the unsampled low-risk cells
    iZ += rbinom(*iN - *inh - *in1 + iV - *in2 + iW, dtheta_l);

    if (iZ <= *inUnacceptable)
      iK ++;

  } // for

  PutRNGstate();

  dK = iK;

  *dconf = dK / dreps;

} // conf


/* // Test using the hypergeometric distribution */

/* void testH(int *nr, int *nb, int *n, int *out) { */
 
/*   GetRNGstate(); */

/*   *out = rhyper(*nr, *nb, *n); */

/*   PutRNGstate(); */


/* } // testH */


/* // Test using the binomial distribution */

/* void testB(int *n, double *p, int *out) { */
 
/*   GetRNGstate(); */

/*   *out = rbinom(*n, *p); */

/*   PutRNGstate(); */


/* } // testH */


