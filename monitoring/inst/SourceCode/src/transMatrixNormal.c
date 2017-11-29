#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Something is wrong with this:

//  Something is wrong with transMatrixNormal, I think.  Note how results don't agree with
//  pg 415 of Montgomery (4th ed) or simulations when sigma > 1

// Simpson's method for (c, d] to 0 and (c, d] to (a, b]
double simpsonN(double a, double b, double c, double d, 
                double k, double mu, double sigma){

  double integrand, cprime, i_double, sum = 0, coef[5] = {1,4,2,4,1};
  int i;

  // Integrals will be evaluated at 5 points (4 partitions)
  for (i=0; i < 5; i++) {

    i_double = i;
    cprime = c + i_double * (d - c) / 4;

    // for (c, d] to (a, b]
    // arguments for pnorm are:
    // double pnorm(double x, double mu, double sigma, int lower_tail, int give_log);
    if (b > 0) 
      integrand = pnorm(b - cprime + k, mu, sigma, 1, 0) - 
                  pnorm(a - cprime + k, mu, sigma, 1, 0);

    // for (c, d] to 0
    else
      integrand = pnorm(k - cprime, mu, sigma, 1, 0);

    sum += integrand * coef[i];

  } // for i

  return(sum / 12);

} // end simpsonN

// Returns the elements of the transition matrix for a standardized, 1-sided, normal CUSUM chart
// Calculates transition probabilities 
// using Simpson's rule assuming the CUSUM statistic is 
// uniformly distributed over the interval of the state.
// The matrix is returned as a vector that is loaded into an R matrix
// by columns

void transMatrixNormal(int *numPartitions,                   
                       double *h,
                       double *k_input,
                       double *mu_input,
                       double *sigma_input,
                       double *Q)

// *size is the number of partitions + 1
// length(*endpoints) should equal *size
// *ep are the endpoints

{

  // Index variables
  int i, j, position;

  // Set pointers to actual variables
  double k = *k_input, mu = *mu_input, sigma = *sigma_input, ep[*numPartitions + 1], numP = *numPartitions, increment;

  // Define the endpoints of the partitions
  for (i=0; i <= *numPartitions; i++) {
    increment = i;
    ep[i] = *h * increment / numP;
  }

  // Calculating P(in state j | were in state i)

  // loop across the columns
  for (j=0; j <= *numPartitions; j++) {

    // loop down the rows
    for (i=0; i <= *numPartitions; i++) {

      // index for the Q vector that will be loaded columnwise
      // into a matrix
      position = i + (*numPartitions + 1) * j;

      // From (c, d] to (a, b]
      if ((i > 0) & (j > 0)) 
        Q[position] = simpsonN(ep[j-1], ep[j], ep[i-1], ep[i], k, mu, sigma);

      // From (c, d] to 0
      else if ((i > 0) & (j == 0))
        Q[position] = simpsonN(-1, -1, ep[i-1], ep[i], k, mu, sigma);

      // From 0 state to (a, b]
      else if ((i == 0) & (j > 0))
        Q[position] = pnorm(ep[j]   + k, mu, sigma, 1, 0) - 
                      pnorm(ep[j-1] + k, mu, sigma, 1, 0);

      // From 0 state to 0 state
      else 
        Q[position] = pnorm(k, mu, sigma, 1, 0);

      if (Q[position] < -1e-12)
        Rprintf("Warning: Matrix coordinate (%i,%i) has a value of %.10f\n", 
                i+1, j+1, Q[position]);
       
      // Clean up any machine error
      if (Q[position] < 0)
        Q[position] = 0;

    } // for i

  } // for j

} // void transMatrixGamma

