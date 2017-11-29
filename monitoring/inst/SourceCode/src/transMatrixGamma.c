#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Simpson's method for (c, d] to 0 and (c, d] to (a, b]
double simpsonG(double a, double b, double c, double d, 
                double k, double alpha, double beta){

  double integrand, cprime, sum=0, coef[5] = {1,4,2,4,1};
  int i;

  // Integrals will be evaluated at 5 points (4 partitions)
  for (i=0; i < 5; i++) {

    double j = i;
    cprime = c + j * (d - c) / 4;

    // for (c, d] to (a, b]
    if (b > 0) 
      integrand = pgamma(b - cprime + k, alpha, beta, 1, 0) - 
                  pgamma(a - cprime + k, alpha, beta, 1, 0);
    // for (c, d] to 0
    else
      integrand = pgamma(k - cprime, alpha, beta, 1, 0);

    sum = sum + integrand * coef[i];

  } // for i

  return(sum / 12);

} // end simpsonG


// Returns the elements of the transition matrix for the Gamma CUSUM
// Calculates transition probabilities 
// using Simpson's rule assuming the CUSUM statistic is 
// uniformly distributed over the interval of the state.
// The matrix is returned as a vector that is loaded into an R matrix
// by columns

void transMatrixGamma(int *numPartitions,                   
                      double *alphaI,
                      double *betaI,
                      double *h,
                      double *kI,
                      double *Q)

// *size is the number of partitions + 1
// length(*endpoints) should equal *size
// *ep are the endpoints

{

  int i, j, position;
  double alpha = *alphaI, beta = *betaI, k = *kI, 
    ep[*numPartitions + 1], numP = *numPartitions;

  // Define the endpoints of the partitions
  for (i=0; i <= *numPartitions; i++) {
    double inc = i;
    ep[i] = *h * inc / numP;
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
        Q[position] = simpsonG(ep[j-1], ep[j], ep[i-1], ep[i], k, alpha, beta);

      // From (c, d] to 0
      else if ((i > 0) & (j == 0))
        Q[position] = simpsonG(0, 0, ep[i-1], ep[i], k, alpha, beta);

      // From 0 state to (a, b]
      else if ((i == 0) & (j > 0))
        Q[position] = pgamma(ep[j]   + k, alpha, beta, 1, 0) - 
                      pgamma(ep[j-1] + k, alpha, beta, 1, 0);

      // From 0 state to 0 state
      else 
        Q[position] = pgamma(k, alpha, beta, 1, 0);

      if (Q[position] < -1e-12)
        Rprintf("Warning: Matrix coordinate (%i,%i) has a value of %.10f\n", 
                i+1, j+1, Q[position]);

      if (Q[position] < 0)
        Q[position] = 0;

    } // for i

  } // for j

} // void transMatrixGamma




