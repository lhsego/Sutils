// Explicit solutions to the linear equations of the Markov Chain for the
// Bernoulli CUSUM based on Marion Reynolds Fortan code

// Initialize variables

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void BernoulliLinearARL(int *m,
                        int *t,
                        int *c,
                        int *iprime,
                        int *k,
                        double *p,
                        int *verbose,
                        double *N)

{

 int i;

 double q = 1 - *p;
 double a = *p * pow(q,(*m-1)), b = 1 - pow(q,(*m-1));

 if (*iprime == 0) {

   double Tc = b - (a * (*c - 1));
   //   Rprintf("Tc = %f\n",Tc);
   N[*c-1] = (1 + Tc) / (*p * Tc);

   //   Rprintf("In first if statement, N[c] = %f\n", N[*c-1]);

 }

 else{

   int ip, j;
   double T[*iprime+2][*m+1], sum1, sum2, c1, c2, c3, c4, M = *m;

   for (i=0; i <= (*m-1); i++) {
     T[0][i+1] = 1;
     T[1][i+1] = b - (a * i);
   }

   T[0][0] = 0;
   T[0][1] = 0;
   T[1][0] = b;

   for (ip=2; ip <= (*iprime+1); ip++){
     T[ip][0] = T[ip-1][*m];
     sum1 = 0;
     for (i=0; i <= (*m-1); i++) {
       sum1 = sum1 + T[ip-1][i+1];
       T[ip][i+1] = T[ip-1][*m] - (a * sum1);
     }
   }


   //   for (i=0; i < (*iprime+2); i++) {
   //     for (j=0; j < (*m+1); j++) {
   //       Rprintf("T[%i][%i]=%f\n",i,j,T[i][j]);
   //     }
   //   }

   // c1
   if (*k == 0) c1 = 0;
   else {
     sum1 = 0;
     for (i=0; i <= (*k-1); i++) sum1 = sum1 + pow(q,i) * T[*iprime][*k-i];
     c1 = 2*sum1;
   }

   // c2
   if (*iprime <= 1) c2 = 0;
   else {
     sum1 = 0;
     for (i=0; i <= (*m-1); i++) {
       sum2 = 0;
       for (j=0; j <= (*iprime-2); j++) {
            sum2 = sum2 + pow(q,(M * j)) * T[*iprime-1-j][*m-i];
            // Rprintf("q = %f, M = %f, j=%i, Mj = %f, q^(Mj) = %f\n",q,M,j,M*j,pow(q,M*j));
       }
       // Rprintf("sum2 = %f\n", sum2);
       sum1 = sum1 + pow(q,i) * sum2;
       // Rprintf("i = %i, q^i = %f, sum1 = %f\n", i, pow(q,i), sum1);
     }
     // Rprintf("q^k = %f\n", pow(q,*k));
     c2 = pow(q,*k) * sum1;
   }


   // c3
   if ((*k == (*m-1)) | (*iprime == 0)) c3 = 0;
   else {
     sum1 = 0;
     for (i=0; i <= (*m-1-*k-1); i++) sum1 = sum1 + pow(q,i)*T[*iprime-1][*m-i];
     c3 = pow(q,*k) * sum1;
   }

   // c4
   c4 = (pow(q, (M * (*iprime-1) + *k)) - T[*iprime][*k+1] + T[*iprime+1][*k]) / *p;

   N[*c-1] = (c1 + c2 + c3 + c4) / T[*iprime+1][*k];

   if (*verbose)
     Rprintf("N[c] = %7.5f  c1 = %7.5f c2 = %7.5f c3 = %7.5f, c4 = %7.5f\n",
             N[*c-1], c1, c2, c3, c4);

  }

  // Calculate remaining values of N[i]

  for (i=(*c+1); i <= *t; i++) N[i-1] = (1 / *p) * (1 - pow(q, i-*c)) + pow(q, (i-*c)) * N[*c-1];

  for (i=(*c-1); i >= 2; i = i-1) N[i-1] = (1 / q) * (N[i] - *p * N[i-1+*m] - 1);

  N[0] = (1 + *p * N[*m-1]) / *p;

} // void linearARL
