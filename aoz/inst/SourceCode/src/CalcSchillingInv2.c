#include <R_ext/Arith.h>
#include <R.h>
#include <Rmath.h>

// Goal is to return the confidence, and to also return the acheived percentage of allowable defectives via a reference.

// Simple function of the Jaech approximation that will get called frequently, it's equal to 1 - Confidence
double QuickCalc(double dn, double dN, double dU)
{
     double dFact = 1.0 - dn / (0.5 * (2.0*dN - dU + 1.0));
     return pow(dFact, dU);
}

// Simple rounding function (to the 5th decimal place)
double round5(double dX)
{
     return round(dX * 100000.0) / 100000.0;
}

// A max function
double max(double dX, double dY)
{
     if (dX > dY) {
          return dX;
      }
      return dY;
}



//double CalcSchillingInv2(double dDefective, int iTotal, int iSamples)
void CalcSchillingInv2(double *pdDefective, int *piTotal, int *piSamples, 
                       double *pdConf, double *pdAchievedAccept, int *iUseAchieved) // p is for pointer
{

  double dDefective = *pdDefective;
  int iTotal = *piTotal, iSamples = *piSamples;

  double dTotal = iTotal;
  double dSamples = iSamples;


    // dDefective is a percentage, not a fraction.

	// Define the two variables that will be returned (or referenced) at the end.
	double dConf, dAchievedAccept;

	// If we've sampled everything
	if (iTotal == iSamples) {

		dConf = 1.0;
		dAchievedAccept = 1.0;

                if (dDefective <= 0)
		  *iUseAchieved = 0;
                else
                  *iUseAchieved = 1;


	// else if % Acceptable is 1
	} else if (dDefective <= 0) {

		dConf = dSamples / dTotal;
		dAchievedAccept = 1.0;
                *iUseAchieved = 0;

	// Otherwise calculate the confidence
	} else {

        // Define convenient variables for use below
       	        double dN = dTotal;
		double dn = dSamples;
		double dPd = dDefective / 100.0;
		double dLambda0 = round5(1.0 - dPd);

		// A target variable for searches and two candiate variables for achieved % Acceptable
		double dTarget = 5e-06, dLambda1 = 0.0, dLambda2 = 0.0;

		// A flag for executing the binary search
		int iDoBinarySearch = 1;

		// Initialize variables for search
		int i;
		double dFindFirstU, di;

		// Search for discrete values, from small u to large, in order to find a limiting
		// value for the binary search
		for (i = 1; i <= iTotal; i++) {

       		        di = i;

			// Break the loop on the first i that results in a test value beloe
			if (QuickCalc(dn, dN, di) < dTarget) {
				dFindFirstU = di;
				break;
			}

			// If we've reached the end of the loop and haven't found the first u because the sample size is small
			if (i == iTotal) {
				iDoBinarySearch = 0;
			}

		}

		// Now perform the binary search for the value of %Acceptable that gives 100% confidence
		if (iDoBinarySearch) {

			double dUlo = dFindFirstU;
			double dUhi = 1.0;
			double dTestU;

			i = 0;

			while ((fabs(dUlo - dUhi) > 1e-10) & (i < 10000)) {

				i++;

				dTestU = (dUlo + dUhi) / 2.0;

				if (QuickCalc(dn, dN, dTestU) < dTarget) {
					dUlo = dTestU;
				} else {
					dUhi = dTestU;
				}

			}

			// Binary search didn't converge
			if (i == 10000) {
   			     error("Binary search did not converge\n");
			}

			double dUsolve = (dUlo + dUhi) / 2.0;

            // Round result to the nearest 5 decimal places (corresponding to VSP's xx.xxx % format
			dLambda1 = round5(1 - dUsolve / dN);

		}

		if (dn >= dLambda0 * dN) {
			dLambda2 = round5(dn / dN);
		}


		if (dLambda0 > dLambda1) {

			if (dn >= dLambda0 * dN) {
				dConf = 1.0;
			} else {
				dConf = round5(1.0 - QuickCalc(dn, dN, max(dN * dPd, 1.0)));
			}

			dAchievedAccept = max(dLambda0, dLambda2);

		} else {

			dConf = 1.0;
			dAchievedAccept = max(dLambda1, dLambda2);

		}


		// This compairison is done with the rounded figures (to 5 decimal places)
		if (dAchievedAccept > dLambda0) {
		  *iUseAchieved = 1;

		  // I guess this 'else' is probably not needed--but will leave it in for clarity.
		} else {
		  
		  *iUseAchieved = 0;
		}

	}


        // Create reference value and return the confidence as a percentage

	//	double &dAchievedDefective = 100 * (1 - dAchievedAccept);

	//	return dConf * 100;

	*pdConf = dConf;
	*pdAchievedAccept = dAchievedAccept;

}
