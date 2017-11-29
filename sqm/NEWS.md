Version 0.1.1,  2015-07-03
--------------------------------------------------------------------------------
	
DEPRECATED

* All functions that were previously created for facilitating web service calls
  have been removed and included in a separate package, SQMWS (WS for 'web service')
* All economic cost functions have been removed, as they were not fully developed
  and were not sufficiently general 

Version 0.1.0,  2013-09-30
--------------------------------------------------------------------------------

NEW FEATURES

* Numerous test scripts were added to 'inst/tests' that perform verification 
  testing on primarily the fidelity (accuracy) module.  Each test script 
  corresponds to a single function, with a naming convention that is 
  self-explanatory.
 
Version 0.0.9,  2013-09-12
--------------------------------------------------------------------------------

NEW FEATURES

* Added a plot method for maUtil(), which makes plots of each single attribute 
  utility function

MODIFIED

* saUtil() and expUtil() have been modified so that if the certainty equivalent
  is provided, then theta is calculated and added to the attributes.  Likewise,
  if theta is provided, the certainty equivalent is calculated and added to the
  attributes.


Version 0.0.8,  2013-05-29
--------------------------------------------------------------------------------

NEW FEATURES

* Added fidelityDiscrete(), a fidelity method for calculating discrete loss or
  discrete utility for a classifier.


Version 0.0.7,  2013-05-21
--------------------------------------------------------------------------------

NEW FEATURES

* Added UtilApp_to_SQM() and SQM_to_UtilApp(), which provide interfaces for the 
  to the Java utility elicitation app developed by Ellen Porter.


Version 0.0.6,  2013-05-01
--------------------------------------------------------------------------------

NEW FEATURES

* Addition of fidelityMethods(), which are the components of the single attribute
  utility functions of fidelity.  They will employ metrics similar to those
  coded in the Accuracy and Scoring components of the package--except they will not
  aggretate across observational units.


Version 0.0.5,  2013-04-15
--------------------------------------------------------------------------------

NEW FEATURES

* The previously existing methods and functions for utility were deprecated and
  replaced with single attribute utility function methods, saUtilmethods(), multi-
  attribute utility function methods, maUtilMethods(), and function to create
  or calculate these utility functions:  saUtil() and maUtil().  
* Added calcExpectedUtil(), a function for calculating the expected utility that
  applies the single and multiattribute utility functions.
* A bootstrapping capability when calculating quantities such as the
  expected utility and log scores:  bootStrap()
* pareto(), a function for identifying the pareto frontier.

DEPRECATED

* All previous risk and utility functionality have been replaced with the new
  utility and expected utility functionality discussed above.


Version 0.0.4,  2012-10-01
--------------------------------------------------------------------------------

NEW FEATURES

* Proper scoring methods were added:  log score, information (negative entropy), 
  Brier score, and sharpness.  This included raw scoring functions (via calcScore()),
  and wrapper functions, i.e. afScore().


Version 0.0.3, 2012-03-01
--------------------------------------------------------------------------------

NEW FEATURES

* The initial phase of the Risk component has been implemented. The
  documentation for much of the implementation details is currently minimal.
  Improvements are planned for the next release. See the Risk section below for
  more details on the implementation details.

RISK

* `afRisk`: This function is the primary end-user function to calculate risk for
  a set of events, their corresponding probabilties, and the consequences
  associated with a terminating/resultant scenario. The function interfaces with
  a set of user-inputted files and, if specified, outputs the calculated risks
  for each signature to a CSV file.

* `calcRisk`: This function is the workhorse function that actually performs the
  risk for a set of events corresponding to a given set of signatures. This
  function can be called by the end-user to bypass the file input/output
  interface features in the 'afRisk' function.

* `readRisk`: This function reads the signature events along with their
  respective probabilities to calculate the corresponding risks for each
  signature. The events are read from a given set of CSV files.

* `writeRisk`: This function exports the calculated risks for each signature to
  the specified CSV file.

Version 0.0.2,  2012-02-09
--------------------------------------------------------------------------------

* Added package description which is accessible with help('SQM')
* Slight modification to the package DESCRIPTION.


Version 0.0.1,  2012-02-02
--------------------------------------------------------------------------------

NEW FEATURES

* The Accuracy component has been revamped to make its usage more
  straightforward. See the ACCURACY section below for more details.

* The initial phase of the Cost component has been implemented. The documentation
  for much of the implementation details is currently minimal. Improvements are
  planned for the next release. See the COST section below for more details on
  the implementation details.

* The initial phase of the Utility component has been implemented. The
  documentation for much of the implementation details is currently minimal.
  Improvements are planned for the next release. See the COST section below for
  more details on the implementation details.

ACCURACY

* A vignette has been added to the SQM package to demonstrate a use case for the
  accuracy component of the SQM package. The vignette is a work in progress and
  should be read with care. To view the vignette, type the following command at
  the R prompt: vignette("vignette-accuracy", "SQM"). Separate vignettes are
  planned for each component (e.g. cost).

* The primary end-user function to calculate accuracy measures for a set of
  signatures is provided in the `afAccuracy` function. The `af*` convention
  will be used for Analytic Framework end-user functions.

* The documentation for the Accuracy component of the SQM package has been
  extended thoroughly. In particular, the documentation for the `afAccuracy`,
  `calcAccuracy`, and `confusion` functions has been revamped, which should
  improve the understanding of the accuracy component.

* Working examples for several functions within the Accuracy component have been
  added and can be accessed with the built-in `example` function. For instance,
  typing `example(afAccuracy)` at the R prompt will demonstrate some simple use-
  cases for the `afAccuracy` function. See `?example` for more details about
  package examples.

* Some sanity checks for inputs are present but minimal. More thorough checks
  are planned for the next release.

COST

* `calcCosts`: This function has been added to calculate the costs for a given
  set of assets, intangible assets, laborers, consumables, and fixed costs for a
  set of signatures. Each of these costs are calculated with its own internal
  functions; to view these, at the R prompt type: help(package = "SQM").
  Currently, `calcCosts` is the primary end-user function to calculate costs.

* `readCosts`: This function reads the signatures from a set of CSV files and
  returns a list of data.frames to be used with the `calcCosts` function.

* `writeCosts`: This function exports the calculated costs for each signature to
  the specified CSV file.

UTILITY

* `calcUtility`: This function has been added to calculate the utility for each
  given signature. A _univariate exponential utility function_ is fit to each
  variable across all signatures based on subject-matter expert (SME) input. This
  input is provided in the `varUtility` data.frame.  Currently, `calcUtility` is
  the primary end-user function to calculate utility.

* `readUtility`: This function reads the signatures and utility fitting arguments
  from a given set of CSV files.

* `utility`: This function fits a univariate utility function to the user-
  specified utility values for a given variable. A numerical method is required
  to calculate the fit and is performed with the built-in R function `uniroot`.

* `plotUtility`: This function plots a fitted exponential utility function for
  the user. It provides meaningful feedback to the user about the specified
  utility fitting arguments. By default, the plot is open in a separate window
  (i.e. graphics device), but the plot can instead be saved to one of the
  following formats: PS, PDF, JPG, TIF, PNG, and BMP. The file format is
  determined by the file extension given in the filename.

* `exponential_utility`: Currently, this is the only univariate function
  available in the SQM package, although numerous others exist. In a future
  release, it is likely that several of the major utility functions will be
  provided.

MINOR CHANGES

* At Adam Wynne's recommendation, better descriptive end-user input parameters
  have been added. For example, in the `confusion` function, the parameters
  `truth` and `class` have been changed to `truthClass` and `predictedClass`,
  respectively.

* The code style throughout the SQM package is now uniform. Specifically,
  variable and function names follow the camelCase format rather than a mix of
  camelCase and snake_case. If there has been an oversight in this matter, blame
  Landon Sego. Actually, notify John Ramey about an inconsistency to have this
  changed.
