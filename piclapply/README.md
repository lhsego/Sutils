# piclapply

## Easy Parallel Computing with R on PIC

Landon Sego, 19 October 2017

### Summary:

This package contains a high performance computing (HPC) parallelization of `lapply()` for the PNNL Institutional Computing (PIC) cluster. It parses a list into subsets and submits a separate R job using `lapply()` for each subset. The parallel jobs are instantiated on multiple nodes/cores using a SLURM batch job. The aim is to make it simple for R users to run large, embarrassingly parallel jobs on PIC--all via a single call to the primary function of the package:  `piclapply()`.

### To begin:

Log on to PIC, start up R, and load the package in the usual way:  `library(piclapply)`.  The package contains 3 exported functions:

* **piclapply()**:  This is the primary function that you'll use to launch jobs in parallel in PIC.  It is an analog to `lapply()`, except it has a number of other arguments you can use to control the parallel computing.

* **showAvailableSystems()**:  This function indicates the PIC systems that can be used with the package.

* **killSlurm()**:  A function for killing SLURM jobs.

Each of these functions is well-documented and should be self-explanatory by looking at the associated help files.

### A note to users:

Sometimes SLURM jobs launched by `piclapply()` fail inexplicably--and when they are rerun, with no changes to the code, they run fine. This is a bug or glitch in the SLURM system, and not a failure in the `piclapply` package.  Consequently, if a SLURM job fails, `piclapply()` makes a second attempt to launch it.  If the second attempt ends in failure, an error is thrown. Bottom line: if your job fails for a reason unrelated to your R code, I recommend just starting it again with a new call to `piclapply()`.

### Installation instructions:

The installation of `piclapply` on PIC is currently maintained by Daniel Fortin <daniel.fortin@pnnl.gov>.  Consequently, you shouldn't need to install it yourself.  These instructions are included here for completeness.

1. Install the `Smisc` package on the PIC head node (inside R):
~~~~
# From CRAN
install.packages("Smisc")

# Or from GitHub
install.packages("devtools") # if the 'devtools' package is not installed
devtools::install_github("pnnl/Smisc")
~~~~

2. After cloning the `piclapply` package to you local machine, build it from the command line using:
~~~~
R CMD build piclapply
~~~~

3. Supposing the PIC system is called `olympus`, copy the .tar.gz file to PIC (from the command line) using something like:
~~~~
scp piclapply_X.Y.Z.tar.gz olympus:/people/username/.
~~~~

4. Install it (from the command line) using something like:
~~~~
ssh olympus "R CMD INSTALL piclapply_X.Y.Z.tar.gz"
~~~~

### A note to package maintainer:

To update the package for new PIC systems, the only file that should need changing is [R/systemSpecifics.R](https://stash.pnnl.gov/projects/RPACK/repos/piclapply/browse/R/systemSpecifics.R).
