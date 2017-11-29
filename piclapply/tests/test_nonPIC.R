# Test script for a non-PIC host.  Meant to be run primarily interactively--which I why I didn't put them into
# a 'testthat' format.
#
# Landon Sego, 17 October 2017

library(piclapply)

try(killSlurm(839))

try(example(piclapply))

# Note:  you may need to change the account name
account.name <- "TeMpSA"

# Run this on a non-PIC host
res.1 <- try(piclapply(a, f1, account.name,
                       needed.objects = c("b1", "b2"),
                       jobName = "example.1",
                       numNodes = 1,
                       partition = "short",
                       check.interval.sec = 1,
                       time.limit.mins = 1,
                       tarball = "none",
                       verbose = TRUE))



