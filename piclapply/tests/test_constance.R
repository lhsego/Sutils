# Test script for constance.  Meant to be run primarily interactively--which I why I didn't put them into
# a 'testthat' format.
#
# Landon Sego, 19 October 2017

library(piclapply, lib.loc = "~/R")

########################################
# Test 1: Show the host and nodes
########################################
showAvailableSystems()
showAvailableSystems(details = TRUE)

# Expecting error
try(example(piclapply))

# Note:  you may need to change the account name
account <- account.name <- "TeMpSA"

# Run the examples
example(piclapply)

########################################
#  Verify tests work on each partition
########################################

# Create a function that calculates the mean of a normal variate
mean.tmp <- function(list.element, sigma = 1) {
  set.seed(list.element)
  mean(rnorm(500, mean = list.element, sd = sigma))
}

# Create a list of means (and seeds) to operate over
aList <- as.list(0:10000)

# Calculate it without piclapply
res <- lapply(aList, mean.tmp, sigma = 0.5)

# Test over the different partitions
nodeTest <- function(partition, verbose = FALSE) {

    res.4 <- piclapply(aList, mean.tmp, account.name, sigma = 0.5,
                       jobName = "example.2",
                       time.limit.mins = 5,
                       numNodes = 2,
                       partition = partition,
                       parseJob.args = list(collate = TRUE, text.to.eval = TRUE),
                       check.interval.sec = 1,
                       tarball = "none",
                       verbose = verbose)
}

slurm <- nodeTest("slurm", TRUE)
short <- nodeTest("short")
gpu <- nodeTest("gpu")
phi <- nodeTest("phi")
long <- nodeTest("long")

# These should all be TRUE
all.equal(res, slurm)
all.equal(res, short)
all.equal(res, gpu)
all.equal(res, phi)
all.equal(res, long)


########################################
# Test 4:  use of primitives
########################################
aList <- Smisc::parseJob(1000, 60)

# Randomly insert some NA's
aList <- lapply(aList, function(x) {x[sample(1:length(x), sample(0:3, 1))] <- NA; return(x)})

res.6 <- piclapply(aList, sum, account.name,
                   na.rm = TRUE,
                   numNodes = 1,
                   partition = "short",
                   check.interval.sec = 5,
                   time.limit.mins = 5,
                   tarball = "none",
                   verbose = FALSE)

# Should be TRUE
all.equal(lapply(aList, sum, na.rm = TRUE), res.6)

# Wihout removing NA's
res.7 <- piclapply(aList, sum, account.name,
                   numNodes = 2,
                   partition = "short",
                   check.interval.sec = 1,
                   time.limit.mins = 5,
                   tarball = "none",
                   verbose = FALSE)

# Should be TRUE
all.equal(lapply(aList, sum), res.7)


########################################
# Checking errors on argument inputs
########################################

# wrong partition
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = "example.1",
                      numNodes = 1,
                      partition = "shorty",
                      check.interval.sec = 1,
                      time.limit.mins = 1,
                      tarball = "none"))

# Should be TRUE
grepl("'partition' must be one of", res.1)

# remove.working.dir
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = "example.1",
                      numNodes = 1,
                      partition = "short",
                      check.interval.sec = 1,
                      time.limit.mins = 1,
                      tarball = "none",
                      remove.working.dir = "bad",
                      verbose = TRUE))

# Should be TRUE
grepl("'remove.working.dir' should be TRUE or FALSE", res.1)


# multiple strings for tarball
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = "example.1",
                      numNodes = 1,
                      partition = "short",
                      check.interval.sec = 1,
                      time.limit.mins = 5,
                      tarball = c("none", "noe1"),
                      verbose = FALSE))

# Should be TRUE
grepl("should be a character string or NULL", res.1)


# jobName
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = FALSE,
                      numNodes = 1,
                      partition = "short",
                      check.interval.sec = 1,
                      time.limit.mins = 5,
                      tarball = "none"))

# Should be TRUE
grepl("'jobName' should be a character string", res.1)


# packages
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = "new",
                      numNodes = 1,
                      packages = 10,
                      partition = "short",
                      check.interval.sec = 1,
                      time.limit.mins = 5,
                      tarball = "none"))

# Should be TRUE
grepl("'packages' should be a character string", res.1)


# Non existent package
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = "new",
                      numNodes = 1,
                      packages = c("MASS", "thisIsNotaPackage"),
                      partition = "short",
                      check.interval.sec = 1,
                      time.limit.mins = 5,
                      tarball = "none"))

# Should be TRUE
grepl("The following requested", res.1)


# Non existent packages
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = "new",
                      numNodes = 1,
                      packages = c("MASS", "thisisnot1", "thisisnot2"),
                      partition = "short",
                      check.interval.sec = 1,
                      time.limit.mins = 5,
                      tarball = "none"))

# Should be TRUE
grepl("The following requested", res.1)


# time.limit.mins
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = "new",
                      numNodes = 1,
                      partition = "short",
                      check.interval.sec = 1,
                      time.limit.mins = 5.2,
                      tarball = "none",
                      verbose = FALSE))

# Should be TRUE
grepl("'time.limit.mins' must be a positive integer", res.1)

# bad needed objects
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "notHere"),
                      jobName = "new",
                      numNodes = 1,
                      partition = "short",
                      check.interval.sec = 1,
                      time.limit.mins = 5,
                      tarball = "none",
                      verbose = FALSE))

# Should be TRUE
grepl("needed.object 'notHere' does not exist", res.1)


# bad parseJob arguments
res.1 <- try(piclapply(a, f1, account.name,
                      needed.objects = c("b1", "b2"),
                      jobName = "new",
                      numNodes = 1,
                      partition = "short",
                      check.interval.sec = 1,
                      parseJob.args = list(not1 = 7, collate = TRUE, not2 = "this"),
                      time.limit.mins = 5,
                      tarball = "none",
                      verbose = FALSE))

# Should be TRUE
grepl("The following are not acceptable", res.1)


# Stick in some reserved objects
process.id <- 12
X <- 22
res.1 <- try(piclapply(a, f1, account.name,
                       needed.objects = c("b1", "b2", "process.id", "X"),
                       jobName = "new",
                       numNodes = 1,
                       partition = "short",
                       check.interval.sec = 1,
                       time.limit.mins = 1,
                       tarball = "none",
                       verbose = FALSE))

# Should be TRUE
grepl("reserved objects", res.1)


# Bad additional arguments to fun
f2 <- function(x, b1 = 10, b2 = 12) mean(x) + max(b1) - min(b2)
res.1 <- try(piclapply(a, f2, b8 = 13, b7 = "bad", account.name,
                       jobName = "new",
                       numNodes = 1,
                       partition = "short",
                       check.interval.sec = 1,
                       time.limit.mins = 1,
                       tarball = "none",
                       verbose = FALSE))

# Should be TRUE
grepl("optional arguments", res.1)

# Bad additional arguments to fun
res.1 <- try(piclapply(a, f2, b1 = 6, b20 = 13,
                       account.name,
                       jobName = "new",
                       numNodes = 1,
                       partition = "short",
                       check.interval.sec = 1,
                       time.limit.mins = 1,
                       tarball = "none",
                       verbose = FALSE))

# Should be TRUE
grepl("optional arguments", res.1)

# Bad account name
res.1 <- try(piclapply(a, f1, "badName",
                       needed.objects = c("b1", "b2"),
                       jobName = "example.1",
                       numNodes = 1,
                       partition = "short",
                       check.interval.sec = 1,
                       time.limit.mins = 1,
                       tarball = "none",
                       verbose = TRUE))

# Check the custom tarball file
res.1 <- piclapply(a, f1, account.name,
                   needed.objects = c("b1", "b2"),
                   jobName = "example.1",
                   numNodes = 1,
                   partition = "short",
                   check.interval.sec = 1,
                   time.limit.mins = 1,
                   tarball = "tmpTar.tar.gz",
                   verbose = FALSE)

# Should be true
file.exists("tmpTar.tar.gz")
unlink("tmpTar.tar.gz")

# Verify default tarball file
res.1 <- piclapply(a, f1, account.name,
                   needed.objects = c("b1", "b2"),
                   jobName = "example.5",
                   numNodes = 1,
                   partition = "short",
                   check.interval.sec = 1,
                   time.limit.mins = 1,
                   tmp.file.list = "tmpF",
                   verbose = FALSE)

print(tmpF)

# Should be TRUE
file.exists(tmpF$tarball)
unlink(tmpF$tarball)


# Investigate working directory and email
res.1 <- piclapply(a, f1, account.name,
                   needed.objects = c("b1", "b2"),
                   jobName = "example.1",
                   numNodes = 1,
                   partition = "short",
                   check.interval.sec = 1,
                   time.limit.mins = 1,
                   tarball = "none",
                   remove.working.dir = FALSE,
                   verbose = FALSE,
                   tmp.file.list = "tmpF1",
                   email.notification = "landon.sego@pnnl.gov")


# Show working folders and README file
dir(tmpF1$wkdir)
dir(file.path(tmpF1$wkdir, "logs"))
dir(file.path(tmpF1$wkdir, "out"))
Smisc::more(file.path(tmpF1$wkdir, "README.txt"))
unlink(tmpF1$wkdir)
