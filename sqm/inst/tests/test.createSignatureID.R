#  Test code for createSignatureID
#  Landon Sego, 2013-11-23

test.createSignatureID <- function() {

  # Test the example case
  data(exampleSignatures)
  y <- createSignatureID(exampleSignatures, "signatureID")
  if (!all(colnames(attributes(y)$groupVarMapping) ==
           c("signatureID", "signatureID.original")))
    stop("Column name mapping not correct when 'groupVars == sigIDname'")
  
  # Base dataset
  x <- expand.grid(a = 1:5, b = letters[1:2], c = c("red", "blue"), d = c(TRUE,FALSE))
  x <- rbind(x, x)
  x$b <- as.character(x$b)
  x$z <- rnorm(NROW(x))
  
  # Test single indicator
  y <- createSignatureID(x, "d", sigIDname = "new")
  stopifnot(all.equal(as.character(y$d), y$new))

  # Test the renaming of columns
  y <- createSignatureID(x, "d", sigIDname = "c")
  if (!("c.original" %in% colnames(y)))
    stop("A column named 'c.original' should be present but is not in present in 'df' ",
         "when 'createSignatureID' was called")
  
  # Test using a separator
  y <- createSignatureID(x, letters[1:4], separator = "&")
  c1 <- nchar(y$signatureID)
  yid <- gsub("&", "", y$signatureID, fixed = TRUE)
  c2 <- nchar(yid)
  if (!all(c1 - c2 == 3))
    stop("Using a custom separator in 'createSignatureID' did not perform as expected")
  
  # Test a situation when the default separator of '.' doesn't work
  x <- expand.grid(a = 1:3, b = c("a", ".", "_", "-+", ":"), stringsAsFactors = FALSE)
  y <- createSignatureID(x, c("a", "b"), sigIDname = "z")
  if (sum(grepl("*", y$z)) != NROW(y))
    stop("Identifying the separator not performing as expected in 'createSignatureID', msg 2")
  
  # Test a situation when the default separator of '.' doesn't work
  x <- expand.grid(a = 1:3, b = c("a", "."), stringsAsFactors = FALSE)
  y <- createSignatureID(x, c("a", "b"), sigIDname = "z")
  if (sum(grepl("_", y$z)) != NROW(y))
    stop("Identifying the separator not performing as expected in 'createSignatureID', msg 1")
  
  # Verify when reconstructing the terms
  x <- expand.grid(a = 1:5, b = letters[1:2], c = c("red", "blue"), d = c(TRUE,FALSE))
  x <- rbind(x, x)
  x$b <- as.character(x$b)
  
  y <- createSignatureID(x, letters[1:4], sigIDname = "z", separator = "=")
  ys <- list2df(strsplit(y$z, "=", fixed = TRUE), col.names = letters[1:4])
  ys$c <- as.factor(ys$c)
  ys$d <- as.logical(ys$d)
  if(!dframeEquiv(ys, x, verbose = FALSE)$equiv)
    stop("Splitting the 'signatureID' in 'createSignatureID' does not give the original ",
         "'groupVars'")
  
} # test.createSignatureID

# Run the test
test.createSignatureID()
