#' Reads the signature costs from given files.
#'
#' For a given file, this function reads in the signatures for which we will
#' calculate the corresponding costs for each signature.
#'
#' The first column should contain the signature ID.
#'
#' We require that the input file is in the CSV file format (i.e. the file
#' delimiter is a comma).
#'
#' By default, each filename argument is \code{NULL}. If a filename remains
#' \code{NULL}, no file is read, and the named element in the returned list
#' is set to \code{NULL}.
#'
#' @export
#' @param assetsFile string with the file name corresponding to the assets for
#' each signature.
#' @param intangibleAssetsFile string with the file name corresponding to the
#' intangible assets for each signature.
#' @param laborCostsFile string with the file name corresponding to the labor
#' costs for laborers for each signature.
#' @param variableCostsFile string with the file name corresponding to the
#' variable costs for the consumables for each signature.
#' @param fixedCostsFile string with the file name corresponding to the
#' variable costs for the consumables for each signature.
#' @return a list with five data.frame corresponding to the inputted costs for
#' each signature.
readCosts <- function(assetsFile = NULL, intangibleAssetsFile = NULL,
                      laborCostsFile = NULL, variableCostsFile = NULL,
                      fixedCostsFile = NULL) {
  signatureAssets <- NULL
  if (!is.null(assetsFile)) {
    assetsColNames <- c("signatureID", "capital", "discountRate", "yearsLife",
                        "qtyProcessRate")
    signatureAssets <- read.csv(assetsFile, header = FALSE,
                                col.names = assetsColNames,
                                stringsAsFactors = FALSE)
  }

  signatureIntangibleAssets <- NULL
  if (!is.null(intangibleAssetsFile)) {
    intangibleAssetsColNames <- c("signatureID", "cost", "discountRate",
                                  "yearsLife", "qtyProcessRate")
    signatureIntangibleAssets <- read.csv(intangibleAssetsFile, header = FALSE,
                                          col.names = intangibleAssetsColNames,
                                          stringsAsFactors = FALSE)
  }

  signatureLaborers <- NULL
  if (!is.null(laborCostsFile)) {
    laborColNames <- c("signatureID", "capital", "burdenedLaborRate",
                       "qtyProcessRate")
    signatureLaborers <- read.csv(laborCostsFile, header = FALSE,
                                  col.names = laborColNames,
                                  stringsAsFactors = FALSE)
  }

  signatureConsumables <- NULL
  if (!is.null(variableCostsFile)) {
    consumableColNames <- c("signatureID", "price", "quantity")
    signatureConsumables <- read.csv(variableCostsFile, header = FALSE,
                                     col.names = consumableColNames,
                                     stringsAsFactors = FALSE)
  }

  signatureFixedCosts <- NULL
  if (!is.null(fixedCostsFile)) {
    fixedCostsColNames <- c("signatureID", "fixedCosts", "qtyProcessRate")
    signatureFixedCosts <- read.csv(fixedCostsFile, header = FALSE,
                                    col.names = consumableColNames,
                                    stringsAsFactors = FALSE)
  }
  
  list(signatureAssets = signatureAssets,
       signatureIntangibleAssets = signatureIntangibleAssets,
       signatureLaborers = signatureLaborers,
       signatureConsumables = signatureConsumables,
       signatureFixedCosts = signatureFixedCosts
      )
}
