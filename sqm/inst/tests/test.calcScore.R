test.calcScore <- function(signatures, methods = scoreMethods(), ...){
  
  stopifnot(is.data.frame(signatures), is.vector(methods), 
            is.character(methods))
  if (!all(c("signatureID", "truthClass") %in% colnames(signatures))) 
    stop("'signatures' must contain columns labled 'signatureID' and 'truthClass'")
  if (sum(grepl("predictionClass", colnames(signatures))) < 
        2) 
  stop("'signatures' must contain at least two columns labeled 'predictionClass' followed by the class name")
  
  scoreMethods <- match.arg(methods, scoreMethods(), several.ok = TRUE)
  
  sig.names <- levels(signatures$signatureID)
  
  # Separate the dataframe into smaller data sets splitting on the signature name
  sep_by_sig <- lapply(1:length(sig.names),function(x){
    ind <- which(signatures$signatureID == sig.names[x])
    signatures[ind,]
  })
  
  score.list <- lapply(1:length(sep_by_sig), function(x){
    inner <- lapply (1:length(methods), function(i){
      h <- paste(methods[i],"(sep_by_sig[[", x, "]])",sep="")
      g <- eval(parse(text=h))
      ind <- which(rowSums(!is.na(g))==ncol(g))
      g[ind,]
    })
    inner
  })
  
  kc <- unlist(score.list)
  nlist <- unique(names(kc))
  
  out <- list()
  for (i in 1:length(nlist)){
    ind <- grepl(nlist[i],names(kc))
    out[[i]] <- kc[ind]
  }
  
  dt <- function(x){t(t(x))};
  
  df <- matrix(nrow=length(nlist))
  for(i in 1:length(out)){
    if (i <= 1){
    df <- cbind(df, dt(unique(out[[i]])))  
    } else {
      df <- cbind(df,dt(unlist(out[[i]])))
    }
  }
  
  rownames(df) <- NULL
  
  df <- as.data.frame(df[,2:ncol(df)])
  df <- df[with(df,order(V1)),]
  rownames(df) <- sort(rownames(df))
  colnames(df) <- nlist
  df$signatureID <- sort(sig.names)
  return(df)
  
}