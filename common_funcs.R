#remove zero columns
removeColsAllZeros = function(ddt) {
      m <- as.matrix(ddt)
      # isNumericColList <- lapply(1:ncol(m), function(ii,mm){is.numeric(mm[,ii])}, mm=m)
      # indexNonNumericCols <- which(!unlist(isNumericColList))
      mnz <- m[, colSums(abs(m),na.rm = TRUE) != 0]
      return(mnz)
} 