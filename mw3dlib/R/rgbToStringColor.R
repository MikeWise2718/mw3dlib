rgbToStringColor <- function(rvek,gvek,bvek) {
  # Converts a 3 vector of rgb values (between 0 and 1)
  # to a single vector of color in R character format
  # so rgb = c(1,0,0) would go to #ff0000
  colVekToStringColor <- function(clr) {
    clr <- pmax(0,pmin(clr,1))
    iclr <- round(255 * clr)
    hclr <- sprintf("#%2.2x%2.2x%2.2x",iclr[[1]],iclr[[2]],iclr[[3]])
    return(hclr)
  }
  nv <- length(rvek)
  m <- matrix(c(rvek,gvek,bvek),nv,3) # matrix with row as r,g,b
  l <- lapply(1:nv,function(x) m[x,]) # now unwrap into a list of rgb's
  rgb <- sapply(l,colVekToStringColor)
}
