#' Read in a compObj object from files
#' 
#' @param quiet A boolean value that supresses reading feedback
#' @param objnameroot the root object name - this forms part of the filenames
#' @param objdir the root object directory
#' @examples
#' robj <- readCompObj3d("crazyflie")
readCompObj3d <- function(objnameroot,objdir = NULL,quiet = T) {
  # Read a set of files specifying a object with parts, components, points and vertice indexes
  # saved in four csv files
  # Each component belongs to exactly one part
  # points and vertice indices belong to a part, but can only be instanced via a component
  # when instanced they are modified by the component transformation which can scale, transform and/or
  # translate
  # material properties are specfied via part
  # one should probably have override options per component, but that would inflate the format
  # Note that this routine also crunches some of the values for easier consumption later

  # Components
  fnameroot <- objnameroot
  if (!is.null(objdir)) {
    fnameroot <- sprintf("%s/%s",objdir,objnameroot)
    fnameroot <- gsub("//","/",fnameroot)
  }
  fname <- sprintf("%s-components.csv",fnameroot)
  cdf <- read.csv(fname)
  cdf <- cdf[cdf$id > 0,]
  nc <- nrow(cdf)
  cdf$sca <- lapply(1:nc,function(i) c(cdf$sca.x[i],cdf$sca.y[i],cdf$sca.z[i]))
  cdf$trn <- lapply(1:nc,function(i) c(cdf$trn.x[i],cdf$trn.y[i],cdf$trn.z[i]))
  cdf$rot <- lapply(1:nc,function(i) matrix(c( cdf$rot.11[i],cdf$rot.12[i],cdf$rot.13[i],
                                               cdf$rot.21[i],cdf$rot.22[i],cdf$rot.23[i],
                                               cdf$rot.31[i],cdf$rot.32[i],cdf$rot.33[i]),3,3))
  # Parts
  fname <- sprintf("%s-parts.csv",fnameroot)
  pdf <- read.csv(fname)
  pdf$amb <- rgbToStringColor(pdf$amb.r,pdf$amb.g,pdf$amb.b)
  pdf$dif <- rgbToStringColor(pdf$dif.r,pdf$dif.g,pdf$dif.b)
  pdf$spc <- rgbToStringColor(pdf$spc.r,pdf$spc.g,pdf$spc.b)
  pdf$ems <- rgbToStringColor(pdf$ems.r,pdf$ems.g,pdf$ems.b)

  # Points
  fname <- sprintf("%s-points.csv",fnameroot)
  ptdf <- read.csv(fname)

  # VertsIdx
  fname <- sprintf("%s-vertidx.csv",fnameroot)
  vidf <- read.csv(fname)

  if (!quiet) {
    np <- nrow(pdf)
    nc <- nrow(cdf)
    npt <- nrow(ptdf)
    nvi <- nrow(vidf)
    print(sprintf("read from %s  %d parts   %d components   %d points   %d verts",
                  fnameroot,np,nc,npt,nvi))
  }

  rv <- list()
  rv$objname <- objnameroot
  rv$fnameroot <- fnameroot
  rv$cdf <- cdf
  rv$pdf <- pdf
  rv$ptdf <- ptdf
  rv$vidf <- vidf
  return(rv)
}
