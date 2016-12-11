#' Read in the Crazyflie files
#' 
#' @param quiet A boolean value that supresses reading feedback
#' @export readCompObjCrazyflie
#' @examples
#' robj <- readCompObjCrazyflie()
readCompObjCrazyflie <- function(quiet = T) {
  cfdir <- paste0(system.file("extdata",package = "mw3dlib"),"/")
  robj <- readCompObj3d("crazyflie",objdir = cfdir,quiet = quiet)
  return(robj)
}