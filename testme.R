library(mw3dlib)
library(rgl)


#readCompObjCrazyflie <- function(quiet = T) {
  #cfdir <- paste0(system.file("extdata",package = "mw3dlib"),"/")
  #robv <- readCompObj3d("crazyflie",objdir = cfdir,quiet = quiet)
  #return(robv)
#}

jnk <- open3d()
robv <- readCompObjCrazyflie()
renderCompObj3d(robv,exclude = "cf",include = "cf",lax = T,quiet = T,wireframe = T)
addAxesToRgl(50)