library(mw3dlib)
library(rgl)

jnk <- open3d()
robj <- readCompObjCrazyflie()
renderCompObj3d(robj,exclude = "cf",include = "cf",lax = T,quiet = T,wireframe = T)
addAxesToRgl(50)