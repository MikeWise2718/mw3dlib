setwd("d:/local/mw3dlib/mw3dlib/")
require(devtools)
devtools::check()
setwd("d:/local/mw3dlib/")
devtools::document("mw3dlib")
install.packages("mw3dlib",type = "source",repo = NULL,verbose = T)
