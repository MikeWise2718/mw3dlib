library(utils)
require(mw3dlib)
detach(package:mw3dlib)
remove.packages(c("mw3dlib"))

#curwd <- getwd()
#setwd("f:/88acres/5-packages/")
install.packages("mw3dlib",type="source",repo=NULL,verbose=T)
#setwd(curwd)
