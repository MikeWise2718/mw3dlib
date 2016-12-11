addAxesToRgl <- function(len = 1,s = NULL,r = NULL,t = NULL,tit = "",charexp = 1) {
  # Dislplay 3-legged axis (X,Y,Z)
  # potentially scaled, rotated and translated

  getHeadToTailPoints <- function(x,y,z) {
    m <- matrix(c(x,y,z),2,3)
    return(m)
  }
  u <- c(0,len)
  v <- c(0,0)
  w <- c(0,0)

  xax <- getHeadToTailPoints(u,v,w)
  yax <- getHeadToTailPoints(w,u,v)
  zax <- getHeadToTailPoints(v,w,u)

  if (!is.null(s)) {
    xax[1,] <- xax[1,] %*% s
    xax[2,] <- xax[2,] %*% s
    yax[1,] <- yax[1,] %*% s
    yax[2,] <- yax[2,] %*% s
    zax[1,] <- zax[1,] %*% s
    zax[2,] <- zax[2,] %*% s
  }
  if (!is.null(r)) {
    xax <- xax %*% r
    yax <- yax %*% r
    zax <- zax %*% r
  }
  if (!is.null(t)) {
    xax[1,] <- xax[1,] + t
    xax[2,] <- xax[2,] + t
    yax[1,] <- yax[1,] + t
    yax[2,] <- yax[2,] + t
    zax[1,] <- zax[1,] + t
    zax[2,] <- zax[2,] + t
  }

  lines3d( xax,color = c("red"))
  lines3d( yax,color = c("green"))
  lines3d( zax,color = c("blue"))

  text3d(xax[2,],text="X",color = c("red"),cex = charexp)
  text3d(yax[2,],text="Y",color = c("green"),cex = charexp)
  text3d(zax[2,],text="Z",color = c("blue"),cex = charexp)

}
