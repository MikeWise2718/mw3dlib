#' Add axis markers at local coordinate space origin
#' 
#' @param len The length of each axis
#' @param sca scale vector for local coord system (1 coord per axis)
#' @param rot rotation matrix  for local coord system
#' @param trn translation vector for local coord system (1 coord per axis)
#' @param title An optional title
#' @param charexp character expansion factor for axis label
#' @export addAxesToRgl
#' @examples
#' addAxesToRgl()
addAxesToRgl <- function(len = 1,sca = NULL,rot = NULL,trn = NULL,title = NULL,charexp = 1) {
  # Dislplay 3-legged axis (X,Y,Z)
  # potentially scaled, rotated and translated

  getHeadToTailPoints <- function(x,y,z) {
    # utility function
    m <- matrix(c(x,y,z),2,3)
    return(m)
  }
  u <- c(0,len)
  v <- c(0,0)
  w <- c(0,0)

  # get our three axes as 3x2 matrix of two points from head to tail
  xax <- getHeadToTailPoints(u,v,w)
  yax <- getHeadToTailPoints(w,u,v)
  zax <- getHeadToTailPoints(v,w,u)

  if (!is.null(sca)) {
    xax[1,] <- xax[1,] %*% sca
    xax[2,] <- xax[2,] %*% sca
    yax[1,] <- yax[1,] %*% sca
    yax[2,] <- yax[2,] %*% sca
    zax[1,] <- zax[1,] %*% sca
    zax[2,] <- zax[2,] %*% sca
  }
  if (!is.null(rot)) {
    xax <- xax %*% rot
    yax <- yax %*% rot
    zax <- zax %*% rot
  }
  if (!is.null(trn)) {
    xax[1,] <- xax[1,] + trn
    xax[2,] <- xax[2,] + trn
    yax[1,] <- yax[1,] + trn
    yax[2,] <- yax[2,] + trn
    zax[1,] <- zax[1,] + trn
    zax[2,] <- zax[2,] + trn
  }

  rgl::lines3d( xax,color = "red")
  rgl::lines3d(yax,color = "green")
  rgl::lines3d(zax,color = "blue")

  rgl::text3d(xax[2,],text = "X",color = "red",cex = charexp)
  rgl::text3d(yax[2,],text = "Y",color = "green",cex = charexp)
  rgl::text3d(zax[2,],text = "Z",color = "blue",cex = charexp)

  if (!is.null(title)) {
    rgl::text3d(xax[1,],text = title,color = "blue",cex = charexp)
  }
}
