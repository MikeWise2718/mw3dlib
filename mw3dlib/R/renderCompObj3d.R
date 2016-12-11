#' Render a compObj object in rgl
#' 
#' @param obj the compobj to render
#' @param exclude Layers to exclude
#' @param include If layers have been excluded, then these layers will be included anyway
#' @param lax If true then local 3-axis markers will be rendered for every component (default F)
#' @param wireframe If true object will be rendered as wireframe, otherwise it is shaded (default F)
#' @param quiet A boolean value that supresses execution feedback output (default T)
#' @export renderCompObj3d
renderCompObj3d <- function(obj,exclude = NULL,include = NULL,lax = F,quiet = T,wireframe = F) {
  # Plot a ComObj3d
  # potentially scaled, rotated and translated
  cdf <- obj$cdf
  pdf <- obj$pdf
  ptdf <- obj$ptdf
  vidf <- obj$vidf
  ncomp <- nrow(obj$cdf)
  npart <- nrow(obj$pdf)

  for (cidx in 1:ncomp) {
    # setup
    cid <- cdf$id[cidx]
    cname <- cdf$compname[cidx]
    pname <- cdf$partname[cidx]
    pidx <- which(pdf$partname == pname)
    partid <- pdf$partid[pidx]

    # get the points for this component
    pt1df <- ptdf[ptdf$partid == partid,]
    pt1df$partid <- NULL
    mpt <- t(as.matrix(pt1df))

    # get the indexs for this component
    vi1df <- vidf[vidf$partid == partid,]
    vi1df$partid <- NULL
    mvi <- t(as.matrix(vi1df))

    # make the mesh, then rotate and transform if necssary
    mesh <- rgl::tmesh3d(mpt,mvi)

    sca <- cdf$sca[[cidx]]  # double brackets because these are lists
    rot <- cdf$rot[[cidx]]
    trn <- cdf$trn[[cidx]]

    mesh <- rgl::scale3d(mesh,x = sca[1],y = sca[2],z = sca[3])
    mesh <- rgl::rotate3d(mesh,matrix = rot)
    mesh <- rgl::translate3d(mesh,trn[1],trn[2],trn[3])

    # render it
    if (!quiet) {
      print(sprintf("%25s cid:%4d  - cidx:%2d pidx:%2d pts:%5d vidx:%5d",
                    cname,cid,cidx,pidx,length(mpt),length(mvi)))
    }

    clr <- pdf$amb[pidx]
    excludeit <- F
    if (!is.null(exclude)) {
      excludeit <- grepl(exclude,cdf$layers[cidx])
      if (excludeit) {
        if (!is.null(include)) {
          excludeit <- !grepl(include,cdf$layers[cidx])
        }
      }
    }
    if (!excludeit) {
      if (wireframe) {
        rgl::wire3d(mesh,color = clr,alpha = pdf$amb.a[pidx])
      } else {
        rgl::shade3d(mesh,color = clr,alpha = pdf$amb.a[pidx])
      }
      if (lax) {
        addAxesToRgl(10,sca=NULL,trn=trn,rot=rot) # show the local coordinate system
      }
    }
  }
}
