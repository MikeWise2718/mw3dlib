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
    mesh <- tmesh3d(mpt,mvi)

    sca <- cdf$sca[[cidx]]  # double brackets because these are lists
    rot <- cdf$rot[[cidx]]
    trn <- cdf$trn[[cidx]]

    mesh <- scale3d(mesh,x = sca[1],y = sca[2],z = sca[3])
    mesh <- rotate3d(mesh,matrix = rot)
    mesh <- translate3d(mesh,trn[1],trn[2],trn[3])

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
        wire3d(mesh,color = clr,alpha = pdf$amb.a[pidx])
      } else {
        shade3d(mesh,color = clr,alpha = pdf$amb.a[pidx])
      }
      if (lax) {
        addAxesToRgl(10,t = trn,r = rot) # show the local coordinate system
      }
    }
  }
}
