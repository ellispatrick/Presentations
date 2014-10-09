DLDA = function(x,y,x.test){
  n <- nrow(x)
  p <- ncol(x)
  nk <- rep(0, max(y) - min(y) + 1)
  K <- length(nk)
  m <- matrix(0, K, p)
  v <- matrix(0, K, p)
  disc <- matrix(0, nrow(x.test), K)
  for (k in (1:K)) {
    which <- (y == k + min(y) - 1)
    nk[k] <- sum(which)
    m[k, ] <- apply(x[which, , drop = FALSE], 2, mean)
    v[k, ] <- apply(x[which, , drop = FALSE], 2, var)
  }
  vp <- apply(v, 2, function(z) sum((nk - 1) * z)/(n - sum(nk != 
                                                             0)))
  w = (m[2,]-m[1,])*(1/vp)

  disc = as.matrix(x.test)%*%(w)-sum(w*colMeans(m))
  pred <- (disc>0)*1
  list(pred = pred,disc = disc)
  
}
