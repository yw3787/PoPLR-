povlr <- function(dat, nperm = 5000) {
  require(combinat)
  require(matrixStats)
  n <- nrow(dat)
  years <- as.numeric(dat$date - dat$date[1]) / 365.25
  dat <- as.matrix(dat[7:ncol(dat)])
  ssyears <- (n - 1) * var(years)
  kvyears <- (n - 2) * ssyears
  ssy <- (n - 1) * colVars(dat)
  ssy[ssy < 1e-6] <- 1e-6
  X <- cbind(rep(1, n), years)
  Xvals <- (solve(t(X) %*% X) %*% t(X))[2,]
  if (n < 8) {
    porder <- matrix(unlist(permn(n)), ncol = n, byrow = TRUE)
    if (nperm < nrow(porder)) 
      porder <- rbind(porder[1,], porder[sample(nrow(porder), nperm - 1),])
    else nperm <- nrow(porder)
  } else {
    porder <- t(replicate(factorial(8), c(1:n)[sample(n)]))
    porder <- rbind(c(1:n), porder)
    porder <- unique(porder)[1:nperm,]
    if (nrow(porder) != nperm) 
      stop("something went wrong and did not get the number of permutations you wanted")
  }
  int <- matrix(NA, nperm, ncol(dat))
  sl <- matrix(NA, nperm, ncol(dat))
  pval <- matrix(NA, nperm, ncol(dat))
  S <- rep(NA, nperm)
  for(i in 1:nperm) {
    sl[i,] <- round(as.numeric((Xvals[porder[i,]] %*% dat)), 6)
    se <- sqrt((ssy - ssyears * sl[i,]^2) / kvyears)
    pval[i,] <- pt(sl[i,] / se, n - 2)
    S[i] <- -sum(log(pval[i,]))
  }
  int <- matrix(rep(apply(dat, 2, mean), nperm), nrow(sl), ncol(sl), byrow = TRUE) - mean(years) * sl
  lr <- data.frame(int = int[1,], sl = sl[1,], p = pval[1,])
  return(list(p = sum(S[1] < S) / nperm, S = S[1], Sdist = S[2:length(S)], lr = lr))
}