RCA <- function (mat, binary = FALSE) {
  mat <- as.matrix(mat)
  share_tech_city  <- mat/rowSums(mat)
  share_tech_total <- colSums(mat)/sum(mat)

  LQ <- t(t(share_tech_city)/share_tech_total)
  LQ[is.na(LQ)] <- 0

  if (binary) {
    LQ[LQ < 1] <- 0
    LQ[LQ > 1] <- 1
  }
  return(LQ)
}

# resource cosdes <https://github.com/PABalland/EconGeo/blob/master/R/RCA.r>
