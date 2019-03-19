# sabah_lichens
# Functions

mds <- function(  # perform MDS analysis
  x) {  # accepts data frame of samples vs. species
  
  repeat {
    mds.out <- metaMDS(
      x, autotransform = FALSE,  # avoid standardisation
      distance = "bray", zerodist = "add", trymax = 100)
    # ensure procedure runs until solution reached:
    if(mds.out$converged == TRUE) break
  }
  return(mds.out)  # output result
  
}




z_std <- function(  # z-standardise data frame
  dat) {  # accepts data frame
  
  dat_z <- apply(dat, MARGIN = 2, function(x) {
    (x - mean(x)) / sd(x)})
  return(as.data.frame(dat_z))  # output standardised data
}
