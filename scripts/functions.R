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
