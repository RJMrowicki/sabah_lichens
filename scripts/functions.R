# sabah_lichens
# Functions

# determine best fitting probability distribution for GLM:
# <https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html>
distr_plots <- function (dat)  # accepts a response vector
{
  # if data include 0s, add 1 (to make non-zero):
  if (0 %in% dat) dat <-  dat + 1
  
  # create quantile-comparison plots for different distributions:
  par(mfrow = c(2, 3))  # set plotting parameters
  # normal:
  qqp(dat, "norm", main = "Normal")
  # lognormal:
  qqp(dat, "lnorm", main = "Lognormal")
  
  if (is.integer(dat)) {  # only if integer,
    # negative binomial (fit distribution to extract required parameters):
    nbin_fit <- fitdistr(dat, "Negative binomial")
    qqp(
      dd_tree_lichens_taxa$S + 1, "nbinom", main = "Negative\nbinomial",
      size = nbin_fit$estimate[[1]], mu = nbin_fit$estimate[[2]]
    )
    # poisson (fit distribution to fit required parameter):
    pois_fit <- fitdistr(dat, "Poisson")
    qqp(dat, "pois", main = "Poisson", lambda = pois_fit$estimate)
  }
  
  # gamma (fit distribution extract required parameters):
  gamm_fit <- fitdistr(dat, "gamma", lower = c(0, 0))
  # (NB -- impose lower limits to avoid negative value errors)
  qqp(
    dat, "gamma", main = "Gamma",
    shape = gamm_fit$estimate[[1]], rate = gamm_fit$estimate[[2]]
  )
}




# perform MDS analysis:
mds <- function (dat)  # accepts data frame of samples vs. species
{  
  repeat {
    mds.out <- metaMDS(
      dat, autotransform = FALSE,  # avoid standardisation
      distance = "bray", zerodist = "add", trymax = 100)
    # ensure procedure runs until solution reached:
    if(mds.out$converged == TRUE) break
  }
  return(mds.out)  # output result
}




# create formatted SIMPER tables:
simp_tab <- function (simp_obj_transf, simp_obj_untransf)  # simper objects
{
  tables_out <- vector(  # create empty list to store tables
    'list', length = length(summary(simp_obj_transf)))
  
  for(i in 1:length(summary(simp_obj_transf))) {
    # create table using transformed data simper output:
    tab <- summary(simp_obj_transf)[[i]][
      c("ava", "avb", "ratio", "cumsum")]
    # convert cumulative % contributions into actual % contributions:
    contrib <- c(tab$cumsum, 0) - c(0, tab$cumsum)
    tab$cumsum <- contrib[1:nrow(tab)]*100  # change decimal to %
    
    tab <- tab[tab$cumsum >= 3, ]  # include only species contributing >3%
    
    # substitute transformed species abundances for untransformed:
    tab[, c("ava", "avb")] <- summary(simp_obj_untransf)[[i]][
      rownames(tab), c("ava", "avb")]
    
    # format columns to appropriate no. of d.p.:
    tab$ava <- formatC(tab$ava, 2, format = "f")  # abundances 2 d.p.
    tab$avb <- formatC(tab$avb, 2, format = "f")
    tab$ratio <- formatC(tab$ratio, 2, format = "f")  # rest to 2 d.p.
    tab$cumsum <- formatC(tab$cumsum, 2, format = "f")
    
    # substitute abundance colnames for actual group names:
    colnames(tab)[which(colnames(tab)=="ava")] <- strsplit(
      names(simp_obj_transf)[i], split = "_")[[1]][1]
    colnames(tab)[which(colnames(tab)=="avb")] <- strsplit(
      names(simp_obj_transf)[i], split = "_")[[1]][2]
    # alter the rest of the column names:
    colnames(tab)[which(colnames(tab)=="ratio")] <- "Contrib./SD"
    colnames(tab)[which(colnames(tab)=="cumsum")] <- "Contrib.\\%"
    
    # store finished table in list:
    tables_out[[i]] <- tab
  }
  return(tables_out)  # output list of tables
}




# z-standardise data frame:
z_std <- function (dat)  # accepts data frame, tibble, etc.
{
  dat_z <- apply(dat, MARGIN = 2, function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)})
  return(as.data.frame(dat_z))  # output standardised data
}




# make custom ggplot2 theme:
# (requires ggplot2::theme())
theme_rob <- function (axis_col = "black")
{ 
  theme_bw() %+replace% 
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(1, 1, 1, 1),
      axis.line = element_line(colour = axis_col, size = 0.25),
      axis.ticks = element_line(colour = axis_col, size = 0.25),
      axis.text = element_text(colour = axis_col, size = 8),
      axis.title = element_text(colour = axis_col, size = 8),
      strip.background = element_blank(),
      strip.text = element_text(size = 11),
      strip.placement = "outside",
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(0, "cm"),
      legend.title = element_blank(),
      legend.text = element_text(size = 8)
    )
}