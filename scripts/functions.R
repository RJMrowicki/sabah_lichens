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




# plot CAP ordination with environmental correlations:
plot_cap_env <- function(
  cap_obj, spp_cor_obj, env_cor_obj = NULL, delta_sq_obj,
  cap_dat, pt_sty_dat, fig_cex = 0.8, plot_lab = "", leg_title = ""
)
{
  # vector of CAP site labels:
  site_labs <- as.vector(rownames(scores(cap_obj)$sites))
  # vector of CAP environmental vector labels:
  env_labs <-  rownames(env_cor_obj)
  # vector of CAP species vector labels:
  spp_labs <- as.vector(rownames(spp_cor_obj))
  
  
  # extract point coordinates (for sites):
  use_x <- scores(cap_obj)$sites[, "CAP1"]
  use_y <- scores(cap_obj)$sites[, "CAP2"]
  # determine x and y limits (based on lowest integer value):
  x0 <- 2 * floor(min(use_x)/2); x1 <- 2 * ceiling(max(use_x)/2)
  y0 <- 2 * floor(min(use_y)/2); y1 <- 2 * ceiling(max(use_y)/2)
  # (NB -- `2 * round(x/2)` rounds to nearest even integer)
  xdist = x1-x0; ydist = y1-y0  # calculate x and y distances
  
  plot(  # create blank plot area
    use_x, use_y, type = "n", bty = "l",
    xaxt = "n", xaxs = "i", yaxt = "n", yaxs = "i",
    xlab = "", ylab = "",
    xlim = c(x0, x1), ylim = c(y0, y1), cex.lab = fig_cex
  )
  
  abline(h = 0, col = grey(0.75))  # horizontal line at x = 0
  abline(v = 0, col = grey(0.75))  # vertical line at y = 0
  sf <- 1.1  # scaling factor for circle perimeter and arrow length
  # # add circle (perimeter represents correlation = 1):
  # draw.circle(0, 0, radius = (x1-x0)*sf, border = grey(0.75))
  
  axis(  # add x axis
    side = 1, cex.axis = fig_cex,
    at = seq(x0, x1, 2), labels = seq(x0, x1, 2)
  )
  title(  # axis label including proportion explained
    line = 2.5, cex.lab = fig_cex,
    xlab = parse(text = paste(
      "paste(\"CAP1 (\", delta^2, \" = \",",
      rep_2dp(delta_sq_obj[1]), ",\")\", sep = \"\")"))
  )
  axis( # add y axis
    side = 2, cex.axis = fig_cex,
    at = seq(y0, y1, 2), labels = seq(y0, y1, 2)
  )
  title(  # axis label including proportion explained
    line = 2.25, cex.lab = fig_cex,
    ylab = parse(text = paste(
      "paste(\"CAP2 (\", delta^2, \" = \",",
      rep_2dp(delta_sq_obj[2]), ",\")\", sep = \"\")"))
  )
  
  # extract point style variable name:
  pt_sty_var <- names(pt_sty_dat)[1]
  points(  # add points
    use_x, use_y,
    pch = as.numeric(pt_sty_dat[  # lookup symbol from table
      match(as_vector(cap_dat[, pt_sty_var]), pt_sty_dat[, pt_sty_var]
      ), "pch"]),
    cex = as.numeric(pt_sty_dat[  # lookup size from table
      match(as_vector(cap_dat[, pt_sty_var]), pt_sty_dat[, pt_sty_var]
      ), "cex"]),
    col = as.character(pt_sty_dat[  # lookup colour from table
      match(as_vector(cap_dat[, pt_sty_var]), pt_sty_dat[, pt_sty_var]
      ), "col"]))
  
  par(xpd = NA)  # allow plotting throughout device region
  
  # text(  # add replicate labels
  #   use_x-0.02*(x1-x0), use_y,
  #   labels = site_labs,
  #   adj = c(1, 0), pos = 1,  # pos overrides adj
  #   offset = 0.25, cex = 0.7, col = grey(0.5)
  # )
  # wordcloud::textplot(  # add non-overlapping replicate labels
  #   use_x, use_y,
  #   words = site_labs,
  #   xlim = c(x0, x1), ylim = c(y0, y1),
  #   new = FALSE, cex = 0.5, col = grey(0.5),
  #   show.lines = FALSE
  # )
  
  if (!is.null(env_cor_obj)) {
    # text(  # add vector overlay of environmental variables
    #   # (NB -- uses product-moment, not rank, correlation coefficients)
    #   cap_obj, display = "bp", col = grey(0.5), axis.bp = FALSE,
    #   arrow.mul = (xdist)*sf,  # length multiplier matches circle radius
    #   head.arrow = 0.05, cex = 0.7,
    #   labels = rownames(summary(cap_obj)$biplot)
    # )
    arrows(  # add arrows for environmental correlations, from origin
      x0 = 0, y0 = 0,
      x1 = env_cor_obj$CAP1*((xdist/2)*sf),  # use scaling factor
      y1 = env_cor_obj$CAP2*((xdist/2)*sf),  # (correspond with circle perimeter)
      code = 2, length = 0.05, lwd = 1.5, col = "blue"
    )
    text(  # add environmental labels
      env_cor_obj$CAP1*((xdist/2)*sf), env_cor_obj$CAP2*((xdist/2)*sf),
      env_labs, pos = c(apply( #
        env_cor_obj, MARGIN = 1, FUN = function (x) {
          if (abs(x["CAP1"]) > abs(x["CAP2"])) {  # if |x| > |y|
            if (x["CAP1"] < 0) {2} else {4}  # if x < 0, label left of point
          } else { # if |x| < |y|
            if (x["CAP2"] < 0) {1} else {3}  # if y < 0, label below point
          }})),
      offset = 0.15, col = "blue", cex = fig_cex
    )
  }
  
  par(xpd = FALSE)  # re-clip plotting to plot region
  
  legend(  # add legend for points
    "topright", bty = "n", title = leg_title,
    legend = pt_sty_dat[, pt_sty_var],
    pch = pt_sty_dat$pch, col = pt_sty_dat$col,
    pt.cex = 0.8*pt_sty_dat$cex, y.intersp = 0.8, cex = 0.8
  )
  legend(  # add plot label
    "topleft", bty = "n", legend = "", title = plot_lab
  )
}




# plot CAP species correlations:
plot_cap_spp <- function (
  spp_cor_obj, spp_cor_obj_lab, fig_cex = 0.8, plot_lab = ""
)
{
  # vector of CAP species vector labels:
  spp_labs <- as.vector(rownames(spp_cor_obj_lab))
  
  plot(  # create blank plot
    spp_cor_obj$CAP1, spp_cor_obj$CAP2,
    type = "n", bty = "l",
    xaxt = "n", xaxs = "i", xlab = "",
    yaxt = "n", yaxs = "i", ylab = "",
    xlim = c(-1, 1), ylim = c(-1, 1), cex.lab = fig_cex
  )
  
  abline(h = 0, col = grey(0.75))  # horizontal line at x = 0
  abline(v = 0, col = grey(0.75))  # vertical line at y = 0
  # draw circle (perimeter represents correlation = 1):
  draw.circle(0, 0, radius = 1, border = grey(0.75))
  
  axis(  # add x-axis
    side = 1, cex.axis = fig_cex,
    at = seq(-1, 1, 0.5), labels = seq(-1, 1, 0.5) 
  )
  title(  # add x-axis label
    line = 2.5, cex.lab = fig_cex,
    xlab = "Correlation with CAP Axis 1"
  )
  axis(  # add y-axis
    side = 2, cex.axis = fig_cex,
    at = seq(-1, 1, 0.5), labels = seq(-1, 1, 0.5)
  )
  title( # add y-axis label
    line = 2.25, cex.lab = fig_cex,
    ylab = "Correlation with CAP Axis 2"
  )
  
  arrows(  # add arrows for species correlations, from origin
    x0 = 0, y0 = 0, x1 = spp_cor_obj$CAP1, y1 = spp_cor_obj$CAP2,
    code = 2, length = 0.05, col = grey(0.5)
  )
  
  par(xpd = TRUE)  # allow plotting throughout figure region
  
  # textplot(  # add non-overlapping species labels
  #   # expand x and y from origin:
  #   (abs(spp_cor_obj_lab$CAP1) + 0.01) * sign(spp_cor_obj_lab$CAP1),
  #   (abs(spp_cor_obj_lab$CAP2) + 0.01) * sign(spp_cor_obj_lab$CAP2),
  #   # spp_cor_obj_lab$CAP1, spp_cor_obj_lab$CAP2,  # original coordinates
  #   words = parse(text = spp_labs),
  #   xlim = c(-1, 1), ylim = c(-1, 1),
  #   new = FALSE, cex = fig_cex, show.lines = FALSE
  # )
  lay <- wordlayout(  # generate coords for non-overlapping text
    # # expand x and y from origin:
    # (abs(spp_cor_obj_lab$CAP1) + 0.01) * sign(spp_cor_obj_lab$CAP1),
    # (abs(spp_cor_obj_lab$CAP2) + 0.01) * sign(spp_cor_obj_lab$CAP2),
    spp_cor_obj_lab$CAP1, spp_cor_obj_lab$CAP2,  # original coordinates
    words = spp_labs, cex = fig_cex,
    xlim = c(-1, 1), ylim = c(-1, 1)
  )
  text(  # add non-overlapping species labels
    # lay[, 1], lay[, 2], rownames(lay),  # generated coordinates
    spp_cor_obj_lab, labels = rownames(spp_cor_obj_lab), # original coordinates
    pos = pmap_dbl(  # position relative to coordinates,
      # based on generated non-overlapping coordinates:
      as.data.frame(lay[, c('x', 'y')]),
      function (x, y) {
        if (abs(x) > abs(y)) {  # if |x| > |y|,
          # if x < 0, label left of point:
          if (x < 0) { 2 } else { 4 }
        } else {  # if |x| < |y|,
          # if y < 0, label below point:
          if (y < 0) { 1 } else { 3 }
        }}),
    offset = 0.1, col = "black", cex = fig_cex
  )
  
  par(xpd = FALSE)  # re-clip plotting to plot region
  
  legend( # add plot label
    "topleft", bty = "n", legend = "", title = plot_lab
  )
}




# plot MDS ordination:
plot_mds <- function(
  mds_obj, pt_sty_dat, pt_ref_dat, pt_sty_var,
  stress = TRUE, legend = FALSE, leg_title = "", plot_lab = "",
  output = FALSE
)
{
  # extract lowest and highest x and y values:
  x0 <- min(scores(mds_obj)[,1]); x1 <- max(scores(mds_obj)[,1])
  y0 <- min(scores(mds_obj)[,2]); y1 <- max(scores(mds_obj)[,2])
  xdist <- abs(x1-x0); ydist <- abs(y1-y0)  # x and y distances
  
  plot(  # create plot area
    mds_obj, disp = "sites",
    type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
    xlim = c(x0-xdist*0.1, x1+xdist*0.1),  # set x and y limits,
    ylim = c(y0-ydist*0.1, y1+ydist*0.1)  # extend each by 10%
  )
  
  points(  # add points
    mds_obj, cex = 1,
    pch = as.numeric(pt_sty_dat[  # lookup style from data frame
      match(as_vector(pt_ref_dat[, pt_sty_var]), pt_sty_dat[, pt_sty_var]), "pch"]),
    col = as.character(pt_sty_dat[
      match(as_vector(pt_ref_dat[, pt_sty_var]), pt_sty_dat[, pt_sty_var]), "col"])
  )
  
  usr <-  par("usr")  # extract plot area coordinates
  if(stress == TRUE) {  # plot stress value
    text(
      usr[2]-((usr[2]-usr[1])/100), usr[4]-((usr[4]-usr[3])/100),
      adj = c(1, 1), cex = 0.8,
      labels = paste("2D stress =", formatC(mds_obj$stress, 2, format = "f"))
    )
  }
  
  if(legend == TRUE) {
    legend(  # add legend for points
      "topright", inset = c(0, 0.05), bty = "n",
      title = leg_title, legend = pt_sty_dat[, pt_sty_var],
      pch = pt_sty_dat$pch, col = pt_sty_dat$col,
      pt.cex = 0.8*pt_sty_dat$cex, y.intersp = 0.8, cex = 0.8
    )
  }
  legend(  # add plot label
    "topleft", bty = "n", legend = "", title = plot_lab
  )
  
  if(output == TRUE) {  # output plot data
    return(list(xdist = xdist, ydist = ydist, usr = usr))
  }
}




# report numeric value (e.g. MS, F) with 2 d.p.:
rep_2dp <- function (num_val)
{
  formatC(num_val, 2, format = "f")
}




# report P value with 3 d.p.:
rep_p <- function (p_val)
{
  if (p_val < 0.001) {"< 0.001"} else {  # display < 0.001 if necessary
    paste("= ", formatC(p_val, 3, format = "f"))  # display P value
  }
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