# sabah_lichens
# Plotting

# Boxplots of lichen diversity vs. site =============================

# create boxplots:

# ~ taxonomic groups:
# ~~ richness:
boxplot_li_taxa_s <-  # assign to object
  ggplot(dd_lichens_taxa, aes(x = `site`, y = `S`)) +
  geom_boxplot() +
  scale_x_discrete(name = "Site") +
  ylim(c(NA, 10.5)) +  # set upper y limit only
  geom_jitter(
    shape = 16, colour = grey(0.25), alpha = 0.5,
    position = position_jitter(width = 0.15, height = 0)) +
  theme_rob()
# ~~ diversity:
# (same plot as above, except with different y variable)
boxplot_li_taxa_h <- boxplot_li_taxa_s + aes(y = `H'`) +
  ylim(c(NA, 2.5))  # set upper y limit only

# ~ functional groups:
boxplot_li_func_s <-
  ggplot(dd_lichens_func, aes(x = `site`, y = `S`)) +
  geom_boxplot() +
  scale_x_discrete(name = "Site") +
  ylim(c(NA, 10.5)) +  # set upper y limit only
  scale_y_continuous(limits = c(0, 10)) +
  geom_jitter(
    shape = 16, colour = grey(0.25), alpha = 0.5,
    position = position_jitter(width = 0.15, height = 0)) +
  theme_rob()
# ~~ diversity:
boxplot_li_func_h <- boxplot_li_func_s + aes(y = `H'`) +
  ylim(c(NA, 2.5))  # set upper y limit only




# output boxplots:

# ~ taxonomic groups:
pdf(  # open .pdf graphics device
  './figs/boxplots_taxa.pdf',
  # set width/height in cm (convert from inches):
  width = 18/2.54, height = 8/2.54)

# arrange richness and diversity plots side by side:
grid.arrange(boxplot_li_taxa_s, boxplot_li_taxa_h, ncol = 2)

dev.off()  # close .pdf graphics device


# ~ functional groups:
pdf(  # open .pdf graphics device
  './figs/boxplots_func.pdf',
  width = 18/2.54, height = 8/2.54)

grid.arrange(boxplot_li_func_s, boxplot_li_func_h, ncol = 2)

dev.off()  # close .pdf graphics device




# CAP ordination plot ===============================================

cap_mar <- c(3.5, 3.5, 0.5, 0.5) # specify plot margins for CAP
fig_cex <- 0.8 # specify plot text size

# lookup tables for point styles:
pt_sty_site <- data.frame(  # by roughness category
  site = levels(dd_lichens_taxa$site),
  pch = c(1, 2, 0), cex = rep(1, 3),
  col = rep("black", 3))




# vector of CAP site labels:
site_labs <- as.vector(rownames(scores(cap_taxa)$sites))

# vector of CAP environmental vector labels:
env_labs <-  rownames(summary(cap_taxa)$biplot)

# vector of CAP species vector labels:
spp_labs <- as.vector(rownames(cor_cap_taxa_spp))




pdf(  # open .pdf graphics device
  './figs/cap_taxa.pdf',
  width = 18/2.54, height = 9/2.54)

par( # set plotting parameters
  las = 1, mar = cap_mar)


# ~~ (A) CAP ordination plot:

par(fig = c(0, 0.5, 0, 1))

# extract point coordinates (for sites):
use_x <- scores(cap_taxa)$sites[, "CAP1"]
use_y <- scores(cap_taxa)$sites[, "CAP2"]
# determine x and y limits (based on lowest integer value):
x0 <- y0 <- min(floor(min(use_x)), floor(min(use_y)))
x1 <- y1 <- max(ceiling(max(use_x)), ceiling(max(use_y)))

plot( # create plot area
  use_x, use_y, type = "n", bty = "l",
  xaxt = "n", xaxs = "i", yaxt = "n", yaxs = "i",
  xlab = "", ylab = "",
  xlim = c(x0, x1), ylim = c(y0, y1), cex.lab = fig_cex)

abline(h = 0, col = grey(0.75))  # horizontal line at x = 0
abline(v = 0, col = grey(0.75))  # vertical line at y = 0
sf <- 0.375  # scaling factor for circle perimeter and arrow length
# draw.circle(  # add circle (perimeter represents correlation = 1)
#   0, 0, radius = (x1-x0)*sf, border = grey(0.75))
axis(  # add x axis
  side = 1, at = seq(x0, x1, 2), labels = seq(x0, x1, 2), cex.axis = fig_cex)
title(  # axis label including proportion explained
  line = 2.5, cex.lab = fig_cex, xlab = parse(text = paste(
    "paste(\"CAP1 (\", delta^2, \" = \",",
    rep_2dp(delta_sq_taxa[1]), ",\")\", sep = \"\")")))
axis( # add y axis
  side = 2, at = seq(y0, y1, 2), labels = seq(y0, y1, 2), cex.axis = fig_cex)
title(  # axis label including proportion explained
  line = 2.25, cex.lab = fig_cex, ylab = parse(text = paste(
    "paste(\"CAP2 (\", delta^2, \" = \",",
    rep_2dp(delta_sq_taxa[2]), ",\")\", sep = \"\")")))

points(  # add points
  use_x, use_y,
  pch = as.numeric(pt_sty_site[  # lookup symbol from table
    match(as_vector(dd_tree_lichens_taxa[,"site"]), pt_sty_site[,"site"]
    ), "pch"]),
  cex = as.numeric(pt_sty_site[  # lookup size from table
    match(as_vector(dd_tree_lichens_taxa[,"site"]), pt_sty_site[,"site"]
    ), "cex"]),
  col = as.numeric(pt_sty_site[  # lookup colour from table
    match(as_vector(dd_tree_lichens_taxa[,"site"]), pt_sty_site[,"site"]
    ), "col"]))

par(xpd = NA)  # allow plotting throughout device region

# text(  # add transect labels
#   use_x-0.02*(x1-x0), use_y,
#   labels = site_labs,
#   adj = c(1, 0), pos = 1,  # pos overrides adj
#   offset = 0.25, cex = 0.7, col = grey(0.5))
# wordcloud::textplot(  # add non-overlapping transect labels
#   use_x, use_y,
#   words = site_labs,# rownames(scores(cap_spp_subst)$sites),
#   xlim = c(x0, x1), ylim = c(y0, y1),
#   new = FALSE, cex = 0.5, col = grey(0.5),
#   show.lines = FALSE)

# text( # add vector overlay of environmental variables
#   # (NB - uses product-moment, not rank, correlation coefficients)
#   cap_taxa, display = "bp", col = grey(0.5), axis.bp = FALSE,
#   arrow.mul = (x1-x0)*sf, # length multiplier matches circle radius
#   head.arrow = 0.05, cex = 0.7,
#   labels = env_labs)
arrows(  # add arrows for environmental correlations, from origin
  x0 = 0, y0 = 0,
  x1 = cor_cap_taxa_env$CAP1*((x1-x0)*sf),  # use scaling factor
  y1 = cor_cap_taxa_env$CAP2*((x1-x0)*sf),  # (correspond with circle perimeter)
  code = 2, length = 0.05, col = grey(0.5))
text(  # add environmental labels
  cor_cap_taxa_env$CAP1*((x1-x0)*sf), cor_cap_taxa_env$CAP2*((x1-x0)*sf),
  env_labs, pos = c(apply( #
    cor_cap_taxa_env, MARGIN = 1, FUN = function (x) {
      if (abs(x["CAP1"]) > abs(x["CAP2"])) { # if |x| > |y|
        if (x["CAP1"] < 0) {2} else {4}  # if x < 0, label left of point
      } else { # if |x| < |y|
        if (x["CAP2"] < 0) {1} else {3}  # if y < 0, label below point
      }})),
  offset = 0.15, col = "black", cex = fig_cex)

par(xpd = FALSE) # re-clip plotting to plot region

# legend(  # add legend for sites
#   "topright", bty = "n", title = "Site",
#   legend = pt_sty_site$site,
#   pch = pt_sty_site$pch, col = pt_sty_site$col,
#   pt.cex = 0.8*pt_sty_site$cex, y.intersp = 0.8, cex = 0.8)
legend(  # add plot label
  "topleft", bty = "n", legend = "", title = expression(bold("A")))




# ~~ (B) species correlations with CAP axes

par(fig = c(0.5, 1, 0, 1), new = TRUE)

plot(  # create plot
  cor_cap_taxa_spp$CAP1, cor_cap_taxa_spp$CAP2, type = "n", bty = "l",
  xaxt = "n", xaxs = "i", xlab = "",
  yaxt = "n", yaxs = "i", ylab = "",
  xlim = c(-1, 1), ylim = c(-1, 1), cex.lab = fig_cex)
abline(h = 0, col = grey(0.75))  # horizontal line at x = 0
abline(v = 0, col = grey(0.75))  # vertical line at y = 0
draw.circle(  # draw circle (perimeter represents correlation = 1)
  0, 0, radius = 1, border = grey(0.75))
axis(  # add x-axis
  side = 1, at = seq(-1, 1, 0.5), labels = seq(-1, 1, 0.5), cex.axis = fig_cex)
title(  # add x-axis label
  line = 2.5, xlab = "Correlation with CAP Axis 1", cex.lab = fig_cex)
axis(  # add y-axis
  side = 2, at = seq(-1, 1, 0.5), labels = seq(-1, 1, 0.5), cex.axis = fig_cex)
title( # add y-axis label
  line = 2.25, ylab = "Correlation with CAP Axis 2", cex.lab = fig_cex)

arrows(  # add arrows for species correlations, from origin
  x0 = 0, y0 = 0, x1 = cor_cap_taxa_spp$CAP1, y1 = cor_cap_taxa_spp$CAP2,
  code = 2, length = 0.05, col = grey(0.5))

par(xpd = TRUE)  # allow plotting throughout figure region
text(  # add species labels
  cor_cap_taxa_spp$CAP1, cor_cap_taxa_spp$CAP2,
  parse(text = spp_labs),
  pos = c(apply(  #
    cor_cap_taxa_spp, MARGIN = 1, FUN = function(x) {
      if(x["CAP2"] < 0) {1} else {3}
    })),
  # if(abs(x["CAP1"]) > abs(x["CAP2"])) { # if |x| > |y|
  #   if(x["CAP1"] < 0) {2} else {4}  # if x < 0, label left of point
  #   } else { # if |x| < |y|
  #     if(x["CAP2"] < 0) {1} else {3}  # if y < 0, label below point
  # }})),
  offset = 0.15, col = "black", cex = fig_cex)
# textplot(  # add non-overlapping species labels
#   cor_cap_taxa_spp$CAP1, cor_cap_taxa_spp$CAP2, # expand x and y
#   words = parse(text = spp_labs),
#   xlim = c(-1, 1), ylim = c(-1, 1),
#   new = FALSE, cex = 0.7, show.lines = FALSE)
par(xpd = FALSE)  # re-clip plotting to plot region

legend( # add plot label
  "topleft", bty = "n", legend = "", title = expression(bold("B")))


dev.off()  # close .pdf graphics device
