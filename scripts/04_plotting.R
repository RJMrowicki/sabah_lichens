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
  width = 18/2.54, height = 8/2.54
)

# arrange richness and diversity plots side by side:
grid.arrange(boxplot_li_taxa_s, boxplot_li_taxa_h, ncol = 2)

dev.off()  # close .pdf graphics device


# ~ functional groups:
pdf(  # open .pdf graphics device
  './figs/boxplots_func.pdf',
  width = 18/2.54, height = 8/2.54
)

grid.arrange(boxplot_li_func_s, boxplot_li_func_h, ncol = 2)

dev.off()  # close .pdf graphics device




# CAP ordination plots ==============================================

cap_mar <- c(3.5, 3.5, 0.5, 0.5)  # specify plot margins for CAP

# lookup tables for point styles:
pt_sty_site <- data.frame(  # by site
  site = levels(dd_lichens_taxa$site),
  pch = c(0, 1, 17),  # symbol
  cex = rep(0.8, 3),  # size
  col = rep(grey(0.5), 3)  # colour
)

pt_sty_girth <- data.frame(  # by girth
  girth = levels(dd_tree_lichens_taxa$girth),
  pch = c(3, 1, 17),  # symbol
  cex = rep(0.8, 3),  # size
  col = rep(grey(0.5), 3)  # colour
)

pt_sty_bark <- data.frame(  # by bark type
  bark = levels(dd_tree_lichens_taxa$bark),
  pch = c(4, 3, 2, 1),  # symbol
  cex = rep(0.8, 4),  # size
  col = rep(grey(0.5), 4)  # colour
)




# ~ Tree-level ------------------------------------------------------

# ~~ taxonomic groups:

pdf(  # open .pdf graphics device
  './figs/cap_taxa.pdf',
  width = 18/2.54, height = 9/2.54
)


par( # set plotting parameters
  las = 1, mar = cap_mar)

par(fig = c(0, 0.5, 0, 1))
plot_cap_env(  # plot CAP ordination (with environmental correlations)
  cap_obj = cap_taxa, cap_dat = dd_tree_lichens_taxa,
  spp_cor_obj = cor_taxa_spp, # env_cor_obj = cor_cap_taxa_env,
  delta_sq_obj = delta_sq_taxa, fig_cex = 0.8,
  pt_sty_dat = pt_sty_girth, plot_lab = "(a)", leg_title = "Tree girth"
)

par(fig = c(0.5, 1, 0, 1), new = TRUE)
plot_cap_spp(  # plot species correlations with CAP axes
  spp_cor_obj = cor_cap_taxa_spp, spp_cor_obj_lab = cor_taxa_spp,
  fig_cex = 0.8, plot_lab = "(b)"
)


dev.off()  # close .pdf graphics device




# ~~ functional groups:

pdf(  # open .pdf graphics device
  './figs/cap_func.pdf',
  width = 18/2.54, height = 9/2.54
)


par( # set plotting parameters
  las = 1, mar = cap_mar)

par(fig = c(0, 0.5, 0, 1))
plot_cap_env(  # plot CAP ordination (with environmental correlations)
  cap_obj = cap_func, cap_dat = dd_tree_lichens_func,
  spp_cor_obj = cor_func_spp, # env_cor_obj = cor_cap_func_env,
  delta_sq_obj = delta_sq_func, fig_cex = 0.8,
  pt_sty_dat = pt_sty_girth, plot_lab = "(a)", leg_title = "Tree girth"
)

par(fig = c(0.5, 1, 0, 1), new = TRUE)
plot_cap_spp(  # plot species correlations with CAP axes
  spp_cor_obj = cor_cap_func_spp, spp_cor_obj_lab = cor_func_spp,
  fig_cex = 0.8, plot_lab = "(b)"
)


dev.off()  # close .pdf graphics device




# ~ Plot-level ------------------------------------------------------

# ~~ taxonomic groups:

pdf(  # open .pdf graphics device
  './figs/cap_taxa_plot.pdf',
  width = 18/2.54, height = 9/2.54
)


par( # set plotting parameters
  las = 1, mar = cap_mar)

par(fig = c(0, 0.5, 0, 1))
plot_cap_env(  # plot CAP ordination (with environmental correlations)
  cap_obj = cap_taxa_plot, cap_dat = tree_lichens_taxa_plot,
  spp_cor_obj = cor_taxa_plot_spp, # env_cor_obj = cor_cap_taxa_env,
  delta_sq_obj = delta_sq_taxa_plot, fig_cex = 0.8,
  pt_sty_dat = pt_sty_site, plot_lab = "(a)", leg_title = "Site"
)

par(fig = c(0.5, 1, 0, 1), new = TRUE)
plot_cap_spp(  # plot species correlations with CAP axes
  spp_cor_obj = cor_cap_taxa_plot_spp, spp_cor_obj_lab = cor_taxa_plot_spp,
  fig_cex = 0.8, plot_lab = "(b)"
)


dev.off()  # close .pdf graphics device
