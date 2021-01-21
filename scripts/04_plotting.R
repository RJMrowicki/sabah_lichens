# sabah_lichens
# Plotting

# Set plot margins and styles ==================================================

# specify plot margins for MDS and CAP ordinations:
mds_mar <- c(0.4, 0.4, 0.4, 0.4)
cap_mar <- c(3.5, 3.5, 0.5, 0.5)

# lookup table for point styles:
pt_sty_site <- data.frame(  # by site
  site = levels(dd_lichens_taxa$site),
  pch = c(0, 1, 17),  # symbol
  cex = rep(0.8, 3),  # size
  # col = rep(grey(0.5), 3)  # colour
  col = c(grey(0.5), "salmon", "royalblue")  # colour
)




# A. MAIN PLOTS ================================================================

# 0. Boxplots of tree functional traits vs. site ('plot-level') ----------------
# (NB -- include in Supplementary Plots instead???)




# tree_lichens_func_plot %>% 
#   pivot_longer(all_of(c(
#     "bark_div", "pH_div", "girth_l_prop", "buttress_prop", "dipterocarp_prop"
#   ))) %>% 
#   ggplot(aes(x = `site`, y = `value`, colour = `site`)) +
#   geom_boxplot(show.legend = FALSE) +  # NB -- remove colour legend
#   scale_colour_manual(values = pt_sty_site$col) +  # set colours manually
#   facet_wrap(
#     vars(name), nrow = 1,
#     scales = "free_y",
#     strip.position = "left",
#     labeller = as_labeller(c(
#       bark_div = "Bark diversity",
#       pH_div = "pH diversity",
#       girth_l_prop = "Proportion of large trees",
#       buttress_prop = "Proportion of trees with buttresses",
#       dipterocarp_prop = "Proportion of dipterocarps")
#     )) +
#   theme(strip.placement = "outside") +
#   labs(y = NULL) +  # remove y label
#   theme_rob()




# ~ bark diversity:
boxplot_tr_func_plot_bark <-
  ggplot(tree_lichens_func_plot, aes(x = `site`, y = `bark_div`, colour = `site`)) +
  geom_boxplot(show.legend = FALSE) +  # NB -- remove colour legend
  scale_colour_manual(values = pt_sty_site$col) +  # set colours manually
  scale_x_discrete(name = "Site") +
  ylim(c(NA, 1.25)) +  # set upper y limit only
  labs(tag = "(a)", y = "Bark category diversity (H')") +  # add plot and y label
  # geom_jitter(
  #   shape = 16, colour = grey(0.25), alpha = 0.5,
  #   position = position_jitter(width = 0.1, height = 0)) +
  theme_rob()

# ~~ pH diversity:
boxplot_tr_func_plot_pH <- boxplot_tr_func_plot_bark + aes(y = `pH_div`) +
  ylim(c(0, 1.5)) +  # set lower and upper y limits
  labs(tag = "(b)", y = "pH category diversity (H')")  # add plot and y label

# ~~ girth proportion:
boxplot_tr_func_plot_girth <- boxplot_tr_func_plot_bark + aes(y = `girth_l_prop`) +
  ylim(c(NA, 0.6)) +  # set upper y limit only
  labs(tag = "(c)", y = "Proportion of large trees")  # add plot and y label

# ~~ buttress proportion:
boxplot_tr_func_plot_buttress <- boxplot_tr_func_plot_bark + aes(y = `buttress_prop`) +
  ylim(c(NA, 0.4)) +  # set upper y limit only
  labs(tag = "(d)", y = "Proportion of trees with buttresses")  # add plot and y label

# ~~ dipterocarp proportion:
boxplot_tr_func_plot_dipterocarp <- boxplot_tr_func_plot_bark + aes(y = `dipterocarp_prop`) +
  ylim(c(NA, 0.6)) +  # set upper y limit only
  labs(tag = "(e)", y = "Proportion of dipterocarps")  # add plot and y label



# output boxplots:

pdf(  # open .pdf graphics device
  './figs/boxplots_tr_func_plot.pdf',
  width = 13.5/2.54, height = 10/2.54
)


# grid.arrange(
#   boxplot_tr_func_plot_bark, boxplot_tr_func_plot_pH,
#   boxplot_tr_func_plot_girth, boxplot_tr_func_plot_buttress,
#   boxplot_tr_func_plot_dipterocarp,
#   nrow = 2, ncol = 3
# )

# to ensure equal margins & separation of labels from axes for all plots,
# generate grobs for all `ggplot` objects, and set their width attributes
# manually, based on the maximum values observed:
bpt1 <- ggplotGrob(boxplot_tr_func_plot_bark)
bpt2 <- ggplotGrob(boxplot_tr_func_plot_pH)
bpt3 <- ggplotGrob(boxplot_tr_func_plot_girth)
bpt4 <- ggplotGrob(boxplot_tr_func_plot_buttress)
bpt5 <- ggplotGrob(boxplot_tr_func_plot_dipterocarp)

maxWidth <- grid::unit.pmax(
  bpt1$widths[2:5], bpt2$widths[2:5], bpt3$widths[2:5], bpt4$widths[2:5], bpt5$widths[2:5]
)

bpt1$widths[2:5] <- as.list(maxWidth)
bpt2$widths[2:5] <- as.list(maxWidth)
bpt3$widths[2:5] <- as.list(maxWidth)
bpt4$widths[2:5] <- as.list(maxWidth)
bpt5$widths[2:5] <- as.list(maxWidth)

grid.arrange(
  bpt1, bpt2, bpt3, bpt4, bpt5,
  layout_matrix = rbind(c(1, 2, NA), c(3, 4, 5)))


dev.off()  # close .pdf graphics device




# 1. Boxplots of lichen diversity vs. site ('plot-level') ----------------------

# create boxplots:

# ~ taxonomic groups:

# ~~ richness:
boxplot_li_taxa_plot_s <-  # assign to object
  ggplot(tree_lichens_taxa_plot, aes(x = `site`, y = `S`, colour = `site`)) +
  geom_boxplot(show.legend = FALSE) +  # NB -- remove colour legend
  scale_colour_manual(values = pt_sty_site$col) +  # set colours manually
  scale_x_discrete(name = "Site") +
  ylim(c(5, 25)) +  # set lower and upper y limits
  labs(tag = "(a)", y = "Richness (S)") +  # add plot and y label
  # geom_jitter(
  #   shape = 16, colour = grey(0.25), alpha = 0.5,
  #   position = position_jitter(width = 0.1, height = 0)) +
  theme_rob()

# ~~ diversity:
# (same plot as above, except with different y variable)
boxplot_li_taxa_plot_h <- boxplot_li_taxa_plot_s + aes(y = `H'`) +
  ylim(c(0.8, 2.6)) +  # set lower and upper y limits
  labs(tag = "(b)", y = "Diversity (H')")  # add plot and y label

# ~~ evenness:
# (same plot as above, except with different y variable)
`boxplot_li_taxa_plot_1-l` <- boxplot_li_taxa_plot_s + aes(y = `1-L`) +
  ylim(c(0.5, 1)) +  # set lower and upper y limits
  labs(tag = "(c)", y = expression(paste("Evenness (1-", lambda, ")")))  # add plot and y label




# ~ functional groups:

# ~~ FRic:
boxplot_li_func_plot_fric <-
  ggplot(tree_lichens_func_plot, aes(x = `site`, y = `FRic`, colour = `site`)) +
  geom_boxplot(show.legend = FALSE) +  # NB -- remove colour legend
  scale_colour_manual(values = pt_sty_site$col) +  # set colours manually
  scale_x_discrete(name = "Site") +
  scale_y_continuous(
    limits = c(NA, 8e-05),  # set upper y limit only
    labels = label_number(accuracy = 1, scale = 1e+05)) +
  labs(  # add plot and y label
    tag = "(a)",
    y = expression(paste("FRic ("%*% 10^{-5}, ")"))) +
  # geom_jitter(
  #   shape = 16, colour = grey(0.25), alpha = 0.5,
  #   position = position_jitter(width = 0.1, height = 0)) +
  theme_rob()

# ~~ FEve:
boxplot_li_func_plot_feve <- boxplot_li_func_plot_fric + aes(y = `FEve`) +
  ylim(c(0.2, 1)) +  # set lower and upper y limits
  labs(tag = "(b)", y = "FEve")  # add plot and y label

# ~~ FDiv:
boxplot_li_func_plot_fdiv <- boxplot_li_func_plot_fric + aes(y = `FDiv`) +
  ylim(c(0.6, 1)) +  # set lower and upper y limits
  labs(tag = "(c)", y = "FDiv")  # add plot and y label

# ~~ FDis:
boxplot_li_func_plot_fdis <- boxplot_li_func_plot_fric + aes(y = `FDis`) +
  ylim(c(0.1, 0.25)) +  # set upper y limit only
  labs(tag = "(d)", y = "FDis")  # add plot and y label




# output boxplots:

# ~ taxonomic groups:
pdf(  # open .pdf graphics device
  './figs/boxplots_taxa_plot.pdf',
  # set width/height in cm (convert from inches):
  width = 13.5/2.54, height = 5/2.54
)


# grid.arrange(
#   boxplot_li_taxa_plot_s, boxplot_li_taxa_plot_h, `boxplot_li_taxa_plot_1-l`,
#   nrow = 1
# )

bplt1 <- ggplotGrob(boxplot_li_taxa_plot_s)
bplt2 <- ggplotGrob(boxplot_li_taxa_plot_h)
bplt3 <- ggplotGrob(`boxplot_li_taxa_plot_1-l`)

maxWidth <- grid::unit.pmax(
  bplt1$widths[2:5], bplt2$widths[2:5], bplt3$widths[2:5]
)

bplt1$widths[2:5] <- as.list(maxWidth)
bplt2$widths[2:5] <- as.list(maxWidth)
bplt3$widths[2:5] <- as.list(maxWidth)

grid.arrange(bplt1, bplt2, bplt3, nrow = 1)


dev.off()  # close .pdf graphics device


# ~ functional groups:
pdf(  # open .pdf graphics device
  './figs/boxplots_func_plot.pdf',
  width = 18/2.54, height = 5/2.54
)

# grid.arrange(
#   boxplot_li_func_plot_fric, boxplot_li_func_plot_feve,
#   boxplot_li_func_plot_fdiv, boxplot_li_func_plot_fdis,
#   nrow = 1
# )

bplf1 <- ggplotGrob(boxplot_li_func_plot_fric)
bplf2 <- ggplotGrob(boxplot_li_func_plot_feve)
bplf3 <- ggplotGrob(boxplot_li_func_plot_fdiv)
bplf4 <- ggplotGrob(boxplot_li_func_plot_fdis)

maxWidth <- grid::unit.pmax(
  bplf1$widths[2:5], bplf2$widths[2:5], bplf3$widths[2:5], bplf4$widths[2:5]
)

bplf1$widths[2:5] <- as.list(maxWidth)
bplf2$widths[2:5] <- as.list(maxWidth)
bplf3$widths[2:5] <- as.list(maxWidth)
bplf4$widths[2:5] <- as.list(maxWidth)

grid.arrange(bplf1, bplf2, bplf3, bplf4, nrow = 1)


dev.off()  # close .pdf graphics device




# 2. MDS ordination plots ('plot-level') ---------------------------------------

pdf(  # open .pdf graphics device
  './figs/mds_plot.pdf',
  width = 18/2.54, height = 6/2.54
)


par( # set plotting parameters
  las = 1, mar = mds_mar)


# ~ taxonomic groups:

par(fig = c(0, 1/3, 0, 1))
plot_mds(  # plot MDS
  mds_obj = mds_li_taxa_plot,
  pt_sty_dat = pt_sty_site, pt_sty_var = "site",
  pt_ref_dat = tree_lichens_taxa_plot,
  legend = TRUE, leg_title = "Site", plot_lab = "(a)"
)

ordiellipse(  # add ellipses for site groups
  mds_li_taxa_plot, tree_lichens_taxa_plot$site,
  draw = "line", col = as.character(pt_sty_site$col)
)


# ~ functional groups:

par(fig = c(1/3, 2/3, 0, 1), new = TRUE)
plot_mds(  # plot MDS
  mds_obj = mds_li_func_plot,
  pt_sty_dat = pt_sty_site, pt_sty_var = "site",
  pt_ref_dat = tree_lichens_func_plot,
  plot_lab = "(b)"
)

ordiellipse(  # add ellipses for site groups
  mds_li_func_plot, tree_lichens_func_plot$site,
  draw = "line", col = as.character(pt_sty_site$col)
)


# ~ lichen traits:

par(fig = c(2/3, 1, 0, 1), new = TRUE)
plot_mds(  # plot MDS
  mds_obj = mds_li_traits_plot,
  pt_sty_dat = pt_sty_site, pt_sty_var = "site",
  pt_ref_dat = tree_lichens_func_plot,
  plot_lab = "(c)"
)

ordiellipse(  # add ellipses for site groups
  mds_li_traits_plot, tree_lichens_func_plot$site,
  draw = "line", col = as.character(pt_sty_site$col)
)


dev.off()  # close .pdf graphics device




# 3.  CAP ordination plots ('plot-level') --------------------------------------

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
  spp_cor_obj = cor_taxa_plot_spp, env_cor_obj = cor_cap_taxa_plot_env,
  delta_sq_obj = delta_sq_taxa_plot, fig_cex = 0.8,
  pt_sty_dat = pt_sty_site, plot_lab = "(a)", leg_title = "Site"
)

par(fig = c(0.5, 1, 0, 1), new = TRUE)
plot_cap_spp(  # plot species correlations with CAP axes
  spp_cor_obj = cor_taxa_plot_spp, spp_cor_obj_lab = cor_taxa_plot_spp,
  # (NB -- spp_cor_obj = `cor_cap_taxa_plot_spp` to plot all 'species' arrows)
  fig_cex = 0.8, plot_lab = "(b)"
)


dev.off()  # close .pdf graphics device




# ~~ functional groups:

pdf(  # open .pdf graphics device
  './figs/cap_func_plot.pdf',
  width = 18/2.54, height = 9/2.54
)


par( # set plotting parameters
  las = 1, mar = cap_mar)

par(fig = c(0, 0.5, 0, 1))
plot_cap_env(  # plot CAP ordination (with environmental correlations)
  cap_obj = cap_func_plot, cap_dat = tree_lichens_func_plot,
  spp_cor_obj = cor_func_plot_spp, env_cor_obj = cor_cap_func_plot_env,
  delta_sq_obj = delta_sq_func_plot, fig_cex = 0.8,
  pt_sty_dat = pt_sty_site, plot_lab = "(a)", leg_title = "Site"
)

par(fig = c(0.5, 1, 0, 1), new = TRUE)
plot_cap_spp(  # plot species correlations with CAP axes
  spp_cor_obj = cor_func_plot_spp, spp_cor_obj_lab = cor_func_plot_spp,
  # (NB -- spp_cor_obj = `cor_cap_func_plot_spp` to plot all 'species' arrows)
  fig_cex = 0.8, plot_lab = "(b)"
)


dev.off()  # close .pdf graphics device




# ~~ lichen traits:

pdf(  # open .pdf graphics device
  './figs/cap_traits_plot.pdf',
  width = 18/2.54, height = 9/2.54
)


par( # set plotting parameters
  las = 1, mar = cap_mar)

par(fig = c(0, 0.5, 0, 1))
plot_cap_env(  # plot CAP ordination (with environmental correlations)
  cap_obj = cap_traits_plot, cap_dat = tree_lichens_func_plot,
  spp_cor_obj = cor_traits_plot_spp, env_cor_obj = cor_cap_traits_plot_env,
  delta_sq_obj = delta_sq_traits_plot, fig_cex = 0.8,
  pt_sty_dat = pt_sty_site, plot_lab = "(a)", leg_title = "Site"
)

par(fig = c(0.5, 1, 0, 1), new = TRUE)
plot_cap_spp(  # plot species correlations with CAP axes
  spp_cor_obj = cor_traits_plot_spp, spp_cor_obj_lab = cor_traits_plot_spp,
  # (NB -- spp_cor_obj = `cor_cap_traits_plot_spp` to plot all 'species' arrows)
  fig_cex = 0.8, plot_lab = "(b)"
)


dev.off()  # close .pdf graphics device




################################################################################

if (supp) {  # if 'supplementary' (i.e. 'tree-level') analyses have been run,
  
  # B. SUPPLEMENTARY PLOTS =======================================================
  
  # 1. Boxplots of lichen diversity vs. site ('tree-level') ----------------------
  
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
  
  # ~~ evenness:
  # (same plot as above, except with different y variable)
  `boxplot_li_taxa_1-l` <- boxplot_li_taxa_s + aes(y = `1-L`) +
    ylim(c(NA, 1)) +  # set upper y limit only
    ylab(expression(paste("1-", lambda)))  # add correct y label
  
  # ~ functional groups:
  
  # ~~ FEve:
  boxplot_li_func_feve <-
    ggplot(dd_lichens_func, aes(x = `site`, y = `FEve`)) +
    geom_boxplot() +
    scale_x_discrete(name = "Site") +
    ylim(c(NA, 1)) +  # set upper y limit only
    geom_jitter(
      shape = 16, colour = grey(0.25), alpha = 0.5,
      position = position_jitter(width = 0.15, height = 0)) +
    theme_rob()
  
  # ~~ FDis:
  boxplot_li_func_fdis <- boxplot_li_func_feve + aes(y = `FDis`) +
    ylim(c(NA, 0.225))  # set upper y limit only
  
  
  
  
  # output boxplots:
  
  # ~ taxonomic groups:
  pdf(  # open .pdf graphics device
    './figs/boxplots_taxa.pdf',
    # set width/height in cm (convert from inches):
    width = 18/2.54, height = 8/2.54
  )
  
  # arrange richness and diversity plots side by side:
  grid.arrange(
    boxplot_li_taxa_s, boxplot_li_taxa_h, `boxplot_li_taxa_1-l`,
    nrow = 1
  )
  
  dev.off()  # close .pdf graphics device
  
  
  # ~ functional groups:
  pdf(  # open .pdf graphics device
    './figs/boxplots_func.pdf',
    width = 18/2.54, height = 8/2.54
  )
  
  grid.arrange(
    boxplot_li_func_feve, boxplot_li_func_fdis,
    nrow = 1
  )
  
  dev.off()  # close .pdf graphics device
  
  
  
  
  # 2. MDS ordination plots ('tree-level') ---------------------------------------
  # (NB -- MDS not converged!)
  
  pdf(  # open .pdf graphics device
    './figs/mds.pdf',
    width = 18/2.54, height = 9/2.54
  )
  
  
  par( # set plotting parameters
    las = 1, mar = mds_mar)
  
  
  # ~ taxonomic groups:
  
  par(fig = c(0, 1/3, 0, 1))
  plot_mds(  # plot MDS
    mds_obj = mds_li_taxa,
    pt_sty_dat = pt_sty_site, pt_sty_var = "site",
    pt_ref_dat = dd_tree_lichens_taxa,
    legend = TRUE, leg_title = "Site", plot_lab = "(a)"
  )
  
  ordiellipse(  # add ellipses for site groups
    mds_li_taxa, dd_tree_lichens_taxa$site,
    draw = "line", col = as.character(pt_sty_site$col)
  )
  
  
  # ~ functional groups:
  
  par(fig = c(1/3, 2/3, 0, 1), new = TRUE)
  plot_mds(  # plot MDS
    mds_obj = mds_li_func,
    pt_sty_dat = pt_sty_site, pt_sty_var = "site",
    pt_ref_dat = dd_tree_lichens_func,
    plot_lab = "(b)"
  )
  
  ordiellipse(  # add ellipses for site groups
    mds_li_func, dd_tree_lichens_func$site,
    draw = "line", col = as.character(pt_sty_site$col)
  )
  
  
  # ~ lichen traits:
  
  par(fig = c(2/3, 1, 0, 1), new = TRUE)
  plot_mds(  # plot MDS
    mds_obj = mds_li_traits,
    pt_sty_dat = pt_sty_site, pt_sty_var = "site",
    pt_ref_dat = dd_tree_lichens_func,
    plot_lab = "(c)"
  )
  
  ordiellipse(  # add ellipses for site groups
    mds_li_traits, dd_tree_lichens_func$site,
    draw = "line", col = as.character(pt_sty_site$col)
  )
  
  
  dev.off()  # close .pdf graphics device
  
  
  
  
  # 3. CAP ordination plots ('tree-level') ---------------------------------------
  
  # ~~ taxonomic groups:
  
  pdf(  # open .pdf graphics device
    './figs/cap_taxa.pdf',
    width = 18/2.54, height = 9/2.54
  )
  
  
  par( # set plotting parameters
    las = 1, mar = cap_mar)
  
  par(fig = c(0, 0.5, 0, 1))
  plot_cap_env(  # plot CAP ordination (without environmental correlations)
    cap_obj = cap_taxa, cap_dat = dd_tree_lichens_taxa,
    spp_cor_obj = cor_taxa_spp, # env_cor_obj = cor_cap_taxa_env,
    delta_sq_obj = delta_sq_taxa, fig_cex = 0.8,
    pt_sty_dat = pt_sty_site, plot_lab = "(a)", leg_title = "Site"
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
  plot_cap_env(  # plot CAP ordination (without environmental correlations)
    cap_obj = cap_func, cap_dat = dd_tree_lichens_func,
    spp_cor_obj = cor_func_spp, # env_cor_obj = cor_cap_func_env,
    delta_sq_obj = delta_sq_func, fig_cex = 0.8,
    pt_sty_dat = pt_sty_site, plot_lab = "(a)", leg_title = "Site"
  )
  
  par(fig = c(0.5, 1, 0, 1), new = TRUE)
  plot_cap_spp(  # plot species correlations with CAP axes
    spp_cor_obj = cor_cap_func_spp, spp_cor_obj_lab = cor_func_spp,
    fig_cex = 0.8, plot_lab = "(b)"
  )
  
  
  dev.off()  # close .pdf graphics device
  
  
  
  
  # ~~ lichen traits:
  
  pdf(  # open .pdf graphics device
    './figs/cap_traits.pdf',
    width = 18/2.54, height = 9/2.54
  )
  
  
  par( # set plotting parameters
    las = 1, mar = cap_mar)
  
  par(fig = c(0, 0.5, 0, 1))
  plot_cap_env(  # plot CAP ordination (without environmental correlations)
    cap_obj = cap_traits, cap_dat = dd_tree_lichens_func,
    spp_cor_obj = cor_traits_spp, # env_cor_obj = cor_cap_func_env,
    delta_sq_obj = delta_sq_traits, fig_cex = 0.8,
    pt_sty_dat = pt_sty_site, plot_lab = "(a)", leg_title = "Site"
  )
  
  par(fig = c(0.5, 1, 0, 1), new = TRUE)
  plot_cap_spp(  # plot species correlations with CAP axes
    spp_cor_obj = cor_cap_traits_spp, spp_cor_obj_lab = cor_traits_spp,
    fig_cex = 0.8, plot_lab = "(b)"
  )
  
  
  dev.off()  # close .pdf graphics device
  
}
