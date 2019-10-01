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
