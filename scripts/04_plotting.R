# sabah_lichens
# Plotting

# Boxplots of lichen diversity vs. site =============================

# create boxplots:

# ~ taxonomic groups:
# ~~ richness:
boxplot_li_taxa_s <-  # assign to object
  ggplot(dd_lichens_taxa, aes(x = `site`, y = `S`)) +
  geom_boxplot() +
  geom_jitter(  # overlay data points
    shape = 16, position = position_jitter(0.2),
    colour = "blue", alpha = 0.5)
# ~~ diversity:
# (same plot as above, except with different y variable)
boxplot_li_taxa_h <- boxplot_li_taxa_s + aes(y = `H'`)

# ~ functional groups:
boxplot_li_func_s <-
  ggplot(dd_lichens_func, aes(x = `site`, y = `S`)) +
  geom_boxplot() +
  geom_jitter(
    shape = 16, position = position_jitter(0.1),
    colour = "blue", alpha = 0.5)
# ~~ diversity:
boxplot_li_func_h <- boxplot_li_func_s + aes(y = `H'`)




# output boxplots:

# ~ taxonomic groups:
pdf(  # open .pdf graphics device
  './figs/boxplots_taxa.pdf',
  # set width/height in cm (convert from inches):
  width = 18/2.54, height = 9/2.54)

# arrange richness and diversity plots side by side:
grid.arrange(boxplot_li_taxa_s, boxplot_li_taxa_h, ncol = 2)

dev.off()  # close .pdf graphics device


# ~ functional groups:
pdf(  # open .pdf graphics device
  './figs/boxplots_func.pdf',
  width = 18/2.54, height = 9/2.54)

grid.arrange(boxplot_li_func_s, boxplot_li_func_h, ncol = 2)

dev.off()  # close .pdf graphics device
