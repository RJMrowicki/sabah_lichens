# sabah_lichens
# Summarise data

# Calculate summary statistics per site ========================================

# ~ No. of observations --------------------------------------------------------

# lichen abundance data:
# ~ taxonomic groups:
nobs_site_li_taxa <- dd_lichens_taxa %>% count(`site`)
# ~ functional groups:
nobs_site_li_func <- dd_lichens_func %>% count(`site`)

# tree functional trait data:
nobs_site_tr_func <- dd_trees_func %>% count(`site`)




# ~ Mean lichen diversity ------------------------------------------------------

# ~~ Tree-level ----------------------------------------------------------------

# taxonomic groups:
mean_div_li_taxa <-
  dd_lichens_taxa %>%
  # group by site:
  group_by(`site`) %>%
  # calculate mean & SE diversity, richness and evenness:
  summarise(
    `mean_S` = mean(`S`), `SE_S` = std.error(`S`),
    `mean_1-L` = mean(`1-L`), `SE_1-L` = std.error(`1-L`),
    `mean_H'` = mean(`H'`), `SE_H'` = std.error(`H'`)
  )

# functional groups:
mean_div_li_func <-
  dd_lichens_func %>%
  # group by site:
  group_by(`site`) %>%
  # calculate mean & SE distance-based FD indices:
  summarise(
    `mean_FEve` = mean(`FEve`, na.rm = TRUE), `SE_FEve` = std.error(`FEve`),
    `mean_FDis` = mean(`FDis`, na.rm = TRUE), `SE_FDis` = std.error(`FDis`)
  )




# ~~ Plot-level ----------------------------------------------------------------

# taxonomic groups:
mean_div_li_taxa_plot <-
  tree_lichens_taxa_plot %>%
  # group by site:
  group_by(`site`) %>%
  # calculate mean & SE diversity, richness and evenness:
  summarise(
    `mean_S` = mean(`S`), `SE_S` = std.error(`S`),
    `mean_1-L` = mean(`1-L`), `SE_1-L` = std.error(`1-L`),
    `mean_H'` = mean(`H'`), `SE_H'` = std.error(`H'`)
  )

# functional groups:
mean_div_li_func_plot <-
  tree_lichens_func_plot %>%
  # group by site
  group_by(`site`) %>%
  # calculate mean & SE distance-based FD indices:
  summarise(
    `mean_FRic` = mean(`FRic`, na.rm = TRUE), `SE_FRic` = std.error(`FRic`),
    `mean_FEve` = mean(`FEve`, na.rm = TRUE), `SE_FEve` = std.error(`FEve`),
    `mean_FDiv` = mean(`FDiv`, na.rm = TRUE), `SE_FDiv` = std.error(`FDiv`),
    `mean_FDis` = mean(`FDis`, na.rm = TRUE), `SE_FDis` = std.error(`FDis`)
  )




# ~ Mean tree functional traits (plot-level) -----------------------------------

mean_tr_func_plot <-
  tree_lichens_taxa_plot %>% 
  # group by site
  group_by(`site`) %>%
  # calculate mean & SE plot-summarised tree functional trait values:
  summarise(
    `mean_bark_div` = mean(`bark_div`, na.rm = TRUE),
    `SE_bark_div` = std.error(`bark_div`),
    `mean_pH_div` = mean(`pH_div`, na.rm = TRUE),
    `SE_pH_div` = std.error(`pH_div`),
    `mean_girth_l_prop` = mean(`girth_l_prop`, na.rm = TRUE),
    `SE_girth_l_prop` = std.error(`girth_l_prop`),
    `mean_buttress_prop` = mean(`buttress_prop`, na.rm = TRUE),
    `SE_buttress_prop` = std.error(`buttress_prop`),
    `mean_dipterocarp_prop` = mean(`dipterocarp_prop`, na.rm = TRUE),
    `SE_dipterocarp_prop` = std.error(`dipterocarp_prop`)
  )

