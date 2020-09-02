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
  # calculate mean and SE diversity and richness:
  summarise(
    `mean_H'` = mean(`H'`), `SE_H'` = std.error(`H'`),
    `mean_S` = mean(`S`), `SE_S` = std.error(`S`)
  )

# functional groups:
mean_div_li_func <-
  dd_lichens_func %>%
  # group by site:
  group_by(`site`) %>%
  # calculate mean and SE diversity, richness and distance-based FD indices:
  summarise(
    `mean_H'` = mean(`H'`), `SE_H'` = std.error(`H'`),
    `mean_S` = mean(`S`), `SE_S` = std.error(`S`),
    `mean_FEve` = mean(`FEve`, na.rm = TRUE), `SE_FEve` = std.error(`FEve`),
    `mean_FDis` = mean(`FDis`, na.rm = TRUE), `SE_FDis` = std.error(`FDis`)
  )




# ~~ Plot-level ----------------------------------------------------------------

# taxonomic groups:
mean_div_li_taxa_plot <-
  tree_lichens_taxa_plot %>%
  # group by site:
  group_by(`site`) %>%
  # calculate mean and SE diversity and richness:
  summarise(
    `mean_H'` = mean(`H'`), `SE_H'` = std.error(`H'`),
    `mean_S` = mean(`S`), `SE_S` = std.error(`S`)
  )

# functional groups:
mean_div_li_func_plot <-
  tree_lichens_func_plot %>%
  # group by site
  group_by(`site`) %>%
  # calculate mean and SE diversity, richness and distance-based FD indices:
  summarise(
    `mean_H'` = mean(`H'`), `SE_H'` = std.error(`H'`),
    `mean_S` = mean(`S`), `SE_S` = std.error(`S`),
    `mean_FRic` = mean(`FRic`, na.rm = TRUE), `SE_FRic` = std.error(`FRic`),
    `mean_FEve` = mean(`FEve`, na.rm = TRUE), `SE_FEve` = std.error(`FEve`),
    `mean_FDiv` = mean(`FDiv`, na.rm = TRUE), `SE_FDiv` = std.error(`FDiv`),
    `mean_FDis` = mean(`FDis`, na.rm = TRUE), `SE_FDis` = std.error(`FDis`)
  )
