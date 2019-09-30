# sabah_lichens
# Summarise data

# Calculate summary statistics per site =============================

# ~ No. of observations ---------------------------------------------

# lichen abundance data:
# ~ taxonomic groups:
nobs_site_li_taxa <- dd_lichens_taxa %>% count(`site`)
# ~ functional groups:
nobs_site_li_func <- dd_lichens_func %>% count(`site`)

# tree functional trait data:
nobs_site_tr_func <- dd_trees_func %>% count(`site`)




# ~ Mean lichen diversity -------------------------------------------

# taxonomic groups:
mean_div_li_taxa <- dd_lichens_taxa %>%
  group_by(`site`) %>%  # group by site
  # calculate mean and SE diversity and richness:
  summarise(
    `mean_H'` = mean(`H'`), `SE_H'` = std.error(`H'`),
    `mean_S` = mean(`S`), `SE_S` = std.error(`S`)
  )

# functional groups:
mean_div_li_func <- dd_lichens_func %>%
  group_by(`site`) %>%
  summarise(
    `mean_H'` = mean(`H'`), `SE_H'` = std.error(`H'`),
    `mean_S` = mean(`S`), `SE_S` = std.error(`S`)
  )
