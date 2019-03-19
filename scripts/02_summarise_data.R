# sabah_lichens
# Summarise data

# No. of observations -----------------------------------------------

# lichen abundance data:
# ~ taxonomic groups:
nobs_site_li_taxa <- dd_lichens_taxa %>% count(`site`)
# ~ functional groups:
nobs_site_li_func <- dd_lichens_func %>% count(`site`)
# NB -- the 'func' dataset has one fewer observation in habitat 'S',
# relating to missing data for tree 'SA4'

# tree functional trait data:
nobs_site_tr_func <- dd_trees_func %>% count(`site`)




# Mean lichen diversity ---------------------------------------------

mean_div_li_taxa <-  # taxonomic groups
  dd_lichens_taxa %>%
  group_by(`site`) %>% summarise(
    `mean_H'` = mean(`H'`), `mean_S` = mean(`S`)
  )

mean_div_li_func <-  # functional groups
  dd_lichens_func %>%
  group_by(`site`) %>% summarise(
    `mean_H'` = mean(`H'`), `mean_S` = mean(`S`)
  )
