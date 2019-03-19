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
