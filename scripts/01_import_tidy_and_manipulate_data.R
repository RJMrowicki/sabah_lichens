# sabah_lichens
# Import, tidy and manipulate data

# Import data =======================================================

# lichen abundance data:
# ~ taxonomic groups:
ddR_lichens_taxa <- read_csv('./data/lichens_taxa.csv')
# ~ functional groups:
ddR_lichens_func <- read_csv('./data/lichens_func.csv')

# tree functional trait data:
ddR_trees_func <- read_csv('./data/trees_func.csv')





# Tidy data =========================================================

# lichen abundance data:
# ~ taxonomic groups:

# create vector of unique lichen taxa:
lichen_taxa <- unique(ddR_lichens_taxa$`Genus code`)

dd_lichens_taxa <-  # create new data frame
  ddR_lichens_taxa %>%
  # rename `D810` to `D710` and `D810_1` to `D810`:
  # (NB -- assuming that `D710` is mistakenly named `D810`)
  rename(`D710` = `D810`, `D810` = `D810_1`) %>%
  # # OR remove `D810` (empty duplicate) and rename `D810_1` to `D810`:
  # select(-`D810`) %>% rename(`D810` = `D810_1`) %>%
  # replace all NA values with 0:
  replace(is.na(.), 0) %>%
  # remove `Fam code` and rename `Genus code` to `taxon`:
  select(-`Fam code`) %>% rename(`taxon` = `Genus code`) %>%
  # transpose to make rows = samples and columns = taxa:
  gather(`tree`, `n`, `D11`:`SF12`) %>% spread(`taxon`, `n`) %>%
  # create factors `site` and `plot` based on characters of `tree`:
  mutate(
    `site` = factor(str_sub(`tree`, end = 1)),
    `plot` = factor(str_sub(`tree`, end = 2))
  ) %>%
  # re-arrange rows by site, plot and tree:
  arrange(`site`, `plot`, `tree`) %>%
  # re-order columns so that `site`, `plot` and `tree` are at the beginning:
  select(`site`, `plot`, `tree`, lichen_taxa)

# ~ functional groups:

# create vector of unique lichen functional groups:
lichen_func_grps <- unique(ddR_lichens_func$`tree no.`)

dd_lichens_func <-  # create new data frame
  ddR_lichens_func %>%
  # rename `D810` to `D710` and `D810_1` to `D810`:
  # (NB -- assuming that `D710` is mistakenly named `D810`)
  rename(`D710` = `D810`, `D810` = `D810_1`) %>%
  # # OR remove `D810` (empty duplicate) and rename `D810_1` to `D810`:
  # select(-`D810`) %>% rename(`D810` = `D810_1`) %>%
  # rename `tree no.` to `func_grp`:
  rename(`func_grp` = `tree no.`) %>%
  # replace all NA values with 0:
  replace(is.na(.), 0) %>%
  # sum all numeric variables by `func_grp`:
  # (NB -- only if duplicate func_grp values are actual duplicates)
  group_by(`func_grp`) %>% summarise_if(is.numeric, sum) %>%
  # transpose to make rows = samples and columns = taxa:
  gather(`tree`, `n`, `D11`:`SF12`) %>% spread(`func_grp`, `n`) %>%
  # create factors `site` and `plot` based on characters of `tree`:
  mutate(
    `site` = factor(str_sub(`tree`, end = 1)),
    `plot` = factor(str_sub(`tree`, end = 2))
  ) %>%
  # re-arrange rows by site, plot and tree:
  arrange(`site`, `plot`, `tree`) %>%
  # re-order columns so that `site`, `plot` and `tree` are at the beginning:
  select(`site`, `plot`, `tree`, `lichen_func_grps`)




# tree functional trait data:

dd_trees_func <-  # create new data frame
  ddR_trees_func %>%
  # rename all variables (except `girth`):
  rename(`tree` = 1, `bark` = 3, `buttress` = 4, `func_grp` = 5) %>%
  # create factors `site`, `plot` and `tree` based on characters of `tree`:
  mutate(
    `site` = factor(str_sub(`tree`, end = 1)),
    `plot` = factor(str_sub(`tree`, end = 2))
  ) %>%
  # recode `bark` to numerical (ordinal) based on relative roughness:
  # (NB -- specify 'dplyr::' as opposed to 'car::')
  mutate(`bark_ord` = dplyr::recode(
    `bark`, 'DR' = 1, 'S' = 2, 'C' = 3, 'R' = 4)) %>%
  # change relevant 'character' variables to 'factors':
  mutate_at(vars(`bark`, `func_grp`, `site`), factor) %>%
  # re-arrange rows by site, plot and tree:
  arrange(`site`, `plot`, `tree`) %>%
  # re-order columns so that `site`, `plot` and `tree` are at the beginning:
  select(`site`, `plot`, `tree`, everything())

# create vector of unique tree numbers:
tree_nos <- unique(dd_trees_func$tree)

# determine which trees are missing from either lichen dataset:
missing_trees <- tree_nos[
  !tree_nos %in% dd_lichens_func$tree |
    !tree_nos %in% dd_lichens_taxa$tree
  ]




# Manipulate data ===================================================

# ~ Calculate lichen diversity indices ------------------------------

# taxonomic groups:
dd_lichens_taxa <- dd_lichens_taxa %>%
  mutate(  # vegan:specnumber(), vegan::diversity()
    `S` = specnumber(.[, lichen_taxa]),  # taxonomic richness (S)
    `H'` = diversity(.[, lichen_taxa], "shannon")  # Shannon-Wiener (H')
  )

# functional groups:
dd_lichens_func <- dd_lichens_func %>%
  mutate(
    `S` = specnumber(.[, lichen_func_grps]),  # func. group richness
    `H'` = diversity(.[, lichen_func_grps], "shannon")  # Shannon-Wiener (H')
  )




# ~ Combine lichen and tree datasets --------------------------------

dd_tree_lichens_taxa <-  # taxonomic groups
  # perform 'left join' with tree dataset as 'x':
  left_join(dd_trees_func, dd_lichens_taxa) %>%
  # remove rows for which there are no lichen taxonomic group data:
  filter(is.na(rowSums(.[, lichen_taxa])) == FALSE)

dd_tree_lichens_func <-  # functional groups
  # perform 'left join' with tree dataset as 'x':
  left_join(dd_trees_func, dd_lichens_func) %>%
  # remove rows for which there are no lichen functional group data:
  filter(is.na(rowSums(.[, lichen_func_grps])) == FALSE)

# create separate `dummy` species vectors for zero-adjusted Bray-Curtis:
dummy_taxa <- rep(1, nrow(dd_tree_lichens_taxa))  # abundance of 1
dummy_func <- rep(1, nrow(dd_tree_lichens_func))
