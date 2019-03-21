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
  # # remove `D810` (empty duplicate) and rename `D810_1` to `D810`:
  # # (NB -- only if this column actually represents a duplicate;
  # # OR is one `D810` column supposed to be `D710`???)
  # select(-`D810`) %>% rename(`D810` = `D810_1`) %>%
  # replace all NA values with 0:
  replace(is.na(.), 0) %>%
  # remove `Fam code` and rename `Genus code` to `taxon`:
  select(-`Fam code`) %>% rename(`taxon` = `Genus code`) %>%
  # transpose to make rows = samples and columns = taxa:
  gather(`tree`, `n`, `D11`:`SF12`) %>% spread(`taxon`, `n`) %>%
  # create factor `site` based on first character of `tree` values:
  mutate(`site` = factor(str_sub(`tree`, end = 1))) %>%
  # add `dummy` variable for zero-adjusted Bray-Curtis:
  mutate(`dummy` = 1) %>%
  # re-order columns so that `site` is at the beginning:
  select(`site`, `tree`, lichen_taxa, `dummy`)

# ~ functional groups:

# create vector of unique lichen functional groups:
lichen_func_grps <- unique(ddR_lichens_func$`tree no.`)

dd_lichens_func <-  # create new data frame
  ddR_lichens_func %>%
  # rename `tree no.` to `func_grp`:
  rename(`func_grp` = `tree no.`) %>%
  # replace all NA values with 0:
  replace(is.na(.), 0) %>%
  # sum all numeric variables by `func_grp`:
  # (NB -- only if the 2 'crsco' values represent duplicates)
  group_by(`func_grp`) %>% summarise_if(is.numeric, sum) %>%
  # transpose to make rows = samples and columns = taxa:
  gather(`tree`, `n`, `D11`:`SF12`) %>% spread(`func_grp`, `n`) %>%
  # create factor `site` based on first character of `tree` values:
  mutate(`site` = factor(str_sub(`tree`, end = 1))) %>%
  # add `dummy` variable for zero-adjusted Bray-Curtis:
  mutate(`dummy` = 1) %>%
  # re-order columns so that `site` is at the beginning:
  select(`site`, `tree`, lichen_func_grps, `dummy`)




# tree functional trait data:

dd_trees_func <-  # create new data frame
  ddR_trees_func %>%
  # rename all variables (except `girth`):
  rename(`tree` = 1, `bark` = 3, `buttress` = 4, `func_grp` = 5) %>%
  # create variable `site` based on first character of `tree` values:
  mutate(`site` = str_sub(`tree`, end = 1)) %>%
  # recode `bark` to numerical (ordinal) based on relative roughness:
  mutate(`bark_ord` = recode(
    `bark`, 'S' = 0, 'C' = 1, 'R' = 2, 'DR' = 3)) %>%
  # change relevant 'character' variables to 'factors':
  mutate_at(c('bark', 'func_grp', 'site'), factor)

# create vector of unique tree numbers:
tree_nos <- unique(dd_trees_func$tree)

# determine which trees are missing from either lichen dataset:
missing_trees <- tree_nos[
  !tree_nos %in% dd_lichens_func$tree |
    !tree_nos %in% dd_lichens_taxa$tree
  ]




# Manipulate data ===================================================

# ~ Calculate lichen diversity indices ------------------------------

dd_lichens_taxa <-  # taxonomic groups
  dd_lichens_taxa %>% mutate(
    `S` = specnumber(.[, lichen_taxa]),  # taxonomic richness
    `H'` = diversity(.[, lichen_taxa], "shannon")  # Shannon-Wiener
  )

dd_lichens_func <-  # functional groups
  dd_lichens_func %>% mutate(
    `S` = specnumber(.[, lichen_func_grps]),  # func. group richness
    `H'` = diversity(.[, lichen_func_grps], "shannon")
  )




# ~ Combine lichen and tree datasets --------------------------------

dd_tree_lichens_taxa <-  # taxonomic groups
  # perform 'left join' with tree dataset as 'x'
  left_join(dd_trees_func, dd_lichens_taxa, by = c('tree', 'site')) %>%
  # remove rows for which there are no lichen taxonomic group data:
  filter(is.na(rowSums(.[, lichen_taxa])) == FALSE)

dd_tree_lichens_func <-  # functional groups
  left_join(dd_trees_func, dd_lichens_func, by = c('tree', 'site')) %>%
  # remove rows for which there are no lichen functional group data:
  filter(is.na(rowSums(.[, lichen_func_grps])) == FALSE)




# ~ Subset environmental variables ------------------------------------

# create vector of continuous/ordinal tree trait variables:
env_vars <- c('girth', 'bark_ord', 'buttress')




env_cor <-  # test correlations between environmental variables
  rcorr(as.matrix(dd_trees_func[, env_vars]), type = "spearman")
# exclude if |\rho| > 0.7 ... currently, all values < 0.7
# create vector of variables to use in subsequent analyses:
env_use <- env_vars  # (currently no variables are excluded)




# BIOENV analysis:

# ~ taxonomic groups:
bioenv_taxa <-
  bioenv(  # run BIOENV analysis
    # log10(x+1)-transformed communit data, including 'dummy' variable
    # to enable calculation of zero-adjusted Bray-Curtis:
    log10(dd_tree_lichens_taxa[, c(lichen_taxa, 'dummy')] + 1),
    # vs. z-standardised environmental data:
    z_std(dd_tree_lichens_taxa[, env_vars]),
    method = "spearman", index = "bray", metric = "euclidean")

bioenv_taxa_rho <-  # extract correlation coefficient
  max(summary(bioenv_taxa)$correlation)
bioenv_vars_taxa <-  # extract variables to use in CAP
  summary(bioenv_taxa)$variables[bioenv_taxa$whichbest]
bioenv_vars_taxa <- strsplit(bioenv_vars_taxa, " ")[[1]]
cond_vars_taxa <-  # specify conditioning variables
  env_vars[-which(env_vars %in% bioenv_vars_taxa)]

# ~ functional groups:
bioenv_func <-
  bioenv(
    log10(dd_tree_lichens_func[, c(lichen_func_grps, 'dummy')] + 1),
    z_std(dd_tree_lichens_func[, env_vars]),
    method = "spearman", index = "bray", metric = "euclidean")

bioenv_func_rho <-  # extract correlation coefficient
  max(summary(bioenv_func)$correlation)
bioenv_vars_func <-  # extract variables to use in CAP
  summary(bioenv_func)$variables[bioenv_func$whichbest]
bioenv_vars_func <- strsplit(bioenv_vars_func, " ")[[1]]
cond_vars_func <-  # specify conditioning variables
  env_vars[-which(env_vars %in% bioenv_vars_func)]
