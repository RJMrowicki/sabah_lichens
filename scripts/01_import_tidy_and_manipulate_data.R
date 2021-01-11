# sabah_lichens
# Import, tidy and manipulate data

# Import data ==================================================================

# lichen abundance data:
# ~ taxonomic groups:
ddR_lichens_taxa <- read_csv('./data/lichens_taxa.csv')
# ~ functional groups:
ddR_lichens_func <- read_csv('./data/lichens_func.csv')

# lichen functional traits matrix:
# (NB -- skip first line of 'dummy' column headers)
ddR_lichen_traits <- read_csv('./data/lichen_traits.csv', skip = 1)

# tree functional trait data:
ddR_trees_func <- read_csv('./data/trees_func.csv')
ddR_trees_pH <- read_csv('./data/trees_pH.csv')




# Tidy data ====================================================================

# lichen abundance data:
# ~ taxonomic groups:

# create ordered vector of unique lichen taxa:
lichen_taxa <- unique(ddR_lichens_taxa$`Genus code`) %>% na.omit %>% sort

dd_lichens_taxa <-  # create new data frame
  ddR_lichens_taxa %>%
  # convert 'logical' (i.e. empty numeric) columns to numeric:
  mutate_if(is.logical, as.numeric) %>% 
  # rename `D810` to `D710` and `D810_1` to `D810`:
  # (NB -- assuming that `D710` is mistakenly named `D810`)
  rename(`D710` = `D810`, `D810` = `D810_1`) %>%
  # # OR remove `D810` (empty duplicate) and rename `D810_1` to `D810`:
  # select(-`D810`) %>% rename(`D810` = `D810_1`) %>%
  # remove `Fam code` and rename `Genus code` to `taxon`:
  select(-`Fam code`) %>% rename(`taxon` = `Genus code`) %>%
  # replace all NA values with 0:
  replace(is.na(.), 0) %>%
  # transpose to make rows = samples and columns = taxa:
  gather(`tree`, `n`, -`taxon`) %>% spread(`taxon`, `n`) %>%
  # create factors `site` and `plot` based on characters of `tree`:
  mutate(
    `site` = factor(str_sub(`tree`, end = 1)),
    `plot` = factor(str_sub(`tree`, end = 2))
  ) %>%
  # re-arrange rows by site, plot and tree:
  arrange(`site`, `plot`, `tree`) %>%
  # re-order columns so that `site`, `plot` and `tree` are at the beginning:
  select(`site`, `plot`, `tree`, all_of(lichen_taxa))




# ~ functional groups:

# create ordered vector of unique lichen functional groups:
lichen_func_grps <- unique(ddR_lichens_func$`tree no.`) %>% na.omit %>% sort

dd_lichens_func <-  # create new data frame
  ddR_lichens_func %>%
  # convert 'logical' (i.e. empty numeric) columns to numeric:
  mutate_if(is.logical, as.numeric) %>% 
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
  gather(`tree`, `n`, -`func_grp`) %>% spread(`func_grp`, `n`) %>%
  # create factors `site` and `plot` based on characters of `tree`:
  mutate(
    `site` = factor(str_sub(`tree`, end = 1)),
    `plot` = factor(str_sub(`tree`, end = 2))
  ) %>%
  # re-arrange rows by site, plot and tree:
  arrange(`site`, `plot`, `tree`) %>%
  # re-order columns so that `site`, `plot` and `tree` are at the beginning:
  select(`site`, `plot`, `tree`, all_of(lichen_func_grps))




# lichen functional traits matrix:
dd_lichen_traits <-  # create new data frame
  ddR_lichen_traits %>%
  # select and rename columns:
  dplyr::select(
    `code` = 1,
    `growth_type` = 3, `thallus_pigments` = 4, `photobiont` = 5,
    `sexual_dispersal` = 6, `fruiting_stalk` = 7, `mazaedia` = 8,
    `vegetative_dispersal` = 9, `surface_byssoid` = 10,
    `fimbriate_prothallus` = 11, `hypothallus` = 12) %>%
  # convert non-binary traits into factors (as required by `FD::dbFD()`):
  mutate_at(
    vars(c(`growth_type`, `photobiont`, `sexual_dispersal`)),
    as_factor) %>%
  # ensure all binary (i.e. numeric, in this case) traits are coded properly:
  mutate_if(
    is_double, ~ { (. > 0) + 0 }) %>%
  # arrange rows alphabetically by functional group code:
  arrange(`code`)

# create vector of lichen trait names:
lichen_traits <- names(dplyr::select(dd_lichen_traits, -`code`))




# tree functional trait data:

# tidy tree pH data, for later merging with tree functional trait data:
dd_trees_pH <-  # create new data frame
  ddR_trees_pH[-nrow(ddR_trees_pH), ] %>%
  # convert relevant `pH` variables to numeric (removes text values):
  mutate_at(vars(c(`pH1`, `pH2`, `pH3`)), as.numeric) %>%
  # rename, manipulate and select `site`, `plot` and `tree` variables,
  # and calculate mean tree pH:
  transmute(
    `site` = `Site`,
    `plot` = paste0(`Site`, `Plot`), `tree` = gsub("_", "", `Tree_code`),
    `pH_mean` = rowMeans(select(., `pH1`, `pH2`, `pH3`), na.rm = TRUE)) %>%
  # convert `site` and `plot` to factors:
  mutate_at(vars(c(`site`, `plot`)), as.factor) %>%
  # convert 'NaN' (not a number) to 'NA' (missing) pH values:
  mutate_at(vars(`pH_mean`), ~ ifelse(is.nan(.), NA_real_, .))

# tidy rest of tree functional trait data, add mean pH values:
dd_trees_func <-  # create new data frame
  ddR_trees_func %>%
  # select (remove functional group code) and rename variables:
  select(
    `tree` = 1,
    `girth_m` = 2, `bark` = 3, `buttress_ord` = 4, `dipterocarp` = 6) %>%
  # create factors `site` and `plot` based on characters of `tree`:
  mutate(
    `site` = factor(str_sub(`tree`, end = 1)),
    `plot` = factor(str_sub(`tree`, end = 2))) %>%
  # create `girth` categorical variable (factor), based on
  # `girth_m` cutoff values of 100 and 200 (a >= x < b):
  mutate(
    `girth` = factor(cut(
      `girth_m`, breaks = c(-Inf, 100, 200, Inf),
      labels = c("s", "m", "l"), right = FALSE))) %>%
  # recode `bark` to numerical (ordinal) based on relative water holding:
  # (NB -- specify 'dplyr::' as opposed to 'car::')
  mutate(`bark_ord` = dplyr::recode(
    `bark`, 'DR' = 1, 'S' = 2, 'C' = 3, 'R' = 4)) %>%
  # create factor `buttress`, in addition to 'ordinal' variable:
  mutate(`buttress` = factor(buttress_ord)) %>%
  # add mean pH values:
  left_join(dd_trees_pH, by = c("site", "plot", "tree")) %>%
  # create `pH` categorical variable (factor), based on
  # `pH_mean` quartile cut-off values (a >= x < b):
  mutate(
    `pH` = factor(cut(
      `pH_mean`, breaks = quantile(`pH_mean`, na.rm = TRUE),
      labels = c("vl", "l", "m", "h"), right = FALSE))) %>%
  # change relevant 'character'/'double' variables to 'factors':
  mutate_at(vars(`bark`, `dipterocarp`), factor) %>%
  # re-arrange rows by site, plot and tree:
  arrange(`site`, `plot`, `tree`) %>%
  # re-order columns so that `site`, `plot` and `tree` are at the beginning:
  select(
    `site`, `plot`, `tree`,
    `girth_m`, `bark_ord`, `buttress_ord`, `pH_mean`,  # continuous variables
    `girth`, `bark`, `buttress`, `dipterocarp`, `pH`,  # categorical variables
    everything())  # everything else (currently nothing)

# create vector of unique tree numbers:
tree_nos <- unique(dd_trees_func$tree)

# determine which trees are missing from either lichen dataset:
missing_trees <- tree_nos[
  !tree_nos %in% dd_lichens_func$tree |
    !tree_nos %in% dd_lichens_taxa$tree
  ]




# Manipulate data ==============================================================

# ~ Tree-level -----------------------------------------------------------------

# ~~ calculate lichen diversity indices ----------------------------------------

# ~ taxonomic groups:
dd_lichens_taxa <- dd_lichens_taxa %>%
  mutate(  # vegan:specnumber(), vegan::diversity()
    `S` = specnumber(.[, lichen_taxa]),  # taxonomic richness (S)
    `1-L` = 1 - (1/diversity(.[, lichen_taxa], "invsimpson")),  # Simpson (1-L)
    `H'` = diversity(.[, lichen_taxa], "shannon")  # Shannon-Wiener (H')
    
  )


# ~ functional groups:

# ~~ non-distance-based diversity indices:
dd_lichens_func0 <- dd_lichens_func %>%
  mutate(
    `S` = specnumber(.[, lichen_func_grps]),  # func. group richness
    `1-L` = 1 - (1/diversity(.[, lichen_func_grps], "invsimpson")),  # Simpson (1-L)
    `H'` = diversity(.[, lichen_func_grps], "shannon")  # Shannon-Wiener (H')
  )


# ~~ distance-based diversity indices:
# (NB -- FEve and FDis only [FEve not calculated where <3 singular 'species'];
# tree-level data insufficient for calculation of FRic and FDiv)

# determine lichen functional group codes shared between traits matrix and
# abundance data (i.e. included in calculation of Functional Diversity indices):
# (NB -- should include ALL codes; if not, some func. grps in abundance data
# are not represented in traits matrix [or vice versa].)
lichen_func_grps_included <-
  intersect(lichen_func_grps, dd_lichen_traits$code)

# (functional group codes excluded from FD calculations:)
# (NB -- no lichen func. grps should be excluded; see previous comment.)
lichen_func_grps_excluded <-
  setdiff(lichen_func_grps, lichen_func_grps_included)

# subset lichen traits matrix:
lichen_traits_dbfd <-
  dd_lichen_traits %>%
  # remove data (rows) for func. groups not shared with traits matrix:
  filter(`code` %in% lichen_func_grps_included) %>%
  # convert functional group code column to rownames:
  column_to_rownames("code")

# subset lichen func. group abundance data:
lichens_func_dbfd <-
  dd_lichens_func0 %>%
  # remove data (columns) for func. groups not shared with traits matrix:
  dplyr::select(`tree`, all_of(lichen_func_grps_included)) %>%
  # remove data (rows) for trees with zero summed abundance:
  filter(rowSums(dplyr::select(., all_of(lichen_func_grps_included))) != 0)

# calculate distance-based FD indices & trait CWMs (for subsetted data):
# (NB -- suppress calculation of FRic and FDiv [via `calc.FRic = FALSE`]
# owing to convex hull calculation errors [`geometry::convexhulln()`];
# also, calculate trait CWM as abundance of each individual class ['all'],
# rather than dominant class ['dom' <default>], for categorical traits.)
lichens_dbfd <-
  dbFD(
    lichen_traits_dbfd, dplyr::select(lichens_func_dbfd, -`tree`),
    calc.FRic = FALSE, CWM.type = "all"
  )
if (file.exists("vert.txt")) {
  file.remove("vert.txt")  # remove 'vertices' file output by function
}

# add to (initial) data frame of lichen functional diversity indices:
dd_lichens_func <-
  dd_lichens_func0 %>%
  # left join by `tree` (combine with dbFD output first):
  left_join(
    bind_cols(
      enframe(lichens_func_dbfd$tree, name = NULL, value = "tree"),
      lichens_dbfd[c("FEve", "FDis")],  # dbFD indices
      as_tibble(lichens_dbfd$CWM)  # trait CWMs
    )
  ) %>%
  # replace NAs in trait CWM columns with 0:
  # (NB -- only if CWMs are calculated via 'all', NOT 'dom')
  mutate_at(vars(contains(lichen_traits)), ~ replace_na(., 0))




# ~~ tree functional trait data (add missing pH) -------------------------------

# # create lookup table of overall mean pH per bark type category:
# mean_pH_bark <-
#   dd_trees_func %>%
#   # group by bark type, calculate mean (mean)pH (NB -- excluding NAs):
#   group_by(bark) %>% summarise(pH_mean = mean(pH_mean, na.rm = TRUE))
# 
# # replace missing tree pH values with mean value for corresponding bark type:
# # (NB -- is this appropriate? Relies on a strong and justifiable relationship
# # between pH and bark type; also possible implications of non-independence/
# # collinearity between pH and bark type environmental variables.)
# dd_trees_func <-
#   dd_trees_func %>%
#   # if pH is NA, use matching mean value for bark type, else use existing value:
#   mutate(
#     `pH_mean` = if_else(
#       is.na(`pH_mean`),
#       mean_pH_bark$pH_mean[match(`bark`, mean_pH_bark$bark)],
#       `pH_mean`
#     )) %>%
#   # update `pH` categorical variable, using same `pH_mean` cut-offs as above:
#   mutate(
#     `pH` = factor(cut(
#       `pH_mean`, breaks = quantile(`pH_mean`, na.rm = TRUE),
#       labels = c("vl", "l", "m", "h"), right = FALSE)))




# ~~ combine lichen and tree datasets ------------------------------------------

dd_tree_lichens_taxa <-  # taxonomic groups
  # perform 'left join' with tree dataset as 'x':
  left_join(dd_trees_func, dd_lichens_taxa) %>%
  # remove rows for which there are no lichen taxonomic group data:
  filter(is.na(rowSums(.[, lichen_taxa])) == FALSE)

dd_tree_lichens_func <-  # functional groups
  # (NB -- includes abundances, dbFD indices AND trait CWMs)
  # perform 'left join' with tree dataset as 'x':
  left_join(dd_trees_func, dd_lichens_func) %>%
  # remove rows for which there are no lichen functional group data:
  filter(is.na(rowSums(.[, lichen_func_grps])) == FALSE)

# create separate `dummy` species vectors for zero-adjusted Bray-Curtis:
dummy_taxa <- rep(1, nrow(dd_tree_lichens_taxa))  # abundance of 1
dummy_func <- rep(1, nrow(dd_tree_lichens_func))




# ~ Plot-level -----------------------------------------------------------------

# ~~ sum lichen abundance data and calculate diversity indices: ----------------

# ~ taxonomic groups:
lichens_taxa_plot <- dd_lichens_taxa %>%
  # calculate summed abundances of lichen taxonomic groups per plot:
  group_by(`plot`) %>% summarise_at(lichen_taxa, sum) %>%
  # calculate per-plot taxonomic diversity indices:
  mutate(
    `S` = specnumber(.[, lichen_taxa]),  # taxonomic richness (S)
    `1-L` = 1 - (1/diversity(.[, lichen_taxa], "invsimpson")),  # Simpson (1-L)
    `H'` = diversity(.[, lichen_taxa], "shannon")  # Shannon-Wiener (H')
  )

# ~ functional groups:

# ~~ non-distance-based diversity indices:
lichens_func_plot0 <- dd_lichens_func %>%
  # calculate summed abundances of lichen functional groups per plot:
  group_by(`plot`) %>% summarise_at(lichen_func_grps, sum) %>%
  # calculate per-plot functional diversity indices:
  mutate(
    `S` = specnumber(.[, lichen_func_grps]),  # taxonomic richness (S)
    `1-L` = 1 - (1/diversity(.[, lichen_func_grps], "invsimpson")),  # Simpson (1-L)
    `H'` = diversity(.[, lichen_func_grps], "shannon"),  # Shannon-Wiener (H')
  )


# ~~ distance-based diversity indices:

# subset lichen func. group abundance data:
lichens_func_plot_dbfd <-
  lichens_func_plot0 %>%
  # remove data (columns) for func. groups not shared with traits matrix:
  dplyr::select(`plot`, all_of(lichen_func_grps_included))

# calculate distance-based FD indices (for subsetted data):
lichens_dbfd_plot <-
  dbFD(
    lichen_traits_dbfd, dplyr::select(lichens_func_plot_dbfd, -`plot`),
    CWM.type = "all"
  )
if (file.exists("vert.txt")) {
  file.remove("vert.txt")  # remove 'vertices' file output by function
}

# add to (initial) data frame of lichen functional diversity indices:
lichens_func_plot <-
  lichens_func_plot0 %>%
  # left join by `tree` (combine with dbFD output first):
  left_join(
    bind_cols(
      enframe(lichens_func_plot_dbfd$plot, name = NULL, value = "plot"),
      lichens_dbfd_plot[c("FRic", "FEve", "FDiv", "FDis")],  # dbFD indices
      as_tibble(lichens_dbfd_plot$CWM)  # trait CWMs
    )
  ) %>%
  # replace NAs in trait CWM columns with 0:
  # (NB -- only if CWMs are calculated via 'all', NOT 'dom')
  mutate_at(vars(contains(lichen_traits)), ~ replace_na(., 0))




# ~~ summarise tree functional trait data --------------------------------------

# calculate 'diversity' of tree trait categories per plot:
# ~ bark:
trees_func_plot_bark_div <- dd_trees_func %>%
  # obtain number of trees per bark category per plot:
  group_by(`plot`, `bark`) %>% tally %>%
  # re-group by `plot` only:
  group_by(`plot`) %>%
  # calculate Shannon-Wiener diversity of bark categories:
  summarise_at(vars(`n`), list(`bark_div` = ~diversity(., "shannon")))

# ~ pH:
trees_func_plot_pH_div <- dd_trees_func %>%
  # remove rows for which `pH` is NA:
  filter(!is.na(`pH`)) %>%
  # obtain number of trees per pH category per plot:
  group_by(`plot`, `pH`) %>% tally %>%
  # re-group by `plot` only:
  group_by(`plot`) %>%
  # calculate Shannon-Wiener diversity of bark categories:
  summarise_at(vars(`n`), list(`pH_div` = ~diversity(., "shannon")))


# # histograms of tree girth in logged (S) vs. unlogged (D, M) forests, to
# # determine cut-off for 'large' trees (point at which tree girth declines
# # dramatically in logged forest compared to unlogged forests -- 150 cm):
# dd_trees_func %>%
#   mutate(`group` = ifelse(`site` == "S", "logged", "unlogged")) %>% 
#   ggplot(aes(`girth_m`)) +
#   facet_wrap(~ `group`) +
#   geom_histogram(breaks = c(seq(0, 600, 50)), closed = "left")


# calculate proportions of girth >200cm, buttresses & dipterocarp trees:
trees_func_plot_props <- dd_trees_func %>%
  # first create factor for 'large' girth (i.e. >= 150 cm):
  mutate(`girth_l` = ifelse(`girth_m` >= 150, "1", "0")) %>%
  # group by plot and calculate respective proportions:
  group_by(`plot`) %>% summarise_at(
    vars(`girth_l`, `buttress`, `dipterocarp`),
    list(`prop` = ~length(which(. == "1")) / length(.))
    )


# combine into single data frame:
trees_func_plot <- reduce(
  list(trees_func_plot_bark_div, trees_func_plot_pH_div, trees_func_plot_props),
  left_join
  ) %>%
  # add column for `site` (distinct rows from main dataframe only):
  left_join(distinct(select(dd_trees_func, `plot`, `site`))) %>%
  # re-order columns so that `site` and `plot` are at the beginning:
  select(`site`, `plot`, everything())




# ~~ combine lichen and tree datasets ------------------------------------------

tree_lichens_taxa_plot <-  # taxonomic groups
  # perform 'left join' with tree dataset as 'x':
  left_join(trees_func_plot, lichens_taxa_plot)
  
tree_lichens_func_plot <-  # functional groups
  # perform 'left join' with tree dataset as 'x':
  left_join(trees_func_plot, lichens_func_plot)

# create separate `dummy` species vectors for zero-adjusted Bray-Curtis:
dummy_taxa_plot <- rep(1, nrow(tree_lichens_taxa_plot))
dummy_func_plot <- rep(1, nrow(tree_lichens_func_plot))
