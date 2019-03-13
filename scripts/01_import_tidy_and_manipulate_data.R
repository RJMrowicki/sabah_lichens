# sabah_lichens
# Import, tidy and manipulate data

# Import data -------------------------------------------------------

# lichen abundance data:
# ~ taxonomic groups:
ddR_lichens_taxa <- read_csv('./data/lichens_taxa.csv')
# ~ functional groups:
ddR_lichens_func <- read_csv('./data/lichens_func.csv')

# tree functional trait data:
ddR_trees_func <- read_csv('./data/trees_func.csv')





# Tidy data ---------------------------------------------------------

# lichen abundance data:
# ~ taxonomic groups:
dd_lichens_taxa <-  # create new data frame
  ddR_lichens_taxa %>%
  # # remove `D810` (empty duplicate) and rename `D810_1` to `D810`:
  # # (NB -- only if this column actually represents a duplicate)
  # select(-`D810`) %>% rename(`D810` = `D810_1`) %>%
  # replace all NA values with 0:
  replace(is.na(.), 0) %>%
  # remove `Fam code` and rename `Genus code` to `taxon`:
  select(-`Fam code`) %>% rename(`taxon` = `Genus code`) %>%
  # transpose to make rows = samples and columns = taxa:
  gather(`tree`, `n`, `D11`:`SF12`) %>% spread(`taxon`, `n`)

# ~ functional groups:
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
  gather(`tree`, `n`, `D11`:`SF12`) %>% spread(`func_grp`, `n`)

# tree functional trait data:
dd_trees_func <-  # create new data frame
  ddR_trees_func %>%
  # rename all variables (except `girth`):
  rename(`tree` = 1, `bark` = 3, `buttress` = 4, `func_grp` = 5) %>%
  # change 'character' variables (excluding `tree`) to 'factor' type:
  mutate_at(3:5, factor)
  