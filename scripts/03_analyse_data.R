# sabah_lichens
# Analyse data

# ANOVAs of lichen diversity vs. site -------------------------------




# PERMANOVAs of lichen community structure vs. site -----------------
# (NB -- R doesn't allow for specification of more complex models or
# post-hoc pairwise comparisons... use PRIMER instead)

# ~ taxonomic groups:
perm_li_taxa <- adonis(  # vegan::adonis()
  # log10(x+1)-transformed data, including 'dummy' variable to enable
  # calculation of zero-adjusted Bray-Curtis dissimilarities:
  log10(dd_lichens_taxa[, c(lichen_taxa, 'dummy')] + 1) ~ site,
  data = dd_lichens_taxa,
  permutations = n_perm, method = "bray"
)

# ~ functional groups:
perm_li_func <- adonis(
  log10(dd_lichens_func[, c(lichen_func_grps, 'dummy')] + 1) ~ site,
  data = dd_lichens_func,
  permutations = n_perm, method = "bray"
)




# MDS ordinations of lichen community structure --------------------
# (NB -- no convergence after multiple iterations)

# ~ taxonomic groups:
mds_li_taxa <- mds(
  # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
  log10(dd_lichens_taxa[, c(lichen_taxa, 'dummy')] + 1)
)

# ~ functional groups:
mds_li_func <- mds(
  log10(dd_lichens_func[, c(lichen_func_grps, 'dummy')] + 1)
)




# CAP of lichen communities vs. tree functional traits --------------
