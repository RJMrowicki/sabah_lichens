# sabah_lichens
# Analyse data

# Univariate analyses ===============================================

# ~ mixed effects models of lichen diversity vs. site ---------------

# ~~ taxonomic groups:

# ~~~ richness:
lm_li_taxa_s <-  # make model
  # (NB -- 'plot/site' specifies both 'plot' AND 'site:plot' as random effects;
  # as 'site' is already a fixed effect (and cannot be a random effect as well),
  # specify 'site:plot' only; and set REML to true if data unbalanced...?)
  lmer(`S` ~ site + (1 | site:plot), data = dd_tree_lichens_taxa)




# # or GLM?
# distr_plots(dd_tree_lichens_taxa$S)
# glm_li_taxa_s <-  # make model
#   glmer(`S`+1 ~ site + (1 | site:plot), data = dd_tree_lichens_taxa, family = Gamma)




# diagnostic plots of model residuals:
# par(mfrow = c(2, 3))
plot(lm_li_taxa_s); # hist(residuals(lm_li_taxa_s))
par(mfrow = c(1, 2))
qqnorm(residuals(lm_li_taxa_s)); hist(residuals(lm_li_taxa_s))

# Shapiro-Wilk normality test:
norm_li_taxa_s <- shapiro.test(residuals(lm_li_taxa_s))
# # Levene's test for homoscedasticity:
# het_li_taxa_s <- leveneTest(`S` ~ site, data = dd_tree_lichens_taxa)

# ANOVA table (based on Type II SS):
anova_li_taxa_s <- Anova(lm_li_taxa_s, type = 'II')




# ~~~ diversity:
lm_li_taxa_h <-  # make model
  lmer(`H'` ~ site + (1 | site:plot), data = dd_tree_lichens_taxa)

# distr_plots(dd_tree_lichens_taxa$`H'`)
# glm_li_taxa_h <-  # make model
#   glmer(`H'`+1 ~ site + (1 | site:plot), data = dd_tree_lichens_taxa, family = Gamma)

# # diagnostic plots of model residuals:
# par(mfrow = c(2, 3))
plot(lm_li_taxa_h); # hist(residuals(lm_li_taxa_h))
par(mfrow = c(1, 2))
qqnorm(residuals(lm_li_taxa_h)); hist(residuals(lm_li_taxa_h))

# Shapiro-Wilk normality test:
norm_li_taxa_h <- shapiro.test(residuals(lm_li_taxa_h))
# # Levene's test for homoscedasticity:
# het_li_taxa_h <- leveneTest(`H'` ~ site, data = dd_tree_lichens_taxa)

# ANOVA table (based on Type II SS):
anova_li_taxa_h <- Anova(lm_li_taxa_h, type = 'II')




# ~~ functional groups:

# ~~~ richness:
lm_li_func_s <-  # make model
  lmer(`S` ~ site + (1 | site:plot), data = dd_tree_lichens_func)

# distr_plots(dd_tree_lichens_func$S)
# glm_li_func_s <-  # make model
#   glmer(`S`+1 ~ site + (1 | site:plot), data = dd_tree_lichens_func, family = Gamma)

# diagnostic plots of model residuals:
# par(mfrow = c(2, 3))
plot(lm_li_func_s); # hist(residuals(lm_li_taxa_s))
par(mfrow = c(1, 2))
qqnorm(residuals(lm_li_func_s)); hist(residuals(lm_li_func_s))

# Shapiro-Wilk normality test:
norm_li_func_s <- shapiro.test(residuals(lm_li_func_s))
# # Levene's test for homoscedasticity:
# het_li_func_s <- leveneTest(`S` ~ site, data = dd_tree_lichens_func)

# ANOVA table (based on Type II SS):
anova_li_func_s <- Anova(lm_li_func_s, type = 'II')




# ~~~ diversity:
lm_li_func_h <-  # make model
  # (transformation doesn't correct normality)
  lmer(`H'` ~ site + (1 | site:plot), data = dd_tree_lichens_func)

# distr_plots(dd_tree_lichens_func$`H'`)
# glm_li_func_h <-  # make model
#   glmer(`H'`+1 ~ site + (1 | site:plot), data = dd_tree_lichens_func, family = Gamma)

# diagnostic plots of model residuals:
# par(mfrow = c(2, 3))
plot(lm_li_func_h); # hist(residuals(lm_li_func_h))
par(mfrow = c(1, 2))
qqnorm(residuals(lm_li_func_h)); hist(residuals(lm_li_func_h))

# Shapiro-Wilk normality test:
norm_li_func_h <- shapiro.test(residuals(lm_li_func_h))
# # Levene's test for homoscedasticity:
# het_li_func_h <- leveneTest(`H'` ~ site, data = dd_tree_lichens_func)

# ANOVA table (based on Type II SS):
anova_li_func_h <- Anova(lm_li_func_h, type = 'II')




# ~ PERANOVAs of lichen diversity vs. site --------------------------

# output data for PERANOVA analysis in PRIMER:
# ~ lichen taxonomic and functional group diversity:
# (NB -- use write.csv() instead of write_csv(), as require rownames)
write.csv(dd_tree_lichens_taxa[, c("S", "H'")], './primer/taxa_uni.csv')
write.csv(dd_tree_lichens_func[, c("S", "H'")], './primer/func_uni.csv')

# ~ corresponding factors (i.e. `site` and `plot`):
# (NB -- paste manually in PRIMER)
write_csv(dd_tree_lichens_taxa[, c('site', 'plot')], './primer/factors_taxa.csv')
write_csv(dd_tree_lichens_func[, c('site', 'plot')], './primer/factors_func.csv')

# ### PERMANOVA analysis in PRIMER here ###

# import results of PERANOVA analysis in PRIMER:
# ~ PERANOVA table and post-hoc pairwise test results:
li_taxa_s <- read_csv('./primer/results/li_taxa_s.csv')
li_taxa_s_ph <- read_csv('./primer/results/li_taxa_s_ph.csv')
li_taxa_h <- read_csv('./primer/results/li_taxa_h.csv')
li_taxa_h_ph <- read_csv('./primer/results/li_taxa_h_ph.csv')
li_func_s <- read_csv('./primer/results/li_func_s.csv')
li_func_s_ph <- read_csv('./primer/results/li_func_s_ph.csv')
li_func_h <- read_csv('./primer/results/li_func_h.csv')
li_func_h_ph <- read_csv('./primer/results/li_func_h_ph.csv')




# Unconstrained multivariate analyses ===============================

# ~ PERMANOVAs of lichen community structure vs. site ---------------
# (NB -- R doesn't allow for specification of more complex models or
# post-hoc pairwise comparisons... use PRIMER instead)

# output data for PERMANOVA analysis in PRIMER:
# ~ lichen taxonomic and functional group abundances:
# (NB -- use write.csv() instead of write_csv(), as require rownames)
write.csv(dd_tree_lichens_taxa[, lichen_taxa], './primer/taxa_multi.csv')
write.csv(dd_tree_lichens_func[, lichen_func_grps], './primer/func_multi.csv')
# ~ corresponding factors (i.e. `site` and `plot`) output above.

# ### PERMANOVA analysis in PRIMER here ###

# import results of PERMANOVA analysis in PRIMER:
# ~ PERMANOVA table and post-hoc pairwise test results:
perm_li_taxa <- read_csv('./primer/results/perm_li_taxa.csv')
perm_li_taxa_ph <- read_csv('./primer/results/perm_li_taxa_ph.csv')
perm_li_func <- read_csv('./primer/results/perm_li_func.csv')
perm_li_func_ph <- read_csv('./primer/results/perm_li_func_ph.csv')




# # ~~ taxonomic groups:
# perm_li_taxa <-
#   adonis(  # run PERMANOVA (vegan::adonis)
#     # log10(x+1)-transformed data, plus 'dummy' variable to enable
#     # calculation of zero-adjusted Bray-Curtis dissimilarities:
#     cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa) ~ site,
#     data = dd_tree_lichens_taxa,
#     permutations = n_perm, method = "bray"
#   )
# 
# # ~~ functional groups:
# perm_li_func <-
#   adonis(
#     cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_func) ~ site,
#     data = dd_tree_lichens_func,
#     permutations = n_perm, method = "bray"
#   )




# ~ MDS ordinations of lichen community structure -------------------
# (NB -- no convergence after multiple iterations)

# # ~~ taxonomic groups:
# mds_li_taxa <-
#   mds(  # custom mds function
#     # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
#     cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa)
#   )
# 
# # ~~ functional groups:
# mds_li_func <-
#   mds(cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_func))




# Constrained multivariate analyses =================================

# ~ Environmental variables -----------------------------------------

# create vector of continuous/ordinal tree trait variables:
env_vars <- c('girth', 'bark_ord', 'buttress')




env_cor <-  # test correlations between environmental variables
  rcorr(as.matrix(dd_trees_func[, env_vars]), type = "spearman")
# exclude if |\rho| > 0.7 ... currently, all values < 0.7
# create vector of variables to use in subsequent analyses:
env_use <- env_vars  # (currently no variables are excluded)




# ~ BIOENV for subsetting environmental variables -------------------

# ~~ taxonomic groups:
bioenv_taxa <-
  bioenv(  # run BIOENV analysis (vegan::bioenv)
    # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
    cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa),
    # vs. z-standardised environmental data:
    z_std(dd_tree_lichens_taxa[, env_vars]),
    method = "spearman", index = "bray", metric = "euclidean"
  )

bioenv_taxa_rho <-  # extract correlation coefficient
  max(summary(bioenv_taxa)$correlation)
bioenv_vars_taxa <-  # extract variables to use in CAP
  summary(bioenv_taxa)$variables[bioenv_taxa$whichbest]
bioenv_vars_taxa <- strsplit(bioenv_vars_taxa, " ")[[1]]
cond_vars_taxa <-  # specify conditioning variables
  env_vars[-which(env_vars %in% bioenv_vars_taxa)]

# ~~ functional groups:
bioenv_func <-
  bioenv(
    cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_func),
    z_std(dd_tree_lichens_func[, env_vars]),
    method = "spearman", index = "bray", metric = "euclidean"
  )

bioenv_func_rho <-  # extract correlation coefficient
  max(summary(bioenv_func)$correlation)
bioenv_vars_func <-  # extract variables to use in CAP
  summary(bioenv_func)$variables[bioenv_func$whichbest]
bioenv_vars_func <- strsplit(bioenv_vars_func, " ")[[1]]
cond_vars_func <-  # specify conditioning variables
  env_vars[-which(env_vars %in% bioenv_vars_func)]




# ~ CAP of lichen communities vs. tree functional traits ------------

# ~~ taxonomic groups:

cap_taxa <-
  capscale(  # run CAP analysis (vegan::capscale)
    formula(paste0(
      # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
      "cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa) ~ ",
      paste0(strsplit(bioenv_vars_taxa, split = " "), collapse = " + "))),
    # vs. z-standardised environmental data:
    data = z_std(dd_tree_lichens_taxa[, bioenv_vars_taxa]),
    distance = "bray")

# extract proportions explained, for total and constrained:
# ~ axis 1:
prop_cap1_taxa <- summary(cap_taxa)$cont[[1]][2, "CAP1"]*100
prop_cap1_con_taxa <- summary(cap_taxa)$concont[[1]][2, "CAP1"]*100
# ~ axis 2:
# (NB -- if only one environmental variable used, CAP2 is irrelevant)
if (length(bioenv_vars_taxa) > 1) {
  prop_cap2_taxa <- summary(cap_taxa)$cont[[1]][2, "CAP2"]*100
  prop_cap2_con_taxa <- summary(cap_taxa)$concont[[1]][2, "CAP2"]*100
} else {
  prop_cap2_taxa <- prop_cap2_con_taxa <- NULL
}

# calculate squared canonical correlation(s):
delta_sq_taxa <-  # create empty vector
  vector(length = length(bioenv_vars_taxa))

for(i in 1:length(bioenv_vars_taxa)) {
  delta_sq_taxa[i] <-  # assign to relevant 
    cor(  # calculate Pearson correlation coefficient
      summary(cap_taxa)$sites[, i],  # site scores
      summary(cap_taxa)$constraints[, i],  # site constraints
      method = "pearson"
    )^2
}




# test significance of overall analysis and of individual terms:
anova_cap_taxa <-  # overall analysis
  anova(cap_taxa, permutations = n_perm, model = "reduced")

# extract F and P values:
cap_taxa_f <- anova_cap_taxa$F[1]
cap_taxa_p <- anova_cap_taxa$`Pr(>F)`[1]

anova_cap_taxa_term <-  # terms assessed sequentially
  anova(cap_taxa, by = "term", permutations = n_perm, model = "reduced")
anova_cap_taxa_margin <-  # assess marginal effects of terms
  anova(cap_taxa, by = "margin", permutations = n_perm, model = "reduced")

# extract F and P values:
cap_taxa_margin_f <- anova_cap_taxa_margin$F[1:length(bioenv_vars_taxa)]
cap_taxa_margin_p <- anova_cap_taxa_margin$`Pr(>F)`[1:length(bioenv_vars_taxa)]
# rename vector elements according to environmental variables used:
names(cap_taxa_margin_p) <- names(cap_taxa_margin_f) <- bioenv_vars_taxa




# test environmental and species correlations with axes:

cor_cap_taxa_env <-  # ~ environmental
  as.data.frame(cor(cbind(
    scores(cap_taxa)$sites,  # site scores
    # z-standardised, subsetted environmental data:
    z_std(dd_tree_lichens_taxa[, bioenv_vars_taxa])),
    method = "spearman")[  # rank (not product-moment) correlation
      -(1:2), 1:2])  # subset relevant correlations

cor_cap_taxa_spp <-  # ~ species
  as.data.frame(cor(cbind(
    scores(cap_taxa)$sites, # site scores
    # log10(x+1)-transformed abundance data:
    log10(dd_tree_lichens_taxa[, lichen_taxa] + 1)),
    method = "spearman")[  # rank (not product-moment) correlation
      -(1:2), 1:2])  # subset relevant correlations

# extract relevant species:
cor_taxa_spp <-
  cor_cap_taxa_spp[which(  # correlation coefficient >= x
    if (length(bioenv_vars_taxa) > 1) {
      abs(cor_cap_taxa_spp$CAP1) >= 0.5 | abs(cor_cap_taxa_spp$CAP2) >= 0.5
    } else {
      abs(cor_cap_taxa_spp$CAP1) >= 0.5
    }
  ), ]

# ordered vector of names of species with strongest correlations:
top_taxa <- rownames(cor_cap_taxa_spp[order(
  apply(cor_cap_taxa_spp, MARGIN = 1, function(x) {max(abs(x))}),
  decreasing = TRUE), ])




# ~~ functional groups:

cap_func <-
  capscale(  # run CAP analysis (vegan::capscale)
    formula(paste0(
      # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
      "cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_func) ~ ",
      paste0(strsplit(bioenv_vars_func, split = " "), collapse = " + "))),
    # vs. z-standardised environmental data:
    data = z_std(dd_tree_lichens_func[, bioenv_vars_func]),
    distance = "bray")

# extract proportions explained, for total and constrained:
# ~ axis 1:
prop_cap1_func <- summary(cap_func)$cont[[1]][2, "CAP1"]*100
prop_cap1_con_func <- summary(cap_func)$concont[[1]][2, "CAP1"]*100
# ~ axis 2:
# (NB -- if only one environmental variable used, CAP2 is irrelevant)
if (length(bioenv_vars_func) > 1) {
  prop_cap2_func <- summary(cap_func)$cont[[1]][2, "CAP2"]*100
  prop_cap2_con_func <- summary(cap_func)$concont[[1]][2, "CAP2"]*100
} else {
  prop_cap2_func <- prop_cap2_con_func <- NULL
}

# calculate squared canonical correlation(s):
delta_sq_func <-  # create empty vector
  vector(length = length(bioenv_vars_func))

for(i in 1:length(bioenv_vars_func)) {
  delta_sq_func[i] <-  # assign to relevant 
    cor(  # calculate Pearson correlation coefficient
      summary(cap_func)$sites[, i],  # site scores
      summary(cap_func)$constraints[, i],  # site constraints
      method = "pearson"
    )^2
}




# test significance of overall analysis and of individual terms:
anova_cap_func <-  # overall analysis
  anova(cap_func, permutations = n_perm, model = "reduced")

# extract F and P values:
cap_func_f <- anova_cap_func$F[1]
cap_func_p <- anova_cap_func$`Pr(>F)`[1]

anova_cap_func_term <-  # terms assessed sequentially
  anova(cap_func, by = "term", permutations = n_perm, model = "reduced")
anova_cap_func_margin <-  # assess marginal effects of terms
  anova(cap_func, by = "margin", permutations = n_perm, model = "reduced")

# extract F and P values:
cap_func_margin_f <- anova_cap_func_margin$F[1:length(bioenv_vars_func)]
cap_func_margin_p <- anova_cap_func_margin$`Pr(>F)`[1:length(bioenv_vars_func)]
# rename vector elements according to environmental variables used:
names(cap_func_margin_p) <- names(cap_func_margin_f) <- bioenv_vars_func




# test environmental and species correlations with axes:

cor_cap_func_env <-  # ~ environmental
  as.data.frame(cor(cbind(
    scores(cap_func)$sites,  # site scores
    # z-standardised, subsetted environmental data:
    z_std(dd_tree_lichens_func[, bioenv_vars_func])),
    method = "spearman")[  # rank (not product-moment) correlation
      -(1:2), 1:2])  # subset relevant correlations

cor_cap_func_spp <-  # ~ species
  as.data.frame(cor(cbind(
    scores(cap_func)$sites, # site scores
    # log10(x+1)-transformed abundance data:
    log10(dd_tree_lichens_func[, lichen_func_grps] + 1)),
    method = "spearman")[  # rank (not product-moment) correlation
      -(1:2), 1:2])  # subset relevant correlations

# extract relevant species:
cor_func_spp <-
  cor_cap_func_spp[which(  # correlation coefficient >= x
    if (length(bioenv_vars_func) > 1) {
      abs(cor_cap_func_spp$CAP1) >= 0.5 | abs(cor_cap_func_spp$CAP2) >= 0.5
    } else {
      abs(cor_cap_func_spp$CAP1) >= 0.5
    }
  ), ]

# ordered vector of names of species with strongest correlations:
top_func <- rownames(cor_cap_func_spp[order(
  apply(cor_cap_func_spp, MARGIN = 1, function(x) {max(abs(x))}),
  decreasing = TRUE), ])
