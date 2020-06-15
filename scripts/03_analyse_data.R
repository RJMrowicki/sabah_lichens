# sabah_lichens
# Analyse data

# 1. Univariate analyses =======================================================

# ~ 1.1 mixed effects models of lichen diversity vs. site ----------------------

# NB -- probability distributions are not really suitable for GLMMs;
# try zero-inflated models, OR permutational ANOVA (see below).

# ~~ taxonomic groups:

# ~~~ richness:
lm_li_taxa_s <-  # make model
  # (NB -- 'plot/site' specifies both 'plot' AND 'site:plot' as
  # random effects; as 'site' is already a fixed effect (and cannot
  # be a random effect as well), specify 'site:plot' only;
  # and set REML to TRUE, as data unbalanced...?)
  lmer(`S` ~ site + (1 | site:plot), data = dd_tree_lichens_taxa)




# # or GLM?
# distr_plots(dd_tree_lichens_taxa$S)
# glm_li_taxa_s <-  # make model
#   glmer(`S`+1 ~ site + (1 | site:plot), data = dd_tree_lichens_taxa, family = Gamma)




# # diagnostic plots of model residuals:
# plot(lm_li_taxa_s)
# par(mfrow = c(1, 2))
# qqnorm(residuals(lm_li_taxa_s)); hist(residuals(lm_li_taxa_s))

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
# plot(lm_li_taxa_h)
# par(mfrow = c(1, 2))
# qqnorm(residuals(lm_li_taxa_h)); hist(residuals(lm_li_taxa_h))

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

# # diagnostic plots of model residuals:
# plot(lm_li_func_s)
# par(mfrow = c(1, 2))
# qqnorm(residuals(lm_li_func_s)); hist(residuals(lm_li_func_s))

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

# # diagnostic plots of model residuals:
# plot(lm_li_func_h)
# par(mfrow = c(1, 2))
# qqnorm(residuals(lm_li_func_h)); hist(residuals(lm_li_func_h))

# Shapiro-Wilk normality test:
norm_li_func_h <- shapiro.test(residuals(lm_li_func_h))
# # Levene's test for homoscedasticity:
# het_li_func_h <- leveneTest(`H'` ~ site, data = dd_tree_lichens_func)

# ANOVA table (based on Type II SS):
anova_li_func_h <- Anova(lm_li_func_h, type = 'II')




# ~ 1.2 PERANOVAs of lichen diversity vs. site ---------------------------------

# output data for PERANOVA analysis in PRIMER:
# ~ lichen taxonomic and functional group diversity:
# (NB -- use write.csv() instead of write_csv(), as require rownames)
write.csv(dd_tree_lichens_taxa[, "S"], './primer/li_taxa_s.csv')
write.csv(dd_tree_lichens_taxa[, "H'"], './primer/li_taxa_h.csv')
write.csv(dd_tree_lichens_func[, "S"], './primer/li_func_s.csv')
write.csv(dd_tree_lichens_func[, "H'"], './primer/li_func_h.csv')

# ~ corresponding factors (i.e. `site` and `plot`):
# (NB -- paste manually in PRIMER)
write_csv(dd_tree_lichens_taxa[, c('site', 'plot')], './primer/factors_taxa.csv')
write_csv(dd_tree_lichens_func[, c('site', 'plot')], './primer/factors_func.csv')


# ### PERANOVA analysis in PRIMER here ###


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




# 2. Unconstrained multivariate analyses =======================================

# ~ 2.1 PERMANOVAs of lichen community structure vs. site ----------------------
# (NB -- R doesn't allow for specification of more complex models or
# post-hoc pairwise comparisons... use PRIMER instead)

# output data for PERMANOVA analysis in PRIMER:
# ~ lichen taxonomic and functional group abundances:
# (NB -- use write.csv() instead of write_csv(), as require rownames)
write.csv(dd_tree_lichens_taxa[, lichen_taxa], './primer/li_taxa.csv')
write.csv(dd_tree_lichens_func[, lichen_func_grps], './primer/li_func.csv')
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




# ~ 2.2 MDS ordinations of lichen community structure --------------------------

# ~~ Tree-level ----------------------------------------------------------------
# (NB -- no convergence after multiple iterations;
# unless no. of dimensions [k] is set to 3...)

# ~~ taxonomic groups:
mds_li_taxa <- mds(  # custom mds function
  # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
  cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa)
)

# ~~ functional groups:
mds_li_func <- mds(
  # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
  cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_func)
)




# ~~ Plot-level ----------------------------------------------------------------

# ~~ taxonomic groups:
mds_li_taxa_plot <- mds(  # custom mds function
  # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
  cbind(log10(tree_lichens_taxa_plot[, lichen_taxa] + 1), dummy_taxa_plot)
)

# ~~ functional groups:
mds_li_func_plot <- mds(
  # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
  cbind(log10(tree_lichens_func_plot[, lichen_func_grps] + 1), dummy_func_plot)
)




# ~ 2.3 SIMPER tests of contributions to differences ---------------------------

# ~~ taxonomic groups:

simp_li_taxa_untransf <- simper(
  # include dummy taxa to avoid empty row errors:
  cbind(dd_tree_lichens_taxa[, lichen_taxa], dummy_taxa),
  if_else(dd_tree_lichens_taxa$site == 'S', 'S', '(D,M)')  # (D,M) vs. S
)
# summary(simp_li_taxa_untransf)

simp_li_taxa_transf <- simper(
  # log10(x+1)-transform taxonomic group data, not dummy data:
  cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa),
  if_else(dd_tree_lichens_taxa$site == 'S', 'S', '(D,M)')  # (D,M) vs. S
)
# summary(simp_li_taxa_transf)

# produce summary table:
simp_li_taxa <- simp_tab(simp_li_taxa_transf, simp_li_taxa_untransf)




# ~~ functional groups:

simp_li_func_untransf <- simper(
  cbind(dd_tree_lichens_func[, lichen_func_grps], dummy_taxa),
  if_else(dd_tree_lichens_func$site == 'S', 'S', '(D,M)')  # D,M vs. S
)
# summary(simp_li_func_untransf)

simp_li_func_transf <- simper(
  cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_taxa),
  if_else(dd_tree_lichens_func$site == 'S', 'S', '(D,M)')  # D,M vs. S
)
# summary(simp_li_func_transf)

# produce summary table:
simp_li_func <- simp_tab(simp_li_func_transf, simp_li_func_untransf)




# 3. Constrained multivariate analyses =========================================

# ~ 3.1 Tree-level -------------------------------------------------------------

# ~~ Environmental variables ---------------------------------------------------

# create vectors of continuous/ordinal and categorical tree trait variables:
env_vars_ord <- c('girth_m', 'bark_ord', 'buttress_ord')
env_vars_cat <- c('girth', 'bark', 'buttress', 'dipterocarp')



# test correlations between environmental variables (ordinal/categorical only)
env_cor <-
  rcorr(as.matrix(dd_trees_func[, env_vars_ord]), type = "spearman")
# exclude if |\rho| > 0.7 ... currently, all values < 0.7
# create vector of variables to use in subsequent analyses:
env_use <- env_vars_ord  # (currently no variables are excluded)




# ~~ BIOENV for subsetting environmental variables -----------------------------
# (NB -- ordinal/categorical only)

# ~~~ taxonomic groups:
bioenv_taxa <-
  bioenv(  # run BIOENV analysis (vegan::bioenv)
    # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
    cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa),
    # vs. z-standardised environmental data:
    z_std(dd_tree_lichens_taxa[, env_use]),
    method = "spearman", index = "bray", metric = "euclidean"
  )

bioenv_taxa_rho <-  # extract correlation coefficient
  max(summary(bioenv_taxa)$correlation)

bioenv_vars_taxa <-  # extract variables to use in CAP
  summary(bioenv_taxa)$variables[bioenv_taxa$whichbest]
bioenv_vars_taxa <- strsplit(bioenv_vars_taxa, " ")[[1]]

cond_vars_taxa <-  # specify conditioning variables
  env_use[-which(env_use %in% bioenv_vars_taxa)]

# ~~~ functional groups:
bioenv_func <-
  bioenv(
    cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_func),
    z_std(dd_tree_lichens_func[, env_use]),
    method = "spearman", index = "bray", metric = "euclidean"
  )

bioenv_func_rho <-  # extract correlation coefficient
  max(summary(bioenv_func)$correlation)

bioenv_vars_func <-  # extract variables to use in CAP
  summary(bioenv_func)$variables[bioenv_func$whichbest]
bioenv_vars_func <- strsplit(bioenv_vars_func, " ")[[1]]

cond_vars_func <-  # specify conditioning variables
  env_use[-which(env_use %in% bioenv_vars_func)]




# ~~ CAP (dbRDA) of lichen communities vs. tree functional traits --------------

# (NB -- use vegan::capscale, as it allows multiple factors as constraints;
# 'traditional' CAP [PRIMER or FORTRAN program] only allows a single factor
# [i.e. grouping variable for Discriminant Analysis].


# ~~~ taxonomic groups:

# specify variables used in CAP:
env_vars <- env_vars_cat
# env_vars <- bioenv_vars_taxa  # BIOENV subset of continuous/ordinal variables


# # ~~~~ continuous/ordinal environmental variables:
# cap_taxa <-
#   capscale(  # run CAP analysis (vegan::capscale)
#     formula(paste0(
#       # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
#       "cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa) ~ ",
#       paste0(strsplit(env_vars, split = " "), collapse = " + "),
#       "+ Condition(plot)"  # (NB -- `plot` as a conditioning variable)
#     )),
#     # vs. z-standardised environmental data:
#     data = z_std(dd_tree_lichens_taxa[, c(env_vars, "plot")]),
#     distance = "bray")


# ~~~~ categorical environmental variables:
cap_taxa <-
  capscale(  # run CAP analysis (vegan::capscale)
    formula(paste0(
      # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
      "cbind(log10(dd_tree_lichens_taxa[, lichen_taxa] + 1), dummy_taxa) ~ ",
      paste0(strsplit(env_vars, split = " "), collapse = " + "),
      "+ Condition(plot)"  # (NB -- `plot` as a conditioning variable)
    )),
    data = dd_tree_lichens_taxa[, c(env_vars, "plot")],
    distance = "bray")


# extract proportions explained, for total and constrained:
# ~ axis 1:
prop_cap1_taxa <- summary(cap_taxa)$cont[[1]][2, "CAP1"]*100
prop_cap1_con_taxa <- summary(cap_taxa)$concont[[1]][2, "CAP1"]*100
# ~ axis 2:
# (NB -- if only one environmental variable used, CAP2 is irrelevant)
if (length(env_vars) > 1) {
  prop_cap2_taxa <- summary(cap_taxa)$cont[[1]][2, "CAP2"]*100
  prop_cap2_con_taxa <- summary(cap_taxa)$concont[[1]][2, "CAP2"]*100
} else {
  prop_cap2_taxa <- prop_cap2_con_taxa <- NULL
}

# calculate squared canonical correlation(s):
delta_sq_taxa <-  # create empty vector
  vector(length = length(env_vars))

for(i in 1:length(env_vars)) {
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
cap_taxa_margin_f <- anova_cap_taxa_margin$F[1:length(env_vars)]
cap_taxa_margin_p <- anova_cap_taxa_margin$`Pr(>F)`[1:length(env_vars)]
# rename vector elements according to environmental variables used:
names(cap_taxa_margin_p) <- names(cap_taxa_margin_f) <- env_vars




# test environmental and species correlations with axes:

# cor_cap_taxa_env <-  # ~ environmental
#   #  (NB -- for ordinal/continuous variables only)
#   as.data.frame(cor(cbind(
#     scores(cap_taxa)$sites,  # site scores
#     # z-standardised, subsetted environmental data:
#     z_std(dd_tree_lichens_taxa[, env_vars_ord])),
#     method = "spearman")[  # rank (not product-moment) correlation
#       -(1:2), 1:2])  # subset relevant correlations

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
    if (length(env_vars) > 1) {
      abs(cor_cap_taxa_spp$CAP1) >= cor_spp_x |
        abs(cor_cap_taxa_spp$CAP2) >= cor_spp_x
    } else {
      abs(cor_cap_taxa_spp$CAP1) >= cor_spp_x
    }
  ), ]

# ordered vector of names of species with strongest correlations:
top_taxa <- rownames(cor_cap_taxa_spp[order(
  apply(cor_cap_taxa_spp, MARGIN = 1, function(x) {max(abs(x))}),
  decreasing = TRUE), ])




# ~~~ functional groups:

# specify variables used in CAP:
env_vars <- env_vars_cat
# env_vars <- bioenv_vars_func  # BIOENV subset of continuous/ordinal variables


# # ~~~~ continuous/ordinal environmental variables:
# cap_func <-
#   capscale(  # run CAP analysis (vegan::capscale)
#     formula(paste0(
#       # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
#       "cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_func) ~ ",
#       paste0(strsplit(env_vars, split = " "), collapse = " + "),
#       "+ Condition(plot)"  # (NB -- `plot` as a conditioning variable)
#     )),
#     # vs. z-standardised environmental data:
#     data = z_std(dd_tree_lichens_func[, c(env_vars, "plot")]),
#     distance = "bray")


# ~~~~ categorical environmental variables:
cap_func <-
  capscale(  # run CAP analysis (vegan::capscale)
    formula(paste0(
      # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
      "cbind(log10(dd_tree_lichens_func[, lichen_func_grps] + 1), dummy_func) ~ ",
      paste0(strsplit(env_vars, split = " "), collapse = " + "),
      "+ Condition(plot)"  # (NB -- `plot` as a conditioning variable)
    )),
    data = dd_tree_lichens_func[, c(env_vars, "plot")],
    distance = "bray")


# extract proportions explained, for total and constrained:
# ~ axis 1:
prop_cap1_func <- summary(cap_func)$cont[[1]][2, "CAP1"]*100
prop_cap1_con_func <- summary(cap_func)$concont[[1]][2, "CAP1"]*100
# ~ axis 2:
# (NB -- if only one environmental variable used, CAP2 is irrelevant)
if (length(env_vars) > 1) {
  prop_cap2_func <- summary(cap_func)$cont[[1]][2, "CAP2"]*100
  prop_cap2_con_func <- summary(cap_func)$concont[[1]][2, "CAP2"]*100
} else {
  prop_cap2_func <- prop_cap2_con_func <- NULL
}

# calculate squared canonical correlation(s):
delta_sq_func <-  # create empty vector
  vector(length = length(env_vars))

for(i in 1:length(env_vars)) {
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
cap_func_margin_f <- anova_cap_func_margin$F[1:length(env_vars)]
cap_func_margin_p <- anova_cap_func_margin$`Pr(>F)`[1:length(env_vars)]
# rename vector elements according to environmental variables used:
names(cap_func_margin_p) <- names(cap_func_margin_f) <- env_vars




# test environmental and species correlations with axes:

# cor_cap_func_env <-  # ~ environmental
#   #  (NB -- for ordinal/continuous variables only)
#   as.data.frame(cor(cbind(
#     scores(cap_func)$sites,  # site scores
#     # z-standardised, subsetted environmental data:
#     z_std(dd_tree_lichens_func[, env_vars_ord])),
#     method = "spearman")[  # rank (not product-moment) correlation
#       -(1:2), 1:2])  # subset relevant correlations

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
    if (length(env_vars) > 1) {
      abs(cor_cap_func_spp$CAP1) >= cor_spp_x |
        abs(cor_cap_func_spp$CAP2) >= cor_spp_x
    } else {
      abs(cor_cap_func_spp$CAP1) >= cor_spp_x
    }
  ), ]

# ordered vector of names of species with strongest correlations:
top_func <- rownames(cor_cap_func_spp[order(
  apply(cor_cap_func_spp, MARGIN = 1, function(x) {max(abs(x))}),
  decreasing = TRUE), ])



# ~ 3.2 Plot-level -------------------------------------------------------------

# ~~ Environmental variables ---------------------------------------------------

# create vectors of continuous/ordinal and categorical tree trait variables:
env_vars_plot <- c("bark_div", "girth_l_prop", "buttress_prop", "dipterocarp_prop")




env_cor_plot <-  # test correlations between environmental variables
  rcorr(as.matrix(trees_func_plot[, env_vars_plot]), type = "spearman")
# exclude if |\rho| > 0.7 ... currently, all values < 0.7
# create vector of variables to use in subsequent analyses:
env_use_plot <- env_vars_plot  # (currently no variables are excluded)




# ~~ BIOENV for subsetting environmental variables -----------------------------

# ~~~ taxonomic groups:
bioenv_taxa_plot <-
  bioenv(  # run BIOENV analysis (vegan::bioenv)
    # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
    cbind(log10(tree_lichens_taxa_plot[, lichen_taxa] + 1), dummy_taxa_plot),
    # vs. z-standardised environmental data:
    z_std(tree_lichens_taxa_plot[, env_use_plot]),
    method = "spearman", index = "bray", metric = "euclidean"
  )

bioenv_taxa_plot_rho <-  # extract correlation coefficient
  max(summary(bioenv_taxa_plot)$correlation)

bioenv_vars_taxa_plot <-  # extract variables to use in CAP
  summary(bioenv_taxa_plot)$variables[bioenv_taxa_plot$whichbest]
bioenv_vars_taxa_plot <- strsplit(bioenv_vars_taxa_plot, " ")[[1]]

cond_vars_taxa_plot <-  # specify conditioning variables
  env_use_plot[-which(env_use_plot %in% bioenv_vars_taxa_plot)]


# ~~~ functional groups:
bioenv_func_plot <-
  bioenv(
    cbind(log10(tree_lichens_func_plot[, lichen_func_grps] + 1), dummy_func_plot),
    z_std(tree_lichens_func_plot[, env_use_plot]),
    method = "spearman", index = "bray", metric = "euclidean"
  )

bioenv_func_plot_rho <-  # extract correlation coefficient
  max(summary(bioenv_func_plot)$correlation)

bioenv_vars_func_plot <-  # extract variables to use in CAP
  summary(bioenv_func_plot)$variables[bioenv_func_plot$whichbest]
bioenv_vars_func_plot <- strsplit(bioenv_vars_func_plot, " ")[[1]]

cond_vars_func_plot <-  # specify conditioning variables
  env_use_plot[-which(env_use_plot %in% bioenv_vars_func_plot)]




# ~~ 3.2.1 CAP (dbRDA) of lichen communities vs. tree functional traits --------

# ~~~ taxonomic groups:

env_vars_plot <- bioenv_vars_taxa_plot  # specify variables used in CAP

cap_taxa_plot <-
  capscale(  # run CAP analysis (vegan::capscale)
    formula(paste0(
      # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
      "cbind(log10(tree_lichens_taxa_plot[, lichen_taxa] + 1), dummy_taxa_plot) ~ ",
      paste0(strsplit(env_vars_plot, split = " "), collapse = " + ")
    )),
    # vs. z-standardised environmental data:
    data = z_std(tree_lichens_taxa_plot[, env_vars_plot]),
    distance = "bray"  # specify Bray-Curtis dissimilarity
  )


# extract proportions explained, for total and constrained:
# ~ axis 1:
prop_cap1_taxa_plot <- summary(cap_taxa_plot)$cont[[1]][2, "CAP1"]*100
prop_cap1_con_taxa_plot <- summary(cap_taxa_plot)$concont[[1]][2, "CAP1"]*100
# ~ axis 2:
# (NB -- if only one environmental variable used, CAP2 is irrelevant)
if (length(env_vars) > 1) {
  prop_cap2_taxa_plot <- summary(cap_taxa_plot)$cont[[1]][2, "CAP2"]*100
  prop_cap2_con_taxa_plot <- summary(cap_taxa_plot)$concont[[1]][2, "CAP2"]*100
} else {
  prop_cap2_taxa_plot <- prop_cap2_con_taxa_plot <- NULL
}

# calculate squared canonical correlation(s):
delta_sq_taxa_plot <-  # create empty vector
  vector(length = length(env_vars_plot))

for(i in 1:length(env_vars_plot)) {
  delta_sq_taxa_plot[i] <-  # assign to relevant item
    cor(  # calculate Pearson correlation coefficient
      summary(cap_taxa_plot)$sites[, i],  # site scores
      summary(cap_taxa_plot)$constraints[, i],  # site constraints
      method = "pearson"
    )^2
}




# test significance of overall analysis and of individual terms:
anova_cap_taxa_plot <-  # overall analysis
  anova(cap_taxa_plot, permutations = n_perm, model = "reduced")

# extract F and P values:
cap_taxa_plot_f <- anova_cap_taxa_plot$F[1]
cap_taxa_plot_p <- anova_cap_taxa_plot$`Pr(>F)`[1]

anova_cap_taxa_plot_term <-  # terms assessed sequentially
  anova(cap_taxa_plot, by = "term", permutations = n_perm, model = "reduced")
anova_cap_taxa_plot_margin <-  # assess marginal effects of terms
  anova(cap_taxa_plot, by = "margin", permutations = n_perm, model = "reduced")

# extract F and P values:
cap_taxa_plot_margin_f <- anova_cap_taxa_plot_margin$F[1:length(env_vars_plot)]
cap_taxa_plot_margin_p <- anova_cap_taxa_plot_margin$`Pr(>F)`[1:length(env_vars_plot)]
# rename vector elements according to environmental variables used:
names(cap_taxa_plot_margin_p) <- names(cap_taxa_plot_margin_f) <- env_vars_plot




# test environmental and species correlations with axes:

cor_cap_taxa_plot_env <-  # ~ environmental
  as.data.frame(cor(cbind(
    scores(cap_taxa_plot)$sites,  # site scores
    # z-standardised, subsetted environmental data:
    z_std(tree_lichens_taxa_plot[, env_vars_plot])),
    method = "spearman")[  # rank (not product-moment) correlation
      -(1:2), 1:2])  # subset relevant correlations

cor_cap_taxa_plot_spp <-  # ~ species
  as.data.frame(cor(cbind(
    scores(cap_taxa_plot)$sites, # site scores
    # log10(x+1)-transformed abundance data:
    log10(tree_lichens_taxa_plot[, lichen_taxa] + 1)),
    method = "spearman")[  # rank (not product-moment) correlation
      -(1:2), 1:2])  # subset relevant correlations

# extract relevant species:
cor_taxa_plot_spp <-
  cor_cap_taxa_plot_spp[which(  # correlation coefficient >= x
    if (length(env_vars) > 1) {
      abs(cor_cap_taxa_plot_spp$CAP1) >= cor_spp_x |
        abs(cor_cap_taxa_plot_spp$CAP2) >= cor_spp_x
    } else {
      abs(cor_cap_taxa_plot_spp$CAP1) >= cor_spp_x
    }
  ), ]

# ordered vector of names of species with strongest correlations:
top_taxa_plot <- rownames(cor_cap_taxa_plot_spp[order(
  apply(cor_cap_taxa_plot_spp, MARGIN = 1, function(x) {max(abs(x))}),
  decreasing = TRUE), ])




# ~~~ functional groups:

env_vars_plot <- bioenv_vars_func_plot  # specify variables used in CAP

cap_func_plot <-
  capscale(  # run CAP analysis (vegan::capscale)
    formula(paste0(
      # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
      "cbind(log10(tree_lichens_func_plot[, lichen_func_grps] + 1), dummy_func_plot) ~ ",
      paste0(strsplit(env_vars_plot, split = " "), collapse = " + ")
    )),
    # vs. z-standardised environmental data:
    data = z_std(tree_lichens_func_plot[, env_vars_plot]),
    distance = "bray"  # specify Bray-Curtis dissimilarity
  )


# extract proportions explained, for total and constrained:
# ~ axis 1:
prop_cap1_func_plot <- summary(cap_func_plot)$cont[[1]][2, "CAP1"]*100
prop_cap1_con_func_plot <- summary(cap_func_plot)$concont[[1]][2, "CAP1"]*100
# ~ axis 2:
# (NB -- if only one environmental variable used, CAP2 is irrelevant)
if (length(env_vars) > 1) {
  prop_cap2_func_plot <- summary(cap_func_plot)$cont[[1]][2, "CAP2"]*100
  prop_cap2_con_func_plot <- summary(cap_func_plot)$concont[[1]][2, "CAP2"]*100
} else {
  prop_cap2_func_plot <- prop_cap2_con_func_plot <- NULL
}

# calculate squared canonical correlation(s):
delta_sq_func_plot <-  # create empty vector
  vector(length = length(env_vars_plot))

for(i in 1:length(env_vars_plot)) {
  delta_sq_func_plot[i] <-  # assign to relevant item
    cor(  # calculate Pearson correlation coefficient
      summary(cap_func_plot)$sites[, i],  # site scores
      summary(cap_func_plot)$constraints[, i],  # site constraints
      method = "pearson"
    )^2
}




# test significance of overall analysis and of individual terms:
anova_cap_func_plot <-  # overall analysis
  anova(cap_func_plot, permutations = n_perm, model = "reduced")

# extract F and P values:
cap_func_plot_f <- anova_cap_func_plot$F[1]
cap_func_plot_p <- anova_cap_func_plot$`Pr(>F)`[1]

anova_cap_func_plot_term <-  # terms assessed sequentially
  anova(cap_func_plot, by = "term", permutations = n_perm, model = "reduced")
anova_cap_func_plot_margin <-  # assess marginal effects of terms
  anova(cap_func_plot, by = "margin", permutations = n_perm, model = "reduced")

# extract F and P values:
cap_func_plot_margin_f <- anova_cap_func_plot_margin$F[1:length(env_vars_plot)]
cap_func_plot_margin_p <- anova_cap_func_plot_margin$`Pr(>F)`[1:length(env_vars_plot)]
# rename vector elements according to environmental variables used:
names(cap_func_plot_margin_p) <- names(cap_func_plot_margin_f) <- env_vars_plot




# test environmental and species correlations with axes:

cor_cap_func_plot_env <-  # ~ environmental
  as.data.frame(cor(cbind(
    scores(cap_func_plot)$sites,  # site scores
    # z-standardised, subsetted environmental data:
    z_std(tree_lichens_func_plot[, env_vars_plot])),
    method = "spearman")[  # rank (not product-moment) correlation
      -(1:2), 1:2])  # subset relevant correlations

cor_cap_func_plot_spp <-  # ~ species
  as.data.frame(cor(cbind(
    scores(cap_func_plot)$sites, # site scores
    # log10(x+1)-transformed abundance data:
    log10(tree_lichens_func_plot[, lichen_func_grps] + 1)),
    method = "spearman")[  # rank (not product-moment) correlation
      -(1:2), 1:2])  # subset relevant correlations

# extract relevant species:
cor_func_plot_spp <-
  cor_cap_func_plot_spp[which(  # correlation coefficient >= x
    if (length(env_vars) > 1) {
      abs(cor_cap_func_plot_spp$CAP1) >= cor_spp_x |
        abs(cor_cap_func_plot_spp$CAP2) >= cor_spp_x
    } else {
      abs(cor_cap_func_plot_spp$CAP1) >= cor_spp_x
    }
  ), ]

# ordered vector of names of species with strongest correlations:
top_func_plot <- rownames(cor_cap_func_plot_spp[order(
  apply(cor_cap_func_plot_spp, MARGIN = 1, function(x) {max(abs(x))}),
  decreasing = TRUE), ])




# ~~ 3.2.2 CAP of lichen communities vs. site ----------------------------------

# output data for CAP analysis in FORTRAN program:
# (NB -- for testing classification success/goodness of fit, which is
# not possible using vegan::capscale)

# ~ lichen taxonomic and functional group diversity:
# (NB -- without rownames [hence write_csv()] or column names)
write_csv(  # (include dummy species)
  cbind(tree_lichens_taxa_plot[, lichen_taxa], dummy_taxa_plot),
  './cap/li_taxa_plot.txt', col_names = FALSE)
write_csv(
  cbind(tree_lichens_func_plot[, lichen_func_grps], dummy_func_plot),
  './cap/li_func_plot.txt', col_names = FALSE)

# (NB -- no need to output factor(s), as specify no. of groups/observations per group:)
# tree_lichens_taxa_plot %>% group_by(site) %>% summarise(n = n())


# ### CAP analysis via FORTRAN program here ###


# (NB -- log10(x+1) transform data, zero-adjusted Bray-Curtis [i.e. include dummy];
# Discriminant Analysis [groups] rather than Canonical Correlation Analysis [variables];
# allow program to choose m automatically;
# run tests with 9,999 permutations, specify random seed as 123)




# import results

# ~~ taxonomic groups:

cap_taxa_site_plot_raw <- read_lines('./cap/li_taxa_plot_results.txt')


# extract canonical axes (i.e. point coordinates):

# determine line number for start of axes table:
axes_start <- grep(
  "Canonical Axes (constrained)",
  cap_taxa_site_plot_raw, fixed = TRUE) + 3
# determine line number for end of table (i.e. start + no. of sites):
axes_end <- axes_start + nrow(tree_lichens_taxa_plot)-1

axes_cap_taxa_site_plot <-
  # extract raw lines containing axes table:
  cap_taxa_site_plot_raw[axes_start:axes_end] %>%
  # split, remove empty elements, remove first element per line:
  strsplit(" ") %>% map(~.[. != ""]) %>% map(~.[-1]) %>%
  # convert into matrix:
  unlist %>% matrix(
    nrow = nrow(tree_lichens_taxa_plot), ncol = 2, byrow = TRUE,
    dimnames = list(NULL, c("CAP1", "CAP2"))
  ) %>% as.data.frame %>%  # convert into data frame
  mutate_all(~ as.numeric(as.vector(.)))  # convert character to numeric


# extract squared canonical correlation(s):
delta_sq_taxa_site_plot <-
  # extract raw line containing delta^2 values:
  cap_taxa_site_plot_raw[grep(
    "Squared Correlations", cap_taxa_site_plot_raw, fixed = TRUE
  ) + 1] %>%
  # extract actual values (using presence of decimal point):
  strsplit(" ") %>% unlist %>% grep(".", ., value = TRUE) %>% as.numeric


# extract species correlations with axes:

# determine line number for start of correlation table:
cor_spp_start <- grep(
  "Correlations of Canonical Axes (Q*) with Original Variables (Y)",
  cap_taxa_site_plot_raw, fixed = TRUE) + 3
# determine line number for end of table (i.e. start + no. of species):
cor_spp_end <- cor_spp_start + length(lichen_taxa)-1

cor_cap_taxa_site_plot_spp <-
  # extract raw lines containing species correlation table:
  cap_taxa_site_plot_raw[cor_spp_start:cor_spp_end] %>%
  # split, remove empty elements, remove first element per line:
  strsplit(" ") %>% map(~.[. != ""]) %>% map(~.[-1]) %>%
  # convert into matrix:
  unlist %>% matrix(
    nrow = length(lichen_taxa), ncol = 2, byrow = TRUE,
    dimnames = list(lichen_taxa, c("CAP1", "CAP2"))
  ) %>% as.data.frame %>%  # convert into data frame
  mutate_all(~ as.numeric(as.vector(.)))  # convert character to numeric
  

# extract group classification success:
succ_cap_taxa_site_plot <-
  # extract raw line containing classification success:
  cap_taxa_site_plot_raw %>%
  grep("Total correct", ., fixed = TRUE, value = TRUE) %>%
  # remove %, split and extract last element:
  gsub("%", "", .) %>% strsplit(" ") %>% map(~.[length(.)]) %>%
  # convert to numeric vector:
  unlist %>% as.numeric




# ~~ functional groups:

cap_func_site_plot_raw <- read_lines('./cap/li_func_plot_results.txt')


# extract canonical axes (i.e. point coordinates):

# determine line number for start of axes table:
axes_start <- grep(
  "Canonical Axes (constrained)",
  cap_func_site_plot_raw, fixed = TRUE) + 3
# determine line number for end of table (i.e. start + no. of sites):
axes_end <- axes_start + nrow(tree_lichens_func_plot)-1

axes_cap_func_site_plot <-
  # extract raw lines containing axes table:
  cap_func_site_plot_raw[axes_start:axes_end] %>%
  # split, remove empty elements, remove first element per line:
  strsplit(" ") %>% map(~.[. != ""]) %>% map(~.[-1]) %>%
  # convert into matrix:
  unlist %>% matrix(
    nrow = nrow(tree_lichens_func_plot), ncol = 2, byrow = TRUE,
    dimnames = list(NULL, c("CAP1", "CAP2"))
  ) %>% as.data.frame %>%  # convert into data frame
  mutate_all(~ as.numeric(as.vector(.)))  # convert character to numeric


# extract squared canonical correlation(s):
delta_sq_func_site_plot <-
  # extract raw line containing delta^2 values:
  cap_func_site_plot_raw[grep(
    "Squared Correlations", cap_func_site_plot_raw, fixed = TRUE
  ) + 1] %>%
  # extract actual values (using presence of decimal point):
  strsplit(" ") %>% unlist %>% grep(".", ., value = TRUE) %>% as.numeric


# extract species correlations with axes:

# determine line number for start of correlation table:
cor_spp_start <- grep(
  "Correlations of Canonical Axes (Q*) with Original Variables (Y)",
  cap_func_site_plot_raw, fixed = TRUE) + 3
# determine line number for end of table (i.e. start + no. of species):
cor_spp_end <- cor_spp_start + length(lichen_func_grps)-1

cor_cap_func_site_plot_spp <-
  # extract raw lines containing species correlation table:
  cap_func_site_plot_raw[cor_spp_start:cor_spp_end] %>%
  # split, remove empty elements, remove first element per line:
  strsplit(" ") %>% map(~.[. != ""]) %>% map(~.[-1]) %>%
  # convert into matrix:
  unlist %>% matrix(
    nrow = length(lichen_func_grps), ncol = 2, byrow = TRUE,
    dimnames = list(lichen_func_grps, c("CAP1", "CAP2"))
  ) %>% as.data.frame %>%  # convert into data frame
  mutate_all(~ as.numeric(as.vector(.)))  # convert character to numeric


# extract group classification success:
succ_cap_func_site_plot <-
  # extract raw line containing classification success:
  cap_func_site_plot_raw %>%
  grep("Total correct", ., fixed = TRUE, value = TRUE) %>%
  # remove %, split and extract last element:
  gsub("%", "", .) %>% strsplit(" ") %>% map(~.[length(.)]) %>%
  # convert to numeric vector:
  unlist %>% as.numeric




################################################################################

# (testing use of BiodiversityR::CAPdiscrim. But produces warnings.)

# ~~~ taxonomic groups:

dist_taxa <-  # pre-calculate dissimilarity matrix for CAPdiscrim()
  vegdist(
    # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
    log10(cbind(tree_lichens_taxa_plot[, lichen_taxa], dummy_taxa_plot) + 1),
    method = "bray"
  )

cap_taxa_plot_site <- 
  CAPdiscrim(  # run CAP analysis (BiodiversityR::CAPdiscrim)
    dist_taxa ~ site,  # vs. site (categorical) only
    # NB -- environmental data as data frame, not tibble:
    data = as.data.frame(tree_lichens_taxa_plot),
    # specify no. of axes resulting in highest classification success:
    m = 4, add = TRUE, permutations = n_perm  
  )




m_max <- 15  # specify max no. of axes

plot(  # create blank plot
  1:m_max, rep(-1000, m_max), type = "n",
  xlim = c(0, m_max), ylim = c(0, 100),
  xlab = "m", ylab = "Classification success (%)"
)

# plot classification success for sequential values of m:
for (i in 1:m_max) {
  cap_result <- CAPdiscrim(
    dist_taxa ~ site,
    data = as.data.frame(tree_lichens_taxa_plot),
    axes = 2, m = i)
  points(i, cap_result$percent)
}




# ~~~ functional groups:

dist_func <-  # pre-calculate dissimilarity matrix for CAPdiscrim()
  vegdist(
    # log10(x+1)-transformed data and zero-adjusted Bray-Curtis:
    log10(cbind(tree_lichens_func_plot[, lichen_func_grps], dummy_func_plot) + 1),
    method = "bray"
  )

cap_func_plot_site <- 
  CAPdiscrim(  # run CAP analysis (BiodiversityR::CAPdiscrim)
    dist_func ~ site,  # vs. site (categorical) only
    # NB -- environmental data as data frame, not tibble:
    data = as.data.frame(tree_lichens_func_plot),
    # specify no. of axes resulting in highest classification success:
    m = 4, add = TRUE, permutations = n_perm  
  )




m_max <- 15  # specify max no. of axes

plot(  # create blank plot
  1:m_max, rep(-1000, m_max), type = "n",
  xlim = c(0, m_max), ylim = c(0, 100),
  xlab = "m", ylab = "Classification success (%)"
)

# plot classification success for sequential values of m:
for (i in 1:m_max) {
  cap_result <- CAPdiscrim(
    dist_func ~ site,
    data = as.data.frame(tree_lichens_func_plot),
    axes = 2, m = i)
  points(i, cap_result$percent)
}
