# -*- coding: utf-8 -*-
# %% [markdown]
# prerequisite:
# - get_DTE_cohort.ipynb

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %% [markdown]
# # Gathering the data

# %%
this_drug <- "SGLT2i"

# %%
load("dte_cohort_wNontreat_data.rds")
dte_cohort_data <- this.data

# %%
logical_vars <- dte_cohort_data %>%
sapply(class) %>%
as.data.frame %>%
rename("type" = ".") %>%
rownames_to_column("var") %>%
filter(type == "logical") %>%
pull(var)

dte_cohort_data <- dte_cohort_data %>%
mutate(across(all_of(logical_vars), ~ as.numeric(.)))

# %%
matchingVars <- read.csv("ps_covariates.csv") %>% mutate(issues = NA)
matchingFormula <- as.formula(paste0("treatment ~ ", paste(matchingVars$var, collapse = " + ")))

matchingVars
matchingFormula

# %%
Semaglutide_Population_for_Semaglutide_vs_Drug <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
Drug_Population_for_Semaglutide_vs_Drug <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

varname_semaglutide_age_at_index_years <- "Semaglutide_age_at_index_years"
varname_thisdrug_age_at_index_years <- paste0(this_drug, "_age_at_index_years")

varname_semaglutide_mdd_to_index_days <- "Semaglutide_mdd_to_index_days"
varname_thisdrug_mdd_to_index_days <- paste0(this_drug, "_mdd_to_index_days")

varname_semaglutide_first_drug_record <- "Semaglutide_first_drug_record"
varname_thisdrug_first_drug_record <- ifelse(this_drug == "Nontreatment", "Nontreatment_index", paste0(this_drug, "_first_drug_record"))

this.data <- dte_cohort_data %>% 
filter(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) | !!sym(Drug_Population_for_Semaglutide_vs_Drug)) %>%
mutate(treatment = ifelse(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) == TRUE, 1, 0)) %>%
mutate(age_at_index_years = ifelse(treatment, 
                                   !!sym(varname_semaglutide_age_at_index_years), 
                                   !!sym(varname_thisdrug_age_at_index_years))) %>%
mutate(mdd_to_index_days = ifelse(treatment,
                                  !!sym(varname_semaglutide_mdd_to_index_days), 
                                  !!sym(varname_thisdrug_mdd_to_index_days))) %>%
mutate(first_drug_record = as.Date(ifelse(treatment, 
                                   !!sym(varname_semaglutide_first_drug_record), 
                                   !!sym(varname_thisdrug_first_drug_record))),
       first_drug_record_year = year(first_drug_record)) %>%

as.data.frame

# %% [markdown]
# # Variable selection

# %% [markdown]
# ## Near zero variance

# %%
this_issue <- "near zero variance"

nzv <- nearZeroVar(this.data[,matchingVars$var], saveMetrics=TRUE)
these_vars <- rownames(nzv)[ nzv$nzv ]
print(these_vars)  

matchingVars.1 <- matchingVars %>% mutate(issues = ifelse(var %in% these_vars, 
                                                            ifelse(is.na(issues), 
                                                                   this_issue, 
                                                                   paste0(issues, ", ", this_issue)), 
                                                            issues))

# %%
# 1 Check for zero-variance covariates
this_issue <- "zero variance"

these_vars <- this.data %>%
dplyr::select(all_of(matchingVars$var)) %>%
sapply(function(x){length(unique(x))}) %>%
as.data.frame %>%
rownames_to_column("var") %>%
dplyr::rename(n_unique = ".") %>%
arrange(n_unique) %>%
filter(n_unique < 2) %>% 
pull(var)

# any that return 1 are constant and should be dropped or combined
matchingVars.2 <- matchingVars.1 %>% mutate(issues = ifelse(var %in% these_vars, 
                                                            ifelse(is.na(issues), 
                                                                   this_issue, 
                                                                   paste0(issues, ", ", this_issue)), 
                                                            issues))

# %% [markdown]
# ## Linear combinations

# %%
this_issue <- "linear combination with another variable"

combos <- findLinearCombos(this.data %>% 
                           dplyr::select(
                               matchingVars %>%
                               filter(var_type %in% c("continuous", "binary")) %>%
                               pull(var)
                           ))
these_vars <- matchingVars$var[combos$remove]

matchingVars.3 <- matchingVars.2 %>% mutate(issues = ifelse(var %in% these_vars, 
                                                            ifelse(is.na(issues), 
                                                                   this_issue, 
                                                                   paste0(issues, ", ", this_issue)), 
                                                            issues))

# %%
this_issue <- "highly correlated with another variable"

# 2) Check for perfect pairwise collinearity
# compute correlations
cor_mat <- cor(this.data %>% 
               dplyr::select(
                   matchingVars %>%
                   filter(var_type %in% c("continuous", "binary")) %>%
                   pull(var)
               ), 
               use = "pairwise.complete.obs")

which(abs(cor_mat) > 0.8, arr.ind = TRUE) %>%
as.data.frame %>%
filter(!row == col) %>%
mutate(across(
    .cols = everything(),
    .fns  = ~ ifelse(.x < 20, "<20", .x)
))

these_cors <- which(abs(cor_mat) > 0.8, arr.ind = TRUE, useNames = TRUE) %>%
as.data.frame %>%
left_join(data.frame(row = 1:nrow(cor_mat), row.name = rownames(cor_mat)), by = join_by(row == row)) %>%
left_join(data.frame(col = 1:ncol(cor_mat), col.name = colnames(cor_mat)), by = join_by(col == col)) %>%
filter(!row == col) %>% 
mutate(correlated = col.name) %>%
rename(var = row.name) %>%
dplyr::select(var, correlated) 

these_vars <- these_cors %>%
pull(var)

matchingVars.4 <- matchingVars.3 %>% mutate(issues = ifelse(var %in% these_vars, 
                                                            ifelse(is.na(issues), 
                                                                   this_issue, 
                                                                   paste0(issues, ", ", this_issue)), 
                                                            issues)) %>%
left_join(these_cors)

# %%
# options(repr.plot.width=25, repr.plot.height=25)
# ggcorrplot(cor_mat,
           # hc.order = TRUE,       # hierarchical clustering order
           # type = "lower",        # show only the lower triangle
           # lab = TRUE,            # add correlation coefficient labels
           # tl.cex = 16
          # )    

# %% [markdown]
# ## Positivity

# %%
this_issue <- "fails positivity assumption"

these_vars <- c()
for(this_var in matchingVars %>% filter(!var_type == "continuous") %>% pull(var)){
    this_table <- table(this.data[,c("treatment", this_var)], useNA = "ifany")

    this_name <- colnames(as.data.frame(this_table))[2]
    this_col <- sym(this_name)

    this_table %>%
    as.data.frame %>%
    mutate(!!this_col := as.character(!!this_col)) %>%
    pivot_wider(names_from = !!this_col, values_from = Freq, names_prefix = paste0(this_name, "_")) %>%
    mutate(across(
        2:3,
        .fns  = ~ ifelse(.x < 20, "<20", .x)
    )) %>% print
    "\n\n" %>% cat

    if(sum(this_table == 0) > 0){these_vars <- c(these_vars, this_var)}
}

matchingVars.5 <- matchingVars.4 %>% mutate(issues = ifelse(var %in% these_vars, 
                                                            ifelse(is.na(issues), 
                                                                   this_issue, 
                                                                   paste0(issues, ", ", this_issue)), 
                                                            issues))

# %% [markdown]
# ## Balance

# %%
matchingVars %>% 
rowwise() %>%
mutate(smd = ifelse(var_type %in% c("continuous", "binary"), 
                    stat_smd(var, "treatment", this.data) %>% round(2), 
                    NA
                   )
      ) %>% 
filter(abs(smd) < 0.05) %>%
pull(var)

# %%
matchingVars %>% 
rowwise() %>%
mutate(pval = stat_pval(var, "treatment", this.data) %>% round(2)) %>%
filter(pval > 0.05) %>%
pull(var)

# %%
this_issue <- "negligible imbalance"

these_vars <- matchingVars %>% 
rowwise() %>%
mutate(smd = ifelse(var_type %in% c("continuous", "binary"), 
                    stat_smd(var, "treatment", this.data) %>% round(2), 
                    NA
                   )
      ) %>% 
filter(abs(smd) < 0.05) %>%
pull(var)

matchingVars.6 <- matchingVars.5 %>% mutate(issues = ifelse(var %in% these_vars, 
                                                            ifelse(is.na(issues), 
                                                                   this_issue, 
                                                                   paste0(issues, ", ", this_issue)), 
                                                            issues))

# %%
this_issue <- "does not predict treatment"

these_vars <- matchingVars %>% 
rowwise() %>%
mutate(pval = stat_pval(var, "treatment", this.data) %>% round(2)) %>%
filter(pval > 0.05) %>%
pull(var)

matchingVars.7 <- matchingVars.6 %>% mutate(issues = ifelse(var %in% these_vars, 
                                                            ifelse(is.na(issues), 
                                                                   this_issue, 
                                                                   paste0(issues, ", ", this_issue)), 
                                                            issues))
# save this one
matchingVars.7

# %% [markdown]
# ## Filter the matching variables

# %%
matchingVars.8 <- matchingVars.7 %>%
filter(is.na(issues) | issues == "highly correlated with another variable")

matchingVars.8

# %%
matchingVars.9 <- matchingVars.8 %>%
rowwise() %>%
mutate(issues = ifelse(!is.na(issues) & !correlated %in% matchingVars.8$var, NA, issues)) %>%
mutate(correlated = ifelse(is.na(issues), NA, correlated)) %>%
ungroup()

matchingVars.9

# %%
matchingVars.10 <- matchingVars.9
while(sum(matchingVars.10$issues == "highly correlated with another variable", na.rm = T) > 0){
    rm.var <- (matchingVars.10 %>% filter(!is.na(correlated)) %>% pull(correlated))[1]
    matchingVars.10 <- matchingVars.10 %>% 
    filter(!var == rm.var) %>%
    mutate(issues = ifelse(correlated == rm.var, NA, issues)) %>%
    mutate(correlated = ifelse(correlated == rm.var, NA, correlated))
}

matchingVars.10

# %%
matchingVars.final <- matchingVars.10 %>%
dplyr::select(var, var_type)

matchingFormula <- as.formula(paste0("treatment ~ ", paste(matchingVars.final$var, collapse = " + ")))

# save this one
matchingVars.final
matchingFormula

write.csv(matchingVars.final, paste0("PS_Covariates-", this_drug, ".csv"))

# %% [markdown]
# # Propensity scoring

# %% [markdown]
# ## Run Twang

# %%
set.seed(123)
ps.out <- twang::ps(
  matchingFormula,
  data              = this.data,
  estimand          = "ATT",
  n.trees           = 5000,
  shrinkage         = 0.01,
  interaction.depth = 2,
  bag.fraction      = 0.8,
  n.minobsinnode    = 10,
  stop.method       = c("es.mean", "ks.max"),
  version           = "xgboost",
  verbose           = FALSE
)

# %%
this.data.ps <- this.data %>% mutate(treatment_name = ifelse(treatment, "Semaglutide", this_drug))
this.data.ps$weight <- get.weights(ps.out, stop.method="es.mean")
this.data.ps$pscore <- ps.out$ps$es.mean.ATT
this.data.ps$logit_pscore <- log( this.data.ps$pscore / (1 - this.data.ps$pscore) )

# %% [markdown]
# ESS will give an estimate of the number of comparison participants that are comparable to the treatment group when estimand = "ATT"

# %% [markdown]
# ## Summary of the PS model

# %%
summary(ps.out)   # shows covariate balance at each candidate stop
                 # standardized mean differences vs. number of trees

# %% [markdown]
# ## Summary Figures

# %% [markdown]
# ### Distribution of propensity score by treatment group

# %%
options(repr.plot.width=14, repr.plot.height=10)

# ggplot(this.data.ps, aes(x=pscore, fill = treatment_name)) +
  # geom_histogram(position="identity", alpha=0.5, bins = 20) +
  # theme(legend.position="top") +
# theme_minimal(base_size = 25)
# ggplot(this.data.ps, aes(x=logit_pscore, fill = treatment_name)) +
  # geom_histogram(position="identity", alpha=0.5, bins = 20) +
  # theme(legend.position="top") +
# theme_minimal(base_size = 25)

# %% [markdown]
# ### Comparison of propensity score to weight

# %%
# ggplot(this.data.ps %>% mutate(treatment_name = factor(treatment_name, levels = c("Semaglutide", this_drug))), aes(x = pscore, y = weight, color = treatment_name)) +
# geom_point() +
# scale_color_viridis_d(option = "plasma", end = .7, name = "Treatment") +
# labs(
        # title    = "Weight vs Propensity Score by Treatment",
        # subtitle = paste0("Comparison of weight and propensity score, split by Semaglutide vs. ", this_drug),
        # x        = "Propensity Score",
        # y        = "Weight",
        # fill     = "Treatment"
    # ) +
    # theme_minimal(base_size = 25) +
    # theme(
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor   = element_blank()
    # )

# %%
# FIGURE FORMATTING
# 1) Grab twang’s own defaults (note: canonical.theme("pdf") usually matches twang’s color scheme):
base.theme <- canonical.theme("pdf", color = TRUE)

#    (b) enlarge *all* text elements by default:
base.theme$fontsize$text   <- 16   # make baseline text = 16pt  
base.theme$fontsize$points <- 12   # for any text inside symbols (rare)

#    (c) if you want to go even bigger for specific parts, bump those cex’s:
base.theme$par.xlab.text  <- list(cex = 2)   # ~21pt x-axis label
base.theme$par.ylab.text  <- list(cex = 2)   # ~21pt y-axis label

#    (d) thicken lines and enlarge symbols by default:
base.theme$plot.line   <- list(lwd = 2)
base.theme$plot.symbol <- list(cex = 1.5)

#    (e) enlarge tick-mark labels (if needed):
base.theme$axis.text <- list(cex = 1.3)         # ~20.8pt tick labels

# 3) Apply these settings globally for all lattice/Trellis plots (including twang’s):
trellis.par.set(base.theme)

options(repr.plot.width=14, repr.plot.height=10)

# %% [markdown]
# ### Balance measure as a function of GBM iterations
#
# The following plot gives the balance measures as a
# function of the number of iterations in the gradient boosting algorithm, with higher iterations corresponding
# to more complicated fitted models

# %%
# plot(ps.out, plots = 1, par.strip.text = list(cex = 2))

# %% [markdown]
# ### Boxplot of treatment/control propensity scores
#
# The boxplots illustrate the spread of the estimated propensity scores in the treatment and comparison groups. 
# Whereas propensity score stratification requires considerable overlap in these spreads, excellent covariate balance
# can often be achieved with weights, even when the propensity scores estimated for the treatment and control groups show
# little overlap.

# %%
# plot(ps.out, plots = 2, par.strip.text = list(cex = 2))   

# %% [markdown]
# ### Standardized effect size of pretreatment variables
#
# The effect size plot illustrates the effect of weights on the magnitude of differences between groups on
# each pretreatment covariate. These magnitudes are standardized using the standardized effect size described
# earlier. In these plots, substantial reductions in effect sizes are observed for most variables (blue lines), with
# only one variable showing an increase in effect size (red lines), but only a seemingly trivial increase. Closed
# red circles indicate a statistically significant difference, many of which occur before weighting, none after.
# In some analyses variables can have very little variance in the treatment group sample or the entire sample
# and group differences can be very large relative to the standard deviations. In these situations, the user is
# warned that some effect sizes are too large to plot.

# %%
# plot(ps.out, plots = 3, par.strip.text = list(cex = 2))   

# %% [markdown]
# ### t-test p-values for weighted pretreatment variables
#
# When many of the p-values testing individual covariates for balance are very small, the groups are clearly
# imbalanced and inconsistent with what we would expect had the groups been formed by random assignment.
# After weighting we would expect the p-values to be larger if balance had been achieved. We use a QQ plot
# comparing the quantiles of the observed p-values to the quantiles of the uniform distribution (45 degree line)
# to conduct this check of balance. Ideally, the p-values from independent tests in which the null hypothesis
# is true will have a uniform distribution. Although the ideal is unlikely to hold even if we had random
# assignment (Bland, 2013), severe deviation of the p-values below the diagonal suggests lack of balance and
# p-values running at or above the diagonal suggests balance might have been achieved. The p-value plot
# (plots=4 or plots="t") allows users to visually to inspect the p-values of the t-tests for group differences
# in the covariate means.

# %%
# plot(ps.out, plots = 4, par.strip.text = list(cex = 2))   

# %% [markdown]
# ### Kolmogorov-Smirnov p-values for weighted pretreatment variables
#
# One can inspect similar plots as directly above for the KS statistic.

# %%
# plot(ps.out, plots = 5, par.strip.text = list(cex = 2))   

# %% [markdown]
# ### Histogram of weights for treatment/control

# %%
# plot(ps.out, plots = 6, par.strip.text = list(cex = 2))   

# %% [markdown]
# ## Balance tables

# %%
bal.table(ps.out)

# %% [markdown]
# # Propensity score weighting

# %% [markdown]
# ### Saving the weighted data

# %%
weighted.data <- this.data.ps
save(weighted.data, file = paste0("PS_Weighted_Dataset-", this_drug, ".rds"))

# weighted.data %>% head

# %% [markdown]
# ### Total weight of each treatment group

# %%
sema_weight_sum <- sum(this.data.ps %>% filter(treatment_name == "Semaglutide") %>% pull(weight))
alte_weight_sum <- sum(this.data.ps %>% filter(treatment_name == this_drug) %>% pull(weight))

cat(sprintf(
    "The sum of the weights in the Semaglutide group is: %s\n\n",
    sema_weight_sum
))

cat(sprintf(
    "The sum of the weights in the %s group is: %s",
    this_drug,
    round(alte_weight_sum, 4)
))

# %% [markdown]
# ### Extreme weights in the alternate drug group

# %%
total_n_alternate <- this.data.ps %>% filter(treatment_name == this_drug) %>% nrow 
total_small_weights <- this.data.ps %>% filter(treatment_name == this_drug & weight < 0.01) %>% nrow 
total_big_weights <- this.data.ps %>% filter(treatment_name == this_drug & weight > 0.99) %>% nrow

cat(sprintf(
    "There are a total of %s (%s) weights in the comparison group that are less than 0.01\n\n",
    comma(total_small_weights),
    percent(total_small_weights / total_n_alternate)
))

cat(sprintf(
    "There are a total of %s (%s) weights in the comparison group that are greater than 0.99",
    comma(total_big_weights),
    percent(total_big_weights / total_n_alternate)
))

# %% [markdown]
# ## Histogram showing the distribution of weights in the Semaglutide vs Alternate Treatment group

# %%
# ggplot(this.data.ps, aes(x=weight, fill = treatment_name)) +
# geom_histogram(position="identity", alpha=0.5, binwidth = 0.1) +
# theme(legend.position="top") +
# theme_minimal(base_size = 20)+
# labs(
    # title    = paste0("Distribution of the PS Weights"),
    # x        = "Weight",
    # y        = "Count"
# ) 

# %% [markdown]
# ### Create a weighted dataset using the `survey` package

# %%
design.ps <- svydesign(ids=~person_id, weights=~weight, data=this.data.ps)

# %% [markdown]
# ### View distributions before and after weighting

# %%
theme_set(
  theme_grey(base_size = 20)
)
options(repr.plot.width=20, repr.plot.height=10)

for(i in 1:nrow(matchingVars)){
    this_var <- matchingVars$var[i]
    inc_in_ps <- this_var %in% matchingVars.final$var

    mu <- this.data.ps %>%
    group_by(treatment_name) %>%
    summarise(grp.mean = mean(.data[[this_var]], na.rm = TRUE), .groups  = "drop")

    # p1 <- ggplot(this.data.ps, aes(x=!!sym(this_var), fill = treatment_name)) +
    # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
    # geom_density(alpha=0.6)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=treatment_name),
               # linetype="dashed")+
    # theme(legend.position="top")+
    # labs(
        # title    = paste0("Distribution of ", this_var, " before weighting"),
        # subtitle = ifelse(inc_in_ps, "Included in PS Matching", "NOT Included in PS Matching"),
        # x        = this_var,
        # y        = "Count"
    # ) 


    mu <- svyby(formula = formula(paste0("~", this_var)), by= ~treatment_name, design = design.ps, FUN = svymean, na.rm = TRUE)
    mu <- mu[,1:2]
    colnames(mu) <- c("treatment_name", "grp.mean")
    rownames(mu) <- NULL

    # p2 <- ggplot(this.data.ps, aes(x=!!sym(this_var), fill = treatment_name, weight = weight)) +
    # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
    # geom_density(alpha=0.6)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=treatment_name),
               # linetype="dashed")+
    # theme(legend.position="top")+
    # labs(
        # title    = paste0("Distribution of ", this_var, " after weighting"),
        # subtitle = ifelse(inc_in_ps, "Included in PS Matching", "NOT Included in PS Matching"),
        # x        = this_var,
        # y        = "Count"
    # ) 

    #print(p1 + p2)
}

# %% [markdown]
# # Propensity score matching

# %% [markdown]
# ### Create the matched dataset

# %%
m.out <- matchit(
  treatment ~ logit_pscore,
  data       = this.data.ps,
  method     = "nearest",        # 1:1 nearest‐neighbor
  replace    = FALSE,            # no replacement
  ratio      = 2                 # for 2:1 matching ratio
)

matched.data <- match.data(m.out)
save(matched.data, file = paste0("PS_Matched_Dataset-", this_drug, ".rds"))

# 3c) Check balance after matching  
summary(m.out, standardize = TRUE)

# %% [markdown]
# ### Propensity score distribution before and after NN matching

# %%
mu <- this.data.ps %>%
    group_by(treatment_name) %>%
    summarise(grp.mean = mean(logit_pscore, na.rm = TRUE), .groups  = "drop")

# p1 <- ggplot(this.data.ps, aes(x=logit_pscore, fill = treatment_name)) +
    # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
    # geom_density(alpha=0.6)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=treatment_name),
               # linetype="dashed")+
    # theme(legend.position="top")+
    # labs(
        # title    = paste0("Distribution of Logit PS before NN Selection"),
        # x        = "Logit PS",
        # y        = "Count"
    # ) 

mu <- matched.data %>%
    group_by(treatment_name) %>%
    summarise(grp.mean = mean(logit_pscore, na.rm = TRUE), .groups  = "drop")

# p2 <- ggplot(matched.data, aes(x=logit_pscore, fill = treatment_name)) +
    # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
    # geom_density(alpha=0.6)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=treatment_name),
               # linetype="dashed")+
    # theme(legend.position="top")+
    # labs(
        # title    = paste0("Distribution of Logit PS after NN Selection"),
        # x        = "Logit PS",
        # y        = "Count"
    # ) 

# print(p1 + p2)

# %% [markdown]
# ### Weight distribution before and after NN matching

# %%
mu <- this.data.ps %>%
    group_by(treatment_name) %>%
    summarise(grp.mean = mean(weight, na.rm = TRUE), .groups  = "drop")

# p1 <- ggplot(this.data.ps, aes(x=weight, fill = treatment_name)) +
    # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
    # geom_density(alpha=0.6)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=treatment_name),
               # linetype="dashed")+
    # theme(legend.position="top")+
    # labs(
        # title    = paste0("Distribution of weight before NN Selection"),
        # x        = "Weight",
        # y        = "Count"
    # ) 

mu <- matched.data %>%
    group_by(treatment_name) %>%
    summarise(grp.mean = mean(weight, na.rm = TRUE), .groups  = "drop")

# p2 <- ggplot(matched.data, aes(x=weight, fill = treatment_name)) +
    # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
    # geom_density(alpha=0.6)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=treatment_name),
               # linetype="dashed")+
    # theme(legend.position="top")+
    # labs(
        # title    = paste0("Distribution of weight after NN Selection"),
        # x        = "Weight",
        # y        = "Count"
    # ) 

# print(p1 + p2)

# %% [markdown]
# ### Weight vs PS before and after NN matching

# %%
# p1 <- ggplot(this.data.ps, aes(x = pscore, y = weight, color = treatment_name)) +
# scale_color_viridis_d(option = "plasma", end = .7, name = "Treatment") +
# geom_point() +
# theme_minimal(base_size = 30) +
# theme(
    # legend.position = c(0.02, 0.98),   # near top-left inside the panel
    # legend.justification = c(0, 1),     # anchor legend's bottom-left corner there
    # legend.background    = element_rect(fill = "white", colour = "black"),
    # legend.key           = element_rect(fill = "white", colour = NA)
# )+
# labs(
    # title    = "Before NN Selection",
    # x        = "PScore",
    # y        = "Weight"
# ) 

# p2 <- ggplot(matched.data, aes(x = pscore, y = weight, color = treatment_name)) +
# scale_color_viridis_d(option = "plasma", end = .7, name = "Treatment") +
# geom_point() +
# theme_minimal(base_size = 30) +
# theme(
    # legend.position = c(0.02, 0.98),   # near top-left inside the panel
    # legend.justification = c(0, 1),     # anchor legend's bottom-left corner there
    # legend.background    = element_rect(fill = "white", colour = "black"),
    # legend.key           = element_rect(fill = "white", colour = NA)
# )+
# labs(
    # title    = "After NN Selection",
    # x        = "PScore",
    # y        = "Weight"
# ) 

# print(p1 + p2)

# %% [markdown]
# ### Extreme weights in the alternate drug group

# %%
total_n_alternate <- matched.data %>% filter(treatment_name == this_drug) %>% nrow 
total_small_weights <- matched.data %>% filter(treatment_name == this_drug & weight < 0.01) %>% nrow 
total_big_weights <- matched.data %>% filter(treatment_name == this_drug & weight > 0.99) %>% nrow

cat(sprintf(
    "There are a total of %s (%s) weights in the comparison group that are less than 0.01\n\n",
    comma(total_small_weights),
    percent(total_small_weights / total_n_alternate)
))

cat(sprintf(
    "There are a total of %s (%s) weights in the comparison group that are greater than 0.99",
    comma(total_big_weights),
    percent(total_big_weights / total_n_alternate)
))

# %% [markdown]
# ### Statistical summary of matched data

# %%
std_diff <- function(var, treat, data) {
    z <- data[[treat]]
    x <- data[[var]]
    xT <- x[z == 1]
    xC <- x[z == 0]
    if (is.numeric(x)) {
        meanT <- mean(xT, na.rm = TRUE)
        meanC <- mean(xC, na.rm = TRUE)
        sdT <- sd(xT, na.rm = TRUE)
        sdC <- sd(xC, na.rm = TRUE)
        # pooled SD
        s_pooled <- sqrt((sdT^2 + sdC^2) / 2)
        # Handle case where pooled SD is 0
        if (s_pooled == 0) {
            return(NA_real_)
        }
        return((meanT - meanC) / s_pooled)
    } else {
        # Presume a factor or binary variable; convert to numeric 0/1
        # If it’s a factor, coerce to numeric proportion
        tabT <- table(xT)
        tabC <- table(xC)

        # proportion in treated with x == 1 (if x is 0/1)
        pT <- mean(as.numeric(xT == levels(x)[2]), na.rm = TRUE)  
        pC <- mean(as.numeric(xC == levels(x)[2]), na.rm = TRUE)
        p_pool <- (pT * length(xT) + pC * length(xC)) / (length(xT) + length(xC))

        denom <- sqrt(p_pool * (1 - p_pool))
        if (denom == 0) {
            return(NA_real_)
        }
        return((pT - pC) / denom)
    }
}

stat_pval <- function(var, treat, data){ #  vname, "treatment", matched_data
    z <- data[[treat]]
    x <- data[[var]]
    xT <- x[z == 1]
    xC <- x[z == 0]
    # 2c. Decide which test to run
    if (is.numeric(x)) {
        # Two‐sample t‐test (Welch’s by default)
        test_obj <- try(t.test(xT, xC), silent = TRUE)
        # In very rare cases (e.g. all values identical) t.test might error; handle gracefully:
        if (inherits(test_obj, "htest")) {
            p_val <- test_obj$p.value
        } else {
            p_val <- NA_real_
        }
    } else {
        # Build a 2×2 table for chi‐square or Fisher’s exact
        tab <- table(x, z)
        # If any expected cell count < 5, use Fisher’s exact
        chi_obj <- try(chisq.test(tab, correct = FALSE), silent = TRUE)
        if (inherits(chi_obj, "htest") && all(chi_obj$expected >= 5)) {
            p_val <- chi_obj$p.value
        } else {
            fish_obj <- try(fisher.test(tab), silent = TRUE)
            p_val     <- if (inherits(fish_obj, "htest")) fish_obj$p.value else NA_real_
                  }
    }
    return(p_val)
}

stat_ks <- function(var, treat, data){
    z <- data[[treat]]
    x <- data[[var]]
    xT <- x[z == 1]
    xC <- x[z == 0]
    if (is.numeric(x)) {
        ks_obj <- try(suppressWarnings(ks.test(xT, xC)), silent = TRUE)
    } else {
        # Coerce factor to integer codes (e.g. 1 vs. 2), then run KS:
        xT_num <- as.numeric(factor(xT))
        xC_num <- as.numeric(factor(xC))
        ks_obj <- try(suppressWarnings(ks.test(xT_num, xC_num)), silent = TRUE)
    }
  
    if (inherits(ks_obj, "htest")) {
        ks_stat  <- as.numeric(ks_obj$statistic)
        ks_pval  <- ks_obj$p.value
    } else {
        ks_stat <- NA_real_
        ks_pval <- NA_real_
    }
    return(list(ks_stat, ks_pval))
    }
    
results_list <- vector("list", length(matchingVars))

for (i in 1:nrow(matchingVars)) {
    vname <- matchingVars[[i,1]]
    inc_in_ps <- vname %in% matchingVars.final$var
  
    # Extract values for treated vs. control
    xT <- matched.data[[vname]][matched.data$treatment == 1]
    xC <- matched.data[[vname]][matched.data$treatment == 0]
  
    # Compute mean/proportion
    if (is.numeric(matched.data[[vname]])) {
        meanT <- mean(xT, na.rm = TRUE)
        meanC <- mean(xC, na.rm = TRUE)
    
    } else {
    # Coerce factor to numeric proportion of “level 2”
    # (adjust this if you want the proportion of a specific level)
        meanT <- mean(as.numeric(xT == levels(matched.data[[vname]])[2]), na.rm = TRUE)
        meanC <- mean(as.numeric(xC == levels(matched.data[[vname]])[2]), na.rm = TRUE)
    }
  
    raw_diff  <- meanT - meanC
    smd_value <- std_diff(vname, "treatment", matched.data)
    p_value <- stat_pval(vname, "treatment", matched.data)
    stat_ks_out <- stat_ks(vname, "treatment", matched.data)
    ks_stat <- stat_ks_out[[1]]
    ks_pval <- stat_ks_out[[2]]
  
    results_list[[i]] <- data.frame(
        Covariate       = vname,
        Included_in_PS  = inc_in_ps,
        Mean_Treated    = round(meanT, 3),
        Mean_Control    = round(meanC, 3),
        Raw_Difference  = round(raw_diff, 3),
        Std_Difference  = round(smd_value, 3),
        Stat_P_Value         = round(p_value, 4),
        KS_Stat         = round(ks_stat, 4),
        KS_Pvalue       = round(ks_pval, 4),
        stringsAsFactors = FALSE
    )
}

# Combine into one data.frame
balance_table <- do.call(rbind, results_list)
row.names(balance_table) <- NULL

balance_table

# %% [markdown]
# ### View distributions before and after matching

# %%
theme_set(
  theme_grey(base_size = 20)
)
options(repr.plot.width=20, repr.plot.height=10)

for(i in 1:nrow(matchingVars)){
    this_var <- matchingVars$var[i]
    inc_in_ps <- this_var %in% matchingVars.final$var

    #matchingVars.final$var[i]

    mu <- this.data.ps %>%
    group_by(treatment_name) %>%
    summarise(grp.mean = mean(.data[[this_var]], na.rm = TRUE), .groups  = "drop")

    # p1 <- ggplot(this.data.ps, aes(x=!!sym(this_var), fill = treatment_name)) +
    # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
    # geom_density(alpha=0.6)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=treatment_name),
               # linetype="dashed")+
    # theme(legend.position="top") +
    # labs(
        # title    = paste0("Distribution of ", this_var, " before matching"),
        # subtitle = ifelse(inc_in_ps, "Included in PS Matching", "NOT Included in PS Matching"),
        # x        = this_var,
        # y        = "Count") 

    mu <- matched.data %>%
    group_by(treatment_name) %>%
    summarise(grp.mean = mean(.data[[this_var]], na.rm = TRUE), .groups  = "drop")

    # p2 <- ggplot(matched.data, aes(x=!!sym(this_var), fill = treatment_name)) +
    # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 20) +
    # geom_density(alpha=0.6)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=treatment_name),
               # linetype="dashed")+
    # theme(legend.position="top") +
    # labs(
        # title    = paste0("Distribution of ", this_var, " after matching"),
        # subtitle = ifelse(inc_in_ps, "Included in PS Matching", "NOT Included in PS Matching"),
        # x        = this_var,
        # y        = "Count") 

   # print(p1 + p2)
}

# %%
