# ---------------------------------------------------------------------------- #
# Preliminary Analysis (full sample)
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(repmis)
library(dplyr)
library(DataCombine)
library(arm)
library(texreg)
#library(Zelig)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load combined data set
main <- import('data/main_combined.csv')

# Set as factors
main$country <- as.factor(main$country)
main$year <- as.factor(main$year)
main$quarter <- as.factor(main$quarter)
main$executive_election_4qt <- as.factor(main$executive_election_4qt)

FindDups(main, c('country', 'year_quarter'))

# Simple logistic regressions tightening --------
t1 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   finstress_qt_mean + bis_housing_change +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

t2 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   `_1_gini_market` +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

t3 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

t4 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi + executive_election_4qt +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

t5 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation + 
                   cbi + fiscal_trans_gfs +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

t6 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi + domestic_credit_change +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))


# Create display table for tightening ---------
est_tighten <- list(t1, t2, t3, t4, t5, t6)

screenreg(est_tighten,
          omit.coef = 'factor')

texreg(est_tighten,
       omit.coef = 'factor',
       table = FALSE)

# Simple logistic regressions loosening -------
l1 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   finstress_qt_mean + bis_housing_change +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

l2 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   `_1_gini_market` + 
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

l3 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))


l4 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   execrlc + executive_election_4qt +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

l5 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   fiscal_trans_gfs +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

l6 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   domestic_credit_change +
                   factor(country) + factor(quarter)
               , data = main, family = binomial(link = 'logit'))

# Create display table for loosening ---------
est_loosen <- list(l1, l2, l3, l4, l5, l6)

screenreg(est_loosen,
          omit.coef = 'factor')

texreg(est_loosen,
       omit.coef = 'factor',
       table = FALSE)
