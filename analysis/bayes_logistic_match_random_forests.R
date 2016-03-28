# ---------------------------------------------------------------------------- #
# Bayes Logistics to Match Random Forests
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

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load combined data set
main <- import('data/main_combined.csv')

# Inequality transformations -------
main$redist_absolute <- main$gini_market - main$gini_net
main$redist_relative <- (main$redist_absolute / main$gini_market) * 100
main$gini_diff_red <- main$gini_market - main$redist

# Find variable correlations
keepers <- c('any_tighten', 'lag_cumsum_any_tighten',
             'gdp_growth', 'gdp_per_capita', 'inflation', 
             'bis_housing_change',  'bis_credit_change', 'cbi',
             'executive_election_4qt', 'cb_policy_rate', 
             'cb_policy_rate_change', 'gini_net',
             'redist_absolute', 'uds_mean')

keeper_labels <- c('Any MPR Tightening', 'Cum. Tight. (lag)', 
                   'GDP Growth', 'GDP/Capita', 'Inflation',
                   'Housing Chng', 'Credit Chng', 'CBI', 'Election', 
                   'Policy Rate', 'Policy Rate Chng', 
                   'Gini Net', 'Abs. Redist.', 'UDS')

subbed <- main[, keepers[-1]]
names(subbed) <- keeper_labels[-1]
iv_correlations <- cor(subbed, use = 'complete.obs')
print(xtable(iv_correlations, 
             caption = 'Predictor Variable Correlations'),
      caption.placement = 'top',
      size = 'tiny',
      file = 'papers/tables/iv_correlations.tex')

# Set as factors
main$country <- factor(main$country)
# main$year <- as.factor(main$year)
# main$quarter <- as.factor(main$quarter)
main$executive_election_4qt <- factor(main$executive_election_4qt)


main$mapp_cb_chair <- NA
main$mapp_cb_chair[main$mapp < 3 & !is.na(main$mapp)] <- 0
main$mapp_cb_chair[main$mapp >= 3] <- 1

main$mof_cb_chair <- NA
main$mof_cb_chair[main$mof < 3 & !is.na(main$mof)] <- 0
main$mof_cb_chair[main$mof >= 3] <- 1

main$mapp_cb_chair <- factor(main$mapp_cb_chair)
main$mapp <- factor(main$mapp)
main$mof <- factor(main$mof)
main$mipp <- factor(main$mipp)

# Rescale DV to get estimates in a sensible interpretable direction
main$any_tighten[main$any_tighten == 0] <- 'No Change'
main$any_tighten[main$any_tighten == 1] <- 'Tighten'

main$any_loosen[main$any_loosen == 0] <- 'No Change'
main$any_loosen[main$any_loosen == 1] <- 'Loosen'

main$any_tighten <- factor(main$any_tighten, 
                           levels = c('Tighten', 'No Change'))
main$any_loosen <- factor(main$any_loosen, 
                          levels = c('Loosen', 'No Change'))

FindDups(main, c('country', 'year_quarter'))

# Only democracies ------
#dem <- main %>% filter(polity2 > 5)

dem = main

# Keep complete cases
dem_no_na_1 <- dem %>% DropNA(keepers)


# Bayes logit for tightening ------------
glm_1 <- glm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + 
                 bis_housing_change +
                 bis_credit_change +
                 inflation +
                 #gini_market + 
                 gini_net +
                 redist_relative +
                 executive_election_4qt +
                 executive_election_4qt_after +
                 cb_policy_rate + cb_policy_rate_change +
                 cbi + 
                 #polconv + 
                 uds_mean + 
                 country + 
                 year # + quarter
             , data = dem_no_na_1, family = binomial(link = 'logit'))

glm_2 <- glm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + 
                 bis_housing_change +
                 bis_credit_change +
                 inflation +
                 #gini_market + 
                 gini_net +
                 redist_relative +
                 executive_election_4qt +
                 executive_election_4qt_after +
                 cb_policy_rate + cb_policy_rate_change +
                 cbi + 
                 #polconv + 
                 uds_mean + 
                 country + 
                 year # + quarter
             , data = dem_no_na_1, family = binomial(link = 'logit'))

texreg(list(bglm_1, bglm_2),
       omit.coef = 'country',
       custom.model.names = c('Tightening MPR', 'Loosening MPR'),
       table = FALSE,
       file = 'papers/tables/garbage_can_ordinary_logit.tex'
)

# Bayes logit for tightening ------------
bglm_1 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + 
                       bis_housing_change +
                       bis_credit_change +
                       inflation +
                       #gini_market + 
                       gini_net +
                       redist_relative +
                       executive_election_4qt +
                       executive_election_4qt_after +
                       cb_policy_rate + cb_policy_rate_change +
                       cbi + 
                       #polconv + 
                       uds_mean + 
                       country + 
                       year # + quarter
                   , data = dem_no_na_1, family = binomial(link = 'logit'))

bglm_2 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + 
                       bis_housing_change +
                       bis_credit_change +
                       inflation +
                       #gini_market + 
                       gini_net +
                       redist_relative +
                       executive_election_4qt +
                       executive_election_4qt_after +
                       cb_policy_rate + cb_policy_rate_change +
                       cbi + 
                       #polconv + 
                       uds_mean + 
                       country + 
                       year # + quarter
                   , data = dem_no_na_1, family = binomial(link = 'logit'))

texreg(list(bglm_1, bglm_2),
          omit.coef = 'country',
          custom.model.names = c('Tightening MPR', 'Loosening MPR'),
          table = FALSE,
          file = 'papers/tables/garbage_can_bayes_logit.tex'
          )
