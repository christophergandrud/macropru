# ---------------------------------------------------------------------------- #
# Preliminary Analysis (Random Forests)
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(repmis)
library(dplyr)
library(xtable)
library(DataCombine)
library(randomForestSRC)
library(ggRandomForests)
library(ggplot2)

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

# Table of country quarter sample used in the models -----------
the_sample <- dem_no_na_1 %>% group_by(country) %>%
                summarise(`First Year` = min(year),
                          `Last Year` = max(year)) %>%
                rename(Country = country)

print(xtable(the_sample, 
             caption = 'Country Quarter-Year Sample Included in the Random Forests After Deleting Cases with Missing Values'),
      caption.placement = 'top',
      include.rownames = FALSE,
      file = 'papers/tables/rf_sample.tex')

# Sample size
Tighten <- summary(dem_no_na_1$any_tighten)[[1]]
Loosen <- summary(dem_no_na_1$any_loosen)[[1]]
Total <- nrow(dem_no_na_1)

print(xtable(data.frame(Tighten, Loosen, Total),
             caption = 'Number of Events and Total Observations for the Estimation Sample'),
             caption.placement = 'top',
             include.rownames = FALSE,
             file = 'papers/tables/rf_sample_size.tex'
      )


# RF for Tightening MPR -------------------------------------------------------
rt1 <- rfsrc(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + 
                 bis_housing_change +
                 bis_credit_change +
                 #inflation +
                 #gini_market + 
                 gini_net +
                 redist_relative +
                 executive_election_4qt +
                 cb_policy_rate + cb_policy_rate_change +
                 cbi + 
                 #polconv + 
                 uds_mean + 
                 country + 
                 year # + quarter
             , data = dem_no_na_1, importance = TRUE)

# Plot OOB errors against the growth of the forest
plot(gg_error(rt1))

# Minimum variable depth for tightening -------------
tighten_md_labels <- c('GDP Growth', 'Country', 'Housing Price Chng', 
                       'Cum. Tight. (lag)', 'FinStress', 'Democracy',
                       'GDP/Capita', 'Gini Diff.', 'Inflation', 'CBI',
                       'Political Constraints', 'Election')

# Minimum depth
gg_md_tighten <- gg_minimal_depth(rt1)
tighten_md <- plot(gg_md_tighten) +
              #  scale_x_discrete(labels = rev(tighten_md_labels)) +
                theme_bw()

tighten_md_vimp <- plot(gg_minimal_vimp(gg_md_tighten)) + theme_bw()

tighten_imp <- plot(gg_vimp(rt1)) + 
                scale_y_continuous(breaks = c(0, 0.01, 0.02)) +
                theme_bw()


ggsave(tighten_md, filename = 'papers/figures/tighten_md.pdf', 
       height = 5.82, width = 9.25)
ggsave(tighten_imp, filename = 'papers/figures/tighten_imp.pdf', 
       height = 5.82, width = 9.25)

# Order variables by minimal depth rank (exclude country)
#xvar_tighten <- gg_md_tighten$topvars[!(gg_md_tighten$topvars %in% 
#                                            c('country', 'quarter'))]

# Partial dependence for tightening -----------
# Almost all variables
xvar_tighten <- gg_md_tighten$varselect$names %>% as.character
xvar_tighten <- xvar_tighten[!(xvar_tighten %in% c('country', 'quarter',
                                                   'executive_election_4qt',
                                                   'ex_regime'))]

partial_bis_tighten <- plot.variable(rt1, xvar = xvar_tighten, partial = TRUE,
                             show.plots = FALSE)

partial_tighten <- plot(gg_partial(partial_bis_tighten), panel = TRUE, 
                        alpha = 0.5) +
                    geom_line() + xlab('') +
                    scale_y_continuous(limits = c(0, 0.3)) +
                    ylab('Predicted Probability of MPR Tightening\n') +
                    xlab('\nPredictor Scale') +
                    theme_bw()

ggsave(partial_tighten, filename = 'papers/figures/patial_tighten.pdf', 
       width = 10, height = 8)

# Interactions for tightening -----------
interation_tighten <- find.interaction(rt1)

plot(gg_interaction(interation_tighten), panel = TRUE)


# RF for Loosening -------------------------------------------------------------
rl1 <- rfsrc(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + 
                 bis_housing_change +
                 bis_credit_change +
                 #inflation +
                 #gini_market + 
                 gini_net +
                 redist_relative +
                 executive_election_4qt +
                 cb_policy_rate + cb_policy_rate_change +
                 cbi + 
                 #polconv + 
                 uds_mean + 
                 country + 
                 year # + quarter
             , data = dem_no_na_1, importance = TRUE)

# Minimum variable depth for loosening -------------
gg_md_loosen <- gg_minimal_depth(rl1)
loosen_md <- plot(gg_md_loosen) +
    #  scale_x_discrete(labels = rev(loosen_md_labels)) +
    theme_bw()

loosen_md_vimp <- plot(gg_minimal_vimp(gg_md_loosen)) + theme_bw()

loosen_imp <- plot(gg_vimp(rl1)) + 
    scale_y_continuous(breaks = c(0, 0.01, 0.02)) +
    theme_bw()


ggsave(loosen_md, filename = 'papers/figures/loosen_md.pdf', 
       height = 5.82, width = 9.25)
ggsave(loosen_imp, filename = 'papers/figures/loosen_imp.pdf', 
       height = 5.82, width = 9.25)

# Order variables by minimal depth rank (exclude country)
#xvar_loosen <- gg_md_loosen$topvars[!(gg_md_loosen$topvars %in% 
#                                            c('country', 'quarter'))]

# Partial dependence for loosening -----------
# Almost all variables
xvar_loosen <- gg_md_loosen$varselect$names %>% as.character
xvar_loosen <- xvar_loosen[!(xvar_loosen %in% c('country', 'quarter',
                                                   'executive_election_4qt',
                                                   'ex_regime'))]

partial_bis_loosen <- plot.variable(rl1, xvar = xvar_loosen, partial = TRUE,
                                     show.plots = FALSE)

partial_loosen <- plot(gg_partial(partial_bis_loosen), panel = TRUE, 
                        alpha = 0.5) +
    geom_line() + xlab('') +
    scale_y_continuous(limits = c(0, 0.3)) +
    ylab('Predicted Probability of MPR Loosening\n') +
    xlab('\nPredictor Scale') +
    theme_bw()

ggsave(partial_loosen, filename = 'papers/figures/patial_loosen.pdf', 
       width = 10, height = 8)

# Interactions for loosening -----------
interaction_loosen <- find.interaction(rl1)

plot(gg_interaction(interaction_loosen), panel = TRUE)
