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
main$gini_diff_market_net <- main$gini_market - main$gini_net
main$gini_relative <- (main$gini_diff_market_net / main$gini_market) * 100
main$gini_diff_red <- main$gini_market - main$redist

# Find variable correlations
keepers <- c('any_tighten', 'lag_cumsum_any_tighten',
             'gdp_growth', 'gdp_per_capita', 'inflation', 
             'finstress_qt_mean',
             'bis_housing_change', 'cbi', 
             'executive_election_4qt',
             'gini_diff_market_net', 'uds_mean')

keeper_labels <- c('Any MPR Tightening', 'Cum. Tight. (lag)', 
                   'GDP Growth', 'GDP/Capita', 'Inflation', 'FinStress',
                   'Housing Chng', 'CBI', 'Election Period',
                   'Gini Diff.', 'UDS')

subbed <- main[, keepers[-1]]
names(subbed) <- keeper_labels[-1]
iv_correlations <- cor(subbed, use = 'complete.obs')
print(xtable(iv_correlations), caption.placement = 'top',
      caption = 'Right-hand Variable Correlations',
      size = 'tiny',
      file = 'papers/tables/iv_correlations.tex')

# Set as factors
main$country <- as.factor(main$country)
main$year <- as.factor(main$year)
main$quarter <- as.factor(main$quarter)
main$executive_election_4qt <- as.factor(main$executive_election_4qt)

# Rescale DV to get easily interpretable estimates 
main$any_tighten[main$any_tighten == 0] <- 4
main$any_tighten[main$any_tighten == 1] <- 3

main$any_loosen[main$any_loosen == 0] <- 4
main$any_loosen[main$any_loosen == 1] <- 3

main$any_tighten <- as.factor(main$any_tighten)
main$any_loosen <- as.factor(main$any_loosen)

FindDups(main, c('country', 'year_quarter'))

# Only democracies ------
#dem <- main %>% filter(polity2 > 5)

dem = main

# Keep complete cases
dem_no_na_1 <- dem %>% DropNA(keepers)

# RF for Tightening MPR -------------------------------------------------------
rt1 <- rfsrc(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + bis_housing_change +
                 inflation + gini_diff_market_net + executive_election_4qt +
                 finstress_qt_mean + cbi + polconv + uds_mean + gdp_per_capita +
                 country
             , data = dem_no_na_1, proximity = TRUE)

# Plot OOB errors against the growth of the forest
plot(gg_error(rt1))

# Minimum variable depth for tightening -------------
tighten_md_labels <- c('GDP Growth', 'Country', 'Housing Price Chng', 
                       'Cum. Tight. (lag)', 'FinStress', 'Democracy',
                       'GDP/Capita', 'Gini Diff.', 'Inflation', 'CBI',
                       'Political Constraints', 'Election')

gg_md_tighten <- gg_minimal_depth(rt1)
tighten_md <- plot(gg_md_tighten) +
                scale_x_discrete(labels = rev(tighten_md_labels)) +
                #ggtitle('Minimal Variable Depth for Predicting Any Tightening\n') +
                theme_bw()

ggsave(tighten_md, filename = 'papers/figures/tighten_md.pdf', height = 5.82, 
       width = 9.25)

# Order variables by minimal depth rank (exclude country)
xvar_tighten <- gg_md_tighten$topvars[!(gg_md_tighten$topvars %in% 'country')]

# Variable dependence tighten
gg_v_tighten <- gg_variable(rt1)

plot(gg_v_tighten, xvar = xvar_tighten, panel = TRUE)

# Partial dependence for tightening -----------
partial_bis_tighten <- plot.variable(rt1, xvar = xvar_tighten, partial = TRUE,
                             show.plots = FALSE)

partial_tighten <- plot(gg_partial(partial_bis_tighten), panel = TRUE, alpha = 0.5) +
                    stat_smooth() + xlab('') +
                    ylab('Predicted Probability of MPR Tightening\n') +
                    xlab('\nPredictor Scale') +
                    theme_bw()

ggsave(partial_tighten, filename = 'papers/figures/patial_tighten.pdf', 
       width = 10, height = 8)

# Interactions for tightening -----------
interation_tighten <- find.interaction(rt1)

plot(gg_interaction(interation_tighten), panel = TRUE)


# RF for Loosening -------------------------------------------------------------
rl1 <- rfsrc(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + bis_housing_change +
                 inflation + gini_diff_market_net + executive_election_4qt +
                 finstress_qt_mean + cbi + polconv + polity2 +
                 country
             , data = dem_no_na_1)

# Minimum variable depth for loosening -------------
gg_md_loosen <- gg_minimal_depth(rl1)
plot(gg_md_loosen) +
    ggtitle('Minimal Variable Depth for Predicting Any Loosening\n') +
    theme_bw()

# Order variables by minimal depth rank (exclude country)
xvar_loosen <- gg_md_loosen$topvars[!(gg_md_loosen$topvars %in% 'country')]

# Partial dependence for tightening -----------
partial_bis_loosen <- plot.variable(rl1, xvar = xvar_loosen, partial = TRUE,
                                     show.plots = FALSE)

plot(gg_partial(partial_bis_loosen), panel = TRUE, alpha = 0.5) +
    stat_smooth() + xlab('') +
    ylab('Predicted Probability\n') +
    xlab('Predictor Scale\n') +
    ggtitle('Partial Dependence Panels for MPR Loosening\n') +
    theme_bw()

# Interactions for loosening -----------
interaction_loosen <- find.interaction(rl1)

plot(gg_interaction(interaction_loosen), panel = TRUE)
