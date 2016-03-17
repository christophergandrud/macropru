# ---------------------------------------------------------------------------- #
# Predictive plots
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(repmis)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(simGLM)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Estimate models -------
source('analysis/regressions_v2_democracies.R')

# Simulate and plot for range of fitted CBI and growth values --------
fitted_cbi_tighten <- with(dem, 
                           expand.grid(
                               cbi = unique(cbi)[!is.na(unique(cbi))],
                               lag_cumsum_any_tighten = mean(lag_cumsum_any_tighten,
                                                             na.rm = TRUE),
                               gdp_growth = quantile(gdp_growth, probs = 0.9, na.rm = T),
                               inflation = mean(inflation, na.rm = TRUE)
                           )
)

fitted_cbi_loosen <- with(dem, 
                           expand.grid(
                               cbi = unique(cbi)[!is.na(unique(cbi))],
                               lag_cumsum_any_tighten = mean(lag_cumsum_any_tighten,
                                                             na.rm = TRUE),
                               gdp_growth = quantile(gdp_growth, probs = 0.1, na.rm = T),
                               inflation = mean(inflation, na.rm = TRUE)
                           )
)

cbi_tighten <- sim_glm(t3, newdata = fitted_cbi_tighten, x_coef = 'cbi', model = 'logit') +
                        xlab('') + 
                        ylab('Probability of Tightening MPR\n')

cbi_loosen <- sim_glm(l3, newdata = fitted_cbi_loosen, x_coef = 'cbi', model = 'logit') +
                        xlab('') + 
                        ylab('Probability of Loosening MPR\n')

pdf(file = 'figures/cbi_predictions.pdf', width = 11)
    grid.arrange(cbi_tighten, cbi_loosen, nrow = 1, 
             bottom = 'Central Bank Independence')
dev.off()
