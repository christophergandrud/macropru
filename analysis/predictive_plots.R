# ---------------------------------------------------------------------------- #
# Predictive plots
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(repmis)
library(dplyr)
library(ggplot2)
library(simGLM)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Estimate models -------
source('analysis/regressions_v2.R')

# Simulate and plot tightening --------
fitted_cbi_tighten <- with(dem, 
                           expand.grid(
                               cbi = unique(cbi)[!is.na(unique(cbi))],
                               lag_cumsum_any_tighten = mean(lag_cumsum_any_tighten,
                                                             na.rm = TRUE),
                               gdp_growth = mean(gdp_growth, na.rm = TRUE),
                               inflation = mean(inflation, na.rm = TRUE)
                           )
)


sim_glm(t3, newdata = fitted_cbi_tighten, x_coef = 'cbi', model = 'logit') +
        xlab('\nCentral Bank Independence') + 
        ylab('Probability of Tightening MPR\n')
