# ---------------------------------------------------------------------------- #
# Descriptive plots
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(repmis)
library(dplyr)
library(tidyr)
library(DataCombine)
library(ggplot2)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load combined data set
main <- import('data/main_combined.csv')

FindDups(main, c('country', 'year_quarter'))

# Find cumulative loosening
main <- main %>% arrange(country, year_quarter) %>% group_by(country) %>%
            mutate(cumsum_any_loosen = cumsum(any_loosen))

main$country[main$country == 'Bolivia, Plurinational State of'] <- 'Bolivia'
main$country[main$country == 'Taiwan, Province of China'] <- 'Taiwan'
main$country[main$country == 'Russian Federation'] <- 'Russia'

# Only democracies ------
dem <- main %>% filter(polity2 > 5)

dem_cumsum <- dem %>% dplyr::select(country, year, cumsum_any_tighten, 
                             cumsum_any_loosen)

dem_cumsum <- dem_cumsum %>% gather(policy_type, cum_sum, 3:4)

dem_cumsum <- FindDups(dem_cumsum, c('country', 'year', 'policy_type'), 
                       NotDups = T)

dem_cumsum$policy_type[dem_cumsum$policy_type == 'cumsum_any_loosen'] <- 'Loosen'
dem_cumsum$policy_type[dem_cumsum$policy_type == 'cumsum_any_tighten'] <- 'Tighten'
    
x = factor(x,levels(x)[c(4,5,1:3)])

dem_cumsum$policy_type <- factor(dem_cumsum$policy_type, 
                                 levels(factor(dem_cumsum$policy_type))[c(2, 1)])

# Plot cumulative tightening by democratic country
ggplot(dem_cumsum, aes(year, cum_sum, colour = policy_type,
                       linetype = policy_type)) +
    geom_line() +
    facet_wrap(~ country) +
    scale_colour_manual(values = c("#F98400", "#5BBCD6"), name = '') +
    scale_linetype(name = '') +
    scale_x_continuous(breaks = c(2000, 2005, 2010)) +
    xlab('') + ylab('Cumulative Sum (from 2000)\n') +
    theme_bw()
