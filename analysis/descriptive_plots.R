# ---------------------------------------------------------------------------- #
# Descriptive plots
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(repmis)
library(rio)
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
main$country[main$country == 'United Arab Emirates'] <- 'UAE'
main$country[main$country == 'Macedonia, the former Yugoslav Republic of'] <- 'Macedonia'

# Only democracies ------
#dem <- main %>% filter(polity2 > 5)

dem <- main

dem_cumsum <- dem %>% dplyr::select(country, year, cumsum_any_tighten, 
                             cumsum_any_loosen)

dem_cumsum <- dem_cumsum %>% gather(policy_type, cum_sum, 3:4)

dem_cumsum <- FindDups(dem_cumsum, c('country', 'year', 'policy_type'), 
                       NotDups = T)

dem_cumsum$policy_type[dem_cumsum$policy_type == 'cumsum_any_loosen'] <- 'Loosen'
dem_cumsum$policy_type[dem_cumsum$policy_type == 'cumsum_any_tighten'] <- 'Tighten'

dem_cumsum$policy_type <- factor(dem_cumsum$policy_type, 
                                 levels(factor(dem_cumsum$policy_type))[c(2, 1)])

# Plot cumulative tightening by democratic country
plot_mpr <- ggplot(dem_cumsum, aes(year, cum_sum, colour = policy_type,
                       linetype = policy_type)) +
    geom_line() +
    facet_wrap(~ country) +
    scale_colour_manual(values = c("#F98400", "#5BBCD6"), name = '') +
    scale_linetype(name = '') +
    scale_x_continuous(breaks = c(2000, 2005, 2010)) +
    scale_y_continuous(breaks = c(0, 10, 20)) +
    xlab('') + ylab('Cumulative Sum (from 2000)\n') +
    theme_bw()

ggsave(plot_mpr, file = 'figures/cumsum_mpr.pdf', width = 14, height = 8.7)

