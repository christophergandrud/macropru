# ---------------------------------------------------------------------------- #
# Combine data sets
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(repmis)
library(dplyr)
library(lubridate)
library(countrycode)
library(DataCombine)
library(tidyr)
library(stringr)
library(psData)
library(WDI)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load macro-pru data -------
boe <- import("data/raw/Data_ReinhardtSowerbutts 2015 BoE WP 546.dta")
boe$country <- countrycode(boe$countrycode, origin = "iso3c", 
                           destination = "country.name")

# Quarter variable
boe$year_quarter <- sprintf("%s.%s", boe$year, boe$quarter) %>% 
    as.numeric

boe <- MoveFront(boe, c("country", "countrycode", "year_quarter", "quarter"))

# Macropru governance
## p. 16 https://www.imf.org/external/pubs/ft/wp/2013/wp13166.pdf
source('data/raw/macropru_governance_indices.R')

macro_gov$country <- countrycode(macro_gov$country, origin = 'country.name',
                                 destination = 'country.name')

# Election timing data --------
## From http://hyde.research.yale.edu/nelda
elections <- import("data/raw/NELDA.xls", col_names = F)

elections <- elections[, c(3, 5:7)]

# Create election quarters
elections$X6 <- as.character(elections$X6)
elections$X6 <- sprintf("0%s", elections$X6)

for (i in 1:nrow(elections)) {
    if (nchar(elections[i, "X6"]) == 5) {
        elections[i, "X6"] <- substr(elections[i, "X6"], 2, 5)
    }
}

elections$election_date <- sprintf("%s%s", elections$X5, elections$X6)
elections$election_date <- ymd(elections$election_date)
elections$year_quarter <- quarter(elections$election_date, with_year = TRUE)

#  Other cleaning
elections <- elections %>% filter(X5 >= 2000)
elections$X3[elections$X3 == "Democratic Republic of Vietnam"] <- "Vietnam"
elections$country <- countrycode(elections$X3, origin = "country.name", 
                                 destination = "country.name")

elections <- elections %>% select(country, year_quarter, election_date, X7)
elections$value <- 1

elections <- FindDups(elections, Vars = c("country", "election_date", 
                                          "X7"), NotDups = T)

elections <- elections %>% spread(X7, value) %>% arrange(country, election_date)

exec_election <- elections %>% select(country, year_quarter, Executive)
exec_election <- exec_election %>% DropNA('Executive')
exec_election <- exec_election %>% FindDups(c('country', 'year_quarter'), 
                                            NotDups = T)


# Inequality ---------
load('data/raw/swiidV4_0.RData') 

swiid$country <- countrycode(swiid$country, origin = 'country.name', 
                               destination = 'country.name')

swiid <- swiid[, c('country', 'year', '_1_gini_net', '_1_gini_market', 
                   '_1_redist', '_1_share1')]

# FinStress -----
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- import(URL)

# Quarter means
finstress_index$year_quarter <- quarter(finstress_index$date, with_year = T)
finstress_index <- finstress_index %>% group_by(country, year_quarter) %>%
    mutate(finstress_qt_mean = mean(FinStress))

finstress_index <- FindDups(finstress_index, c('country', 'year_quarter'), 
                            NotDups = T)

finstress_index <- finstress_index %>% select(country, year_quarter, 
                                            finstress_qt_mean) %>% as.data.frame

# Polity ---------
polity <- PolityGet(vars = c("polity2"))
polity$country <- countrycode(polity$country, origin = 'country.name',
                              destination = 'country.name')
polity <- polity %>% select(-iso2c, -standardized_country)

# DPI ---------
dpi <- DpiGet(vars = c("execrlc"))
dpi$country <- countrycode(dpi$country, origin = 'country.name',
                              destination = 'country.name')
dpi <- dpi %>% select(-iso2c)

# Clean up Missing
dpi$execrlc[dpi$execrlc == -999] <- NA
dpi$execrlc[dpi$execrlc == 0] <- NA

# Wang et al. GFS Fiscal transparency -----------
# Downloaded from https://www.imf.org/External/pubs/cat/longres.aspx?sk=43177.0
fiscal_trans <- import('data/raw/wp15188.xlsx', sheet = "GFS Index Score", 
                       skip = 2)
fiscal_trans$country <- countrycode(fiscal_trans$Country, origin = 'country.name',
                                    destination = 'country.name')

fiscal_trans <- fiscal_trans[, c(17, 4:14)]

fiscal_trans <- fiscal_trans %>% gather(year, fiscal_trans_gfs, 
                                        2:ncol(fiscal_trans)) %>%
                    arrange(country, year)

# Bodea and Hicks CBI -------------
# Downloaded from http://www.princeton.edu/~rhicks/data.html
cbi <- foreign::read.dta('data/raw/cb_rh_iodata.dta')
cbi$country <- countrycode(cbi$countryname, origin = 'country.name',
                                    destination = 'country.name')

## No EU
cbi <- cbi %>% DropNA('country')

cbi <- cbi %>% select(country, year, lvau, lvaw)
cbi <- cbi %>% rename(cbi = lvau) %>% rename(cbi_weighted = lvaw)

# WDI -------------
wdi <- WDI(indicator = c('NY.GDP.MKTP.KD.ZG', 'FP.CPI.TOTL.ZG', 'SI.POV.GINI',
                         'FS.AST.DOMS.GD.ZS'), 
           start = 1990, end = 2015)
wdi <- wdi %>% rename(gdp_growth = NY.GDP.MKTP.KD.ZG) %>% 
    rename(inflation = FP.CPI.TOTL.ZG) %>%
    rename(gini = SI.POV.GINI) %>% 
    rename(domestic_credit = FS.AST.DOMS.GD.ZS)

wdi$country <- countrycode(wdi$iso2c, origin = 'iso2c',
                           destination = 'country.name')
wdi <- wdi %>% DropNA('country')

wdi <- wdi %>% select(-iso2c)
wdi <- change(wdi, Var = 'domestic_credit', GroupVar = 'country', 
                  NewVar = 'domestic_credit_change')

# BIS Housing price change -----------
## Downloaded from http://www.bis.org/statistics/pp_selected.htm
bis <- import('data/raw/full_BIS_SELECTED_PP_csv.csv')

# Keep year-on-year real quarterly housing price change (%)
bis <- bis %>% grepl.sub(pattern = 'R:Real', Var = 'Value') %>%
        grepl.sub(pattern = '771:Year-on-year changes, in per cent', 
                  Var = 'Unit of measure') %>%
        select(-Frequency, -Value, -`Unit of measure`, -`Time Period`)

bis_col <- str_split_fixed(bis$`Reference area`, n = 2, pattern = ':') %>%
            as.data.frame %>% setNames(c('iso2c', 'country'))
bis <- cbind(bis_col, bis) %>% select(-iso2c, -`Reference area`)
bis$country <- countrycode(bis$country, origin = 'country.name',
                           destination = 'country.name')
bis <- bis %>% DropNA('country')

bis <- bis %>% gather(year_quarter, bis_housing_change, 2:ncol(bis))
bis$year_quarter <- gsub('-Q', '.', bis$year_quarter) %>% as.numeric
bis$bis_housing_change <- bis$bis_housing_change %>% as.numeric
bis <- bis %>% arrange(country, year_quarter)
bis <- bis %>% DropNA('bis_housing_change')


# Combine ------
comb <- merge(exec_election, boe, by = c("country", "year_quarter"), 
              all.y = T)
comb <- merge(comb, macro_gov, by = 'country', all.x = T)
comb <- merge(comb, swiid, by = c('country', 'year'), all.x = T)
comb <- merge(comb, polity, by = c('country', 'year'), all.x = T)
comb <- merge(comb, finstress_index, by = c('country', 'year_quarter'), 
              all.x = T)
comb <- merge(comb, dpi, by = c('country', 'year'), all.x = T)
comb <- merge(comb, fiscal_trans, by = c('country', 'year'), all.x = T)
comb <- merge(comb, cbi, by = c('country', 'year'), all.x = T)
comb <- dMerge(comb, bis, by = c('country', 'year_quarter'), all = T)
comb <- dMerge(comb, wdi, by = c('country', 'year'), all.x = T)
comb <- comb %>% arrange(country, year_quarter)
FindDups(comb, c('country', 'year_quarter'))

# Clean up --------------
# Capital cumulative sum
comb <- comb %>% arrange(country, year_quarter) %>% group_by(country) %>%
    mutate(cumsum_capital = cumsum(Capital))

# Finish creating election dummy
for (i in 5:7) {
    comb[, i][is.na(comb[, i])] <- 0
}

# Any election
comb$any_election <- 0
comb$any_election[comb$Executive == 1] <- 1
comb$`Legislative/Parliamentary`[comb$`Legislative/Parliamentary` == 1] <- 1

# Create 4 qtrs from any election dummy
comb <- SpreadDummy(comb, Var = 'any_election', GroupVar = 'country',
                    NewVar = 'any_election_4qt', spreadBy = 3)

# Create 4 qtrs from executive election dummy
comb <- SpreadDummy(comb, Var = 'Executive', GroupVar = 'country',
                    NewVar = 'executive_election_4qt', spreadBy = 3)

# Any tightening
tighten <- names(comb)[grep("*_Tighten", names(comb))]

comb$any_tighten <- 0
for (i in tighten) {
    comb$any_tighten[comb[, i] == 1] <- 1
}

# Any loosening
loosen <- names(comb)[grep("*_Loosen", names(comb))]

comb$any_loosen <- 0
for (i in loosen) {
    comb$any_loosen[comb[, i] == 1] <- 1
}

# Cumulative tightening
comb <- comb %>% arrange(country, year_quarter) %>% group_by(country) %>%
    mutate(cumsum_any_tighten = cumsum(any_tighten))

comb <- slide(comb, Var = 'cumsum_any_tighten', GroupVar = 'country', 
              TimeVar = 'year_quarter', NewVar = 'lag_cumsum_any_tighten')

comb <- comb %>% MoveFront(c("country", "countrycode", "year",
                             "year_quarter", "quarter", "election_date", 'polity2', 
                             'finstress_qt_mean'))

comb <- comb %>% select(-standardized_country, -countryname)

# Export -----------
export(comb, file = 'data/main_combined.csv')
