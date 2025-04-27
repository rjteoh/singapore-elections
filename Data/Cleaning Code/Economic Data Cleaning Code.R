# Script for cleaning raw economic data

#----Setup----

# loading libraries
library(tidyverse)
library(slider)
library(stargazer)
library(here)

# clearing variables
rm(list = ls())

#----Calculating Annual GDP Growth----

# loading data
gdp_annual <- read.csv(here("Data", "Raw Data", "gdp_annual_raw.csv"))

gdp_annual <- gdp_annual %>%
  # dropping unneeded columns
  select(Data.Series, GDP.In.Chained..2015..Dollars..Million.Dollars.) %>%
  # renaming columns to something more descriptive
  rename(
    Year = Data.Series,
    GDP = GDP.In.Chained..2015..Dollars..Million.Dollars.
  ) %>%
  # calculating annual GDP growth
  mutate(
    # lead() functions tells R to look at the next row in the dataframe
    GDP.1yr = GDP / lead(GDP, n = 1) * 100 - 100
  ) %>%
  # adding additional variables that might be interesting
  mutate(
    # adding a boolean indicator to determine if it is a recession year or not
    Recession = if_else(GDP.1yr < 0, TRUE, FALSE), 
    # adding a variable showing the average growth across three years
    GDP.4yr = slide_mean(GDP.1yr, before = 0, after = 3, complete = TRUE),
    # adding a lag variable
    GDP.lag = lead(GDP.1yr, 1)
  )
  
# exporting cleaned data
write.csv(gdp_annual, file = here("Data", "gdp_annual.csv"), row.names = FALSE)

#--- Calculating Residential Unemployment----

# Data on unemployment was manually transcribed from Singstat and MOM data.
# We do not have data on resident unemployment before 1991 and need to estimate it.

# loading data
unemp_annual <- read.csv(here("Data", "unemployment_annual.csv"))

# finding the relationship between resident and non-resident unemployment
unemp_mod <- lm(Res.Unemp.1yr ~ Total.Unemp.1yr, data = unemp_annual) 

# the relationship is statistically significant at the p = 0 level
summary(unemp_mod)

# saving the estimator as a number
estimator <- coef(unemp_mod)[2] %>% as.numeric()

# adding total unemployment numbers for 1972 (from Lee 1973)
unemp_annual <- unemp_annual %>% 
  mutate(
    Total.Unemp.1yr = if_else(
      Year == 1972, 4.8, Total.Unemp.1yr
      )
  )

# using the estimator to fill in missing resident unemployment numbers
unemp_annual <- unemp_annual %>%
  mutate(
    Res.Unemp.1yr = if_else(is.na(Res.Unemp.1yr), 
                            Total.Unemp.1yr * estimator, Res.Unemp.1yr),
    Res.Unemp.4yr = slide_mean(Res.Unemp.1yr, before = 0, after = 3, complete = TRUE)
  )

# exporting regression in stargazer
stargazer(unemp_mod, type = 'html', 
          # labeling dependent variables
          dep.var.labels = c("Resident Unemployment (%)"),
          # labeling covariates 
          covariate.labels = "Total Unemployment (%)",
          digits = 3, # setting to 3 s.f.
          omit.stat = c("ser", "f"),  # omitting standard error and f stat
          align = TRUE, # aligning table nicely
          out = here('unemp.html')) # saving as file

# exporting cleaned data
write.csv(unemp_annual, file = here("Data", "unemployment_annual_estimated.csv"), row.names = FALSE)
