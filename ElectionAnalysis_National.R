
#----Setup----

# loading libraries
library(tidyverse)
library(here)
library(stargazer)
library(lubridate)
library(sandwich)
library(lmtest)
library(car)

# clearing variables
rm(list = ls())

# loading data
vote_data <- read.csv(here("Data", "election_detailed_clean.csv"))
by_data <- read.csv(here("Data", "byelection_detailed.csv"))
gdp_annual <- read.csv(here("Data", "gdp_annual.csv"))
gdp_quart <- read.csv(here("Data", "gdp_quarterly.csv"))
cpi_annual <- read.csv(here("Data", "cpi_annual.csv"))
cpi_quart <- read.csv(here("Data", "cpi_quarterly.csv"))
unemp_annual <- read.csv(here("Data", "unemployment_annual_estimated.csv"))
election_dates <- read.csv(here("Data", "election_dates.csv"))

# creating helper function to calculate heteroskedascity robust ses
# for printing in stargazer
robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC3")))
}


#----Adding by-election data to general election data---

# adding boolean to distinguish between by- and general elections
vote_data <- vote_data %>%
  mutate(
    Gen.Election = TRUE
  )

vote_data <- full_join(
  vote_data,
  by_data
)

#----Creating Combined National Vote----

# combining votes by election year
sg_vote <- vote_data %>%
  # dropping unneeded column
  select(!Constituency) %>%
  # dropping districts which were walkovers - i.e. where the PAP did not 
  # need to contest
  filter(!is.na(PAP)) %>%
  # grouping by individual elections
  group_by(Year) %>%
  # summing all the vote categories and total electorate size
  summarize(
    Electorate = sum(Electorate, na.rm = TRUE),
    PAP = sum(PAP, na.rm = TRUE),
    WP = sum(WP, na.rm = TRUE),
    Others = sum(Others, na.rm = TRUE),
    No.Vote = sum(No.Vote, na.rm = TRUE),
    # returns TRUE if all in the Gen.Election column are true, false otherwise
    Gen.Election = all(Gen.Election)
  )

# converting raw numbers into percentages
sg_vote <- sg_vote %>%
  mutate(
    # calculating turnout
    Turnout = (Electorate - No.Vote) / Electorate * 100,
    # calculating vote share as a percentage of total turnout
    across(c(PAP, WP, Others, No.Vote), ~. / (Electorate - No.Vote) * 100)
  ) %>%
  select(!Electorate) # dropping unneeded column

#----Adding PAP vs WP variable----

# building a data set for PAP vote share only in constituencies also contested 
# by WP (incl. some 3-cornered fights)
pap_wp <- vote_data %>%
  # selecting only relevant rows
  select(!Constituency) %>%
  filter(!is.na(WP)) %>%
  # calculating turnout for each constituency 
  mutate(
    Turnout = Electorate - No.Vote
  ) %>%
  # finding % of vote PAP won in contested constiuencies each election
  group_by(Year) %>%
  summarize(
    PAP.WP = sum(PAP, na.rm = TRUE) / sum(Turnout, na.rm = TRUE) * 100,
  ) 

# joining PAP vs WP data to national vote data
sg_vote <- left_join(
  sg_vote,
  pap_wp,
  by = 'Year'
)

# creating a new variable to show if PAP won >60% of the vote
sg_vote <- sg_vote %>%
  mutate(
    PAP.Win = if_else(PAP.WP < 60, 0, 1)
  )

#----Adding Dependent Variables----

# joining GDP data to national vote data
sg_vote <- left_join(
  sg_vote,
  gdp_annual,
  by = 'Year'
) %>%
  select(!c("GDP","GDP.Cap")) # dropping unneeded columns 

# joining inflation data to national vote data
sg_vote <- left_join(
  sg_vote,
  cpi_annual,
  by = 'Year'
) %>%
  select(!CPI) # dropping unneeded columns 

# joining unemployment data to national vote data
sg_vote <- left_join(
  sg_vote,
  unemp_annual,
  by = 'Year'
) %>%
  # dropping unneeded columns 
  select(!c('Labour.Force', 'Total.Emp', 'Total.Unemp')) 

# creating a variable to show whether unemployment was high that year
sg_vote <- sg_vote %>%
  mutate(
    High.Unemp = if_else(Total.Unemp.1yr > 3, TRUE, FALSE)
  )

# adding date to the dataset
sg_vote <- left_join(
  sg_vote,
  election_dates,
  by = 'Year'
)

# adding quarterly data to the dataset
sg_vote <- sg_vote %>%
  # calculating the quarter the vote took place in
  mutate(
    Qtr = quarter(Date, with_year = TRUE)
  ) %>%
  # joining data by quarter
  left_join(
    .,
    gdp_quart,
    by = 'Qtr'
  ) %>%
  left_join(
    .,
    cpi_quart,
    by = 'Qtr'
  ) %>%
  select(!c("GDP", "Qtr", "CPI")) # dropping unneeded columns

#----Adding synthetic values to try and improve model fit----

# calculating qtr GDP growth data for 1972, based on an assumption of even growth
# across all qtrs
yr_1972 <- sg_vote %>%
  filter(Year == 1972) %>%
  pull(GDP.1yr)
qtr_1972 <- ((1 + yr_1972/100) ^ (1/4) - 1) * 100

# editing 1972 values
sg_vote <- sg_vote %>%
  mutate(
    GDP.Qtr = if_else(
      Year == 1972, qtr_1972, GDP.Qtr
    ),
    GDP.Qtr.Lag = if_else(
      Year == 1972, qtr_1972, GDP.Qtr.Lag
    )
  )

#----Performing Regressions----

# filtering out by-elections
sg_vote_gen <- sg_vote %>%
  filter(Gen.Election == TRUE)

# regressing PAP vote share on GDP growth - no statistical significance
mod_PAPyr <- lm(PAP ~ GDP.1yr, data = sg_vote_gen)
mod_PAPqtr <- lm(PAP ~ GDP.Qtr.Lag, data = sg_vote_gen)
mod_PAPcap <- lm(PAP ~ GDP.1yr.Cap, data = sg_vote_gen)
mod_PAPunemp <- lm(PAP ~ Total.Unemp.1yr, data = sg_vote_gen)

# regressing PAP vote share vs WP on GDP growth per capita - significant
mod_cap <- lm(PAP.WP ~ GDP.1yr.Cap, data = sg_vote_gen)

# regressing PAP vote share vs WP on GDP growth (1 yr) - significant
mod_yr <- lm(PAP.WP ~ GDP.1yr, data = sg_vote_gen)

# regressing PAP vote share vs WP on GDP growth (lagged quarter) - significant
mod_qtr <- lm(PAP.WP ~ GDP.Qtr.Lag, data = sg_vote_gen)

# regressing PAP vote share vs WP on unemployment - not significant
mod_unemp <- lm(PAP.WP ~ Total.Unemp.1yr, data = sg_vote_gen)

# regressing PAP vote share vs WP on inflation - not significant
mod_inflate <- lm(PAP.WP ~ Inflation.1yr, data = sg_vote_gen)

# adding controls for the various regressions
mod_yr_ctrl <- lm(PAP.WP ~  GDP.1yr + Total.Unemp.1yr + Inflation.1yr, data = sg_vote_gen)
mod_cap_ctrl <- lm(PAP.WP ~  GDP.1yr.Cap + Total.Unemp.1yr + Inflation.1yr, data = sg_vote_gen)
mod_qtr_ctrl <- lm(PAP.WP ~  GDP.Qtr.Lag + Total.Unemp.1yr + Inflation.1yr, data = sg_vote_gen)
mod_yr_ctrl2 <- lm(PAP ~  GDP.1yr + Total.Unemp.1yr + Inflation.1yr, data = sg_vote_gen)
mod_cap_ctrl2 <- lm(PAP ~  GDP.1yr.Cap + Total.Unemp.1yr + Inflation.1yr, data = sg_vote_gen)
mod_qtr_ctrl2 <- lm(PAP ~  GDP.Qtr.Lag + Total.Unemp.1yr + Inflation.1yr, data = sg_vote_gen)


#----Exporting Data & Results----

# exporting data
write.csv(sg_vote, here("sg_vote.csv"), row.names = FALSE)

# exporting results of single variable regressions of GDP growth using robust ses
stargazer(mod_cap, mod_yr, mod_qtr, mod_PAPcap, mod_PAPyr, mod_PAPqtr, type = 'html', 
          # setting title
          title = "Estimating the effect of GDP growth on the PAP's vote share in general elections",
          # labeling dependent variables
          dep.var.labels = c("PAP vote share vs WP only", "PAP total vote share"),
          # labeling covariates 
          covariate.labels = c("Election-Year GDP Per Capita Growth (%)", "Election-Year GDP Growth (%)", 
                               "GDP Growth in Quarter Prior to Election (%)"),
          align = TRUE, # aligning table nicely
          out = here('genelection_monovariate.html')) # saving as file

# exporting results of single variable regressions of unemployment using robust ses
stargazer(mod_unemp, mod_PAPunemp, type = 'html', 
          # setting title
          title = "Estimating the effect of unemployment on the PAP's vote share in general elections",
          # labeling dependent variables
          dep.var.labels = c("PAP vote share vs WP only", "PAP total vote share"),
          # labeling covariates 
          covariate.labels = c("Election-Year Unemployment Rate (%)"),
          align = TRUE, # aligning table nicely
          out = here('genelection_unemp.html')) # saving as file


# exporting result of covariate regressions
stargazer(mod_cap_ctrl, mod_qtr_ctrl, mod_cap_ctrl2, mod_qtr_ctrl2, type = 'html', 
          # setting title
          title = "Estimating the effect of economic variables on the PAP's vote share in general elections",
          # labeling dependent variables
          dep.var.labels = c("PAP vote share vs WP only", "PAP total vote share"),
          # labeling covariates 
          covariate.labels = c("Election-Year GDP Per Capita Growth (%)", 
                               "GDP Growth in Quarter Prior to Election (%)", 
                               "Election-Year Unemployment Rate (%)", 
                               "Election-Year Inflation Rate (%)"),
          digits = 3, # setting to 3 s.f.
          align = TRUE, # aligning table nicely
            out = here('genelection_covariate.html')) # saving as file

#----Exporting Results with robust ses----

# exporting results of single variable regressions of GDP growth using robust ses
stargazer(mod_cap, mod_yr, mod_qtr, mod_PAPcap, mod_PAPyr, mod_PAPqtr, type = 'html', 
          # setting title
          title = "Estimating the effect of GDP growth on the PAP's vote share in general elections",
          # labeling dependent variables
          dep.var.labels = c("PAP vote share vs WP only", "PAP total vote share"),
          # labeling covariates 
          covariate.labels = c("Election-Year GDP Per Capita Growth (%)", "Election-Year GDP Growth (%)", 
                               "GDP Growth in Quarter Prior to Election (%)"),
          # ensuring that results use robust ses
          se = list(
            robust_se(mod_cap),
            robust_se(mod_yr),
            robust_se(mod_qtr),
            robust_se(mod_PAPcap),
            robust_se(mod_PAPyr),
            robust_se(mod_PAPqtr)
          ),
          align = TRUE, # aligning table nicely
          out = here('Robust Results','genelection_monovariateR.html')) # saving as file

# exporting results of single variable regressions of unemployment using robust ses
stargazer(mod_unemp, mod_PAPunemp, type = 'html', 
          # setting title
          title = "Estimating the effect of unemployment on the PAP's vote share in general elections",
          # labeling dependent variables
          dep.var.labels = c("PAP vote share vs WP only", "PAP total vote share"),
          # labeling covariates 
          covariate.labels = c("Election-Year Unemployment Rate (%)"),
          # ensuring that results use robust ses
          se = list(
            robust_se(mod_unemp),
            robust_se(mod_PAPunemp)
          ),
          align = TRUE, # aligning table nicely
          out = here('Robust Results', 'genelection_unempR.html')) # saving as file


# exporting result of covariate regressions
stargazer(mod_cap_ctrl, mod_qtr_ctrl, mod_cap_ctrl2, mod_qtr_ctrl2, type = 'html', 
          # setting title
          title = "Estimating the effect of economic variables on the PAP's vote share in general elections",
          # labeling dependent variables
          dep.var.labels = c("PAP vote share vs WP only", "PAP total vote share"),
          # labeling covariates 
          covariate.labels = c("Election-Year GDP Per Capita Growth (%)", 
                               "GDP Growth in Quarter Prior to Election (%)", 
                               "Election-Year Unemployment Rate (%)", 
                               "Election-Year Inflation Rate (%)"),
          digits = 3, # setting to 3 s.f.
          # ensuring that results use robust ses
          se = list(
            robust_se(mod_cap_ctrl),
            robust_se(mod_qtr_ctrl),
            robust_se(mod_cap_ctrl2),
            robust_se(mod_qtr_ctrl2)
          ),
          align = TRUE, # aligning table nicely
          out = here('Robust Results', 'genelection_covariateR.html')) # saving as file



#----Adding by-election results----

# checking if our model holds once we expand the dataset to include by-elections - it does, but only for the per capita data
mod_qtrby <- lm(PAP.WP ~  GDP.Qtr.Lag, data = sg_vote)
mod_unempby <- lm(PAP.WP ~  Total.Unemp.1yr, data = sg_vote)
mod_ctrlby <- lm(PAP.WP ~  GDP.Qtr.Lag + Total.Unemp.1yr + Inflation.1yr, data = sg_vote)
mod_qtrby2 <- lm(PAP ~  GDP.Qtr.Lag, data = sg_vote)
mod_unempby2 <- lm(PAP ~  Total.Unemp.1yr, data = sg_vote)
mod_ctrlby2 <- lm(PAP ~  GDP.Qtr.Lag + Total.Unemp.1yr + Inflation.1yr, data = sg_vote)

# instead the unemployment factor becomes more significant
mod_unemp <- lm(PAP.WP ~ Total.Unemp.1yr, data = sg_vote)
summary(mod_unemp)

# exporting result of covariate regressions including by-elections
stargazer(mod_ctrlby, mod_ctrlby2, type = 'html', 
          # setting title
          title = "Estimating the effect of GDP growth on the PAP's vote share in all elections (incl. by elections)",
          # labeling dependent variables
          dep.var.labels = c("PAP vote share vs WP only", "PAP total vote share"),
          # labeling covariates 
          covariate.labels = c("GDP Growth in Quarter Prior to Election (%)", "Election-Year Unemployment Rate (%)"),
          omit = c("Inflation.1yr"),
          omit.labels = c("Inflation Controls"),
          digits = 3, # setting to 3 s.f.
          align = TRUE, # aligning table nicely
          out = here('allelection.html')) # saving as file

#---- Outlier  test----

# creating a graph to show the 2020 outlier
outlier_graph <- sg_vote_gen %>%
  ggplot(
    aes(x = GDP.Qtr.Lag, y = PAP.WP)
  ) +
  geom_point(
    aes(color = ifelse(PAP.WP < 50 & Gen.Election == TRUE, "Outlier", "Reg"))
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Outlier" = "red", "Reg" = "black")) +
  labs(x = "Lagged Quarterly GDP Growth", y = "PAP vote share vs WP") +
  theme(legend.position = "none")

ggsave(here("outlier_graph.png"), outlier_graph, width = 6, height = 4)

# note that our model starts to break down if we remove the outlier year of 2020
# where economic growth was terrible - and yet the unemployment factor holds
sg_test <- sg_vote %>% filter(Year < 2020)
mod_test <- lm(PAP.WP ~ GDP.Qtr.Lag + Total.Unemp.1yr + Inflation.1yr, data = sg_test)
summary(mod_test)