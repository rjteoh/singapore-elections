# cleaning environment and loading libraries
library(tidyverse)
library(here)
rm(list = ls())

# creating an empty data frame to store results
results <- data.frame(
  Year = as.numeric(),
  Constituency = as.character(),
  Electorate = as.numeric(),
  PAP = as.numeric(),
  WP = as.numeric(),
  Others = as.numeric(),
  No.Vote = as.numeric()
)

# ---- creating the cleaning function ---- 
election_clean <- function(df, year) {
  
  # cleaning data frame
  df <- df %>%
    # adding the year column
    mutate(
      Year = year
    ) %>%
    # dropping unnecessary columns from the dataframe
    select(c("Year", "Constituency", "Electorate", "Party", "Votes")) %>%
    # removing empty rows
    filter(Party != '')
  
  # filling in blank rows with the correct constituency name and electorate size
  for (i in 1:nrow(df))(
    # if the constituency cell is blank
    if (df$Constituency[i] == ""){
      # copy the value in the cell above
      df$Constituency[i] = df$Constituency[i - 1]
      # and do the same for the electorate column
      df$Electorate[i] = df$Electorate[i - 1]
    } else{
      # else, do nothing
    }
  )
  
  # changing relevant values to numerics
  df <- df %>%
    # removing white spaces from row values
    mutate(
      Party = trimws(Party),
      Votes = trimws(Votes)
    ) %>%
    mutate(
      # simplfying party names
      Party = case_when(
        Party == "People's Action Party" ~ "PAP",
        Party == "Workers' Party" ~ "WP",
        TRUE ~ "Others"
      )
    ) %>%
    # removing commas and converting column values to numerics
    mutate(
      # rows with text denoting walkovers have NAs introduced by coercion
      Electorate = gsub(",","", Electorate) %>% as.numeric() %>% suppressWarnings(),
      Votes = gsub(",","",Votes) %>% as.numeric() %>% suppressWarnings()
    )
  
  # reshaping data for easier manipulation
  df <- df %>%
    pivot_wider(
      names_from = Party,
      values_from = Votes,
      # handling duplicate entries by summing them - i.e. when there are multiple 
      # third parties contesting
      values_fn = sum
    ) %>%
    mutate(
      # adding a column showing non-voters(including blank votes)
      No.Vote = case_when(
        # inserting NA values for uncontested seats
        PAP == NA ~ NA,
        # else subtracting the sum of all votes from total electorate
        # coalesce is used to replace NA values with `0` for the subtraction
        TRUE ~ Electorate - PAP - coalesce(WP,0) - coalesce(Others,0) 
      )
    ) 
  
  return(df)
}

# ---- iterating through raw data and combining into a single csv ----

# obtaining a list of all .csv files in the GE raw data folder
file_list <- list.files(here("Data", "Raw Data", "General Election"), pattern = "*.csv", full.names = TRUE)

for (file in file_list){
  # loading data
  df <- read.csv(file)
  
  # obtaining the length of the file name
  n <- nchar(file)
  
  # extracting the election year from the file name
  year <- file %>% str_sub(n - 7, n - 4) %>% unlist() %>% as.integer()
  
  # cleaning file
  df <- df %>% election_clean(year)
  
  # adding cleaned file to results frame
  results <- rbind(results, df)
}

# exporting cleaned data
write.csv(results, file = here("Data", "election_detailed_raw.csv"), row.names = FALSE)

# some additional manual cleaning of data was conducted after exporting - e.g.
# deleting trailing numerical characters in constituency names, standardizing
# spellings of constituency names. Cleaned data is saved as 
# election_detailed_clean.csv.