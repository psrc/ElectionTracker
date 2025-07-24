library(magrittr)
library(data.table)
library(dplyr)
library(stringr)

source("scrape_scheduled_races.R")
source("scrape_candidate_lists.R")
source("scrape_election_results.R")
source("scrape_psrc_boards.R")

# County codes mapping
COUNTY_CODES <- list(
  "King" = 17,
  "Kitsap" = 18,
  "Pierce" = 27,
  "Snohomish" = 34
)

# Filtering terms
DISTRICT_TERMS <- c("county", "city", "town")
OFFICE_TERMS <- c("mayor", "council", "executive", "commissioner")

year <- format(Sys.Date(), "%Y")
election_date <- # string in YYYYMMDD format

# Helper function for standardized capitalization
custom_title_case <- function(text) {
  exceptions <- c("and", "of", "the")

  # Use map_chr to handle the vectorization
  map_chr(text, function(single_text) {
    words <- str_split(single_text, " ")[[1]]
    words <- ifelse(tolower(words) %in% exceptions,
                    tolower(words),
                    str_to_title(words))
    str_c(words, collapse = " ")
  })
}

# Generate all four tables
election_data <- list()
election_data$scheduled_races  <- scrape_scheduled_races(year)
election_data$candidate_lists  <- scrape_candidate_lists(NULL)
election_data$election_results <- scrape_election_results(election_date)
election_data$psrc_boards      <- scrape_psrc_boards()
election_data %<>% lapply(setDT)

reports <- list()

# Note: Assumes district names are already standardized across datasets
reports$board_members_not_running <- copy(election_data$psrc_boards) %>%

  # Join with scheduled_races to find board members who are incumbents
  .[election_data$scheduled_races, on=c("full_name"="incumbent", "district"), nomatch=NULL] %>%

  # Filter to only include those who are NOT running (no active candidate record)
  .[!election_data$candidate_lists[status == "Active"],
    on = c("district", "county", "office"="race", "full_name"="name")] %>%

  # Sort for consistent output
  setorder(county, district, office, full_name)

reports$board_members_won <- copy(election_data$psrc_boards) %>%

  # Join with scheduled_races to find board members who are incumbents
  .[election_data$scheduled_races, on=c("full_name"="incumbent", "district"), nomatch=NULL] %>%
  .[, race:=paste(district, office)] %>%

  # Filter to only include those who won
  .[election_data$election_results[outcome == "won"],
    on = c("county", "race"="race_name", "full_name"="candidate"), nomatch=NULL] %>%

  # Sort for consistent output
  setorder(county, district, office, full_name)

reports$board_members_lost <- copy(election_data$psrc_boards) %>%

  # Join with scheduled_races to find board members who are incumbents
  .[election_data$scheduled_races, on=c("full_name"="incumbent", "district"), nomatch=NULL] %>%
  .[, race:=paste(district, office)] %>%

  # Filter to only include those who lost
  .[election_data$election_results[outcome == "lost"],
    on = c("county", "race"="race_name", "full_name"="candidate"), nomatch=NULL] %>%

  # Sort for consistent output
  setorder(county, district, office, full_name)

reports$newly_electeds <- copy(election_data$scheduled_races) %>%
  .[, race:=paste(district, office)] %>%
  .[election_data$election_results[outcome == "won"],
    on = c("county", "race"="race_name"), nomatch=NULL] %>%
  .[candidate!=incumbent] %>% setnames("incumbent", "replaces")
