library(magrittr)
library(data.table)
library(dplyr)
library(stringr)
library(purrr)

# Parameters & helper functions for scraping functions

## County codes mapping
COUNTY_CODES <- list(
  "King" = 17,
  "Kitsap" = 18,
  "Pierce" = 27,
  "Snohomish" = 34
)

## Filtering terms
DISTRICT_TERMS <- c("county", "city", "town")
OFFICE_TERMS <- c("mayor", "council", "executive", "commissioner")

## Helper for consistent capitalization
custom_title_case <- function(text) {
  exceptions <- c("and", "of", "the")
  map_chr(text, function(single_text) {
    words <- str_split(single_text, " ")[[1]]
    words <- ifelse(tolower(words) %in% exceptions,
                    tolower(words),
                    str_to_title(words))
    str_c(words, collapse = " ")
  })
}

source("scrape_scheduled_races.R")
source("scrape_candidate_lists.R")
source("scrape_election_results.R")
source("scrape_psrc_boards.R")

# Core helper functions --------------------
load_election_data <- function(year = NULL, election_code = NULL, election_date = NULL) {
  year <- year %||% format(Sys.Date(), "%Y")

  list(
    scheduled_races = scrape_scheduled_races(year),
    candidate_lists = scrape_candidate_lists(election_code),
    election_results = if (!is.null(election_date)) scrape_election_results(election_date),
    psrc_boards = scrape_psrc_boards()
  ) %>% lapply(setDT)
}

join_board_with_races <- function(psrc_boards, scheduled_races) {
  psrc_boards %>%
    .[scheduled_races, on = c("full_name" = "incumbent", "district"), nomatch = NULL] %>%
    .[, race := paste(district, office)]
}

filter_by_outcome <- function(data, election_results, outcome, join_cols = c("county", "race" = "race_name", "full_name" = "candidate")) {
  data[election_results[outcome == ..outcome], on = join_cols, nomatch = NULL]
}

standardize_and_sort <- function(data, sort_cols = c("county", "district", "office", "full_name")) {
  # Only sort by columns that exist
  existing_cols <- intersect(sort_cols, names(data))
  if (length(existing_cols) > 0) {
    do.call(setorder, c(list(data), as.list(existing_cols)))
  }
  data
}

# Main report functions --------------------
make_candidate_filing_report <- function(election_code = NULL, year = NULL) {
  election_data <- load_election_data(year, election_code)

  list(
    candidate_list = election_data$candidate_lists[status == "Active"],

    board_members_running = join_board_with_races(election_data$psrc_boards, election_data$scheduled_races) %>%
      .[election_data$candidate_lists[status == "Active"],
        on = c("district", "county", "office" = "race", "full_name" = "name")] %>%
      .[!is.na(board_affiliation)] %>%
      standardize_and_sort(),

    board_members_not_running = join_board_with_races(election_data$psrc_boards, election_data$scheduled_races) %>%
      .[!election_data$candidate_lists[status == "Active"],
        on = c("district", "county", "full_name" = "name")] %>%
      standardize_and_sort()
  )
}

make_primary_election_report <- function(election_date, election_code = NULL, year = NULL) {
  election_data <- load_election_data(year, election_code, election_date)
  board_with_races <- join_board_with_races(election_data$psrc_boards, election_data$scheduled_races)

  list(
    advancing = filter_by_outcome(
      election_data$candidate_lists,
      election_data$election_results,
      "advanced",
      c("county", "race" = "race_name", "full_name" = "candidate")
    ),

    board_members_advanced = filter_by_outcome(board_with_races, election_data$election_results, "advanced") %>%
      standardize_and_sort(),

    board_members_lost = filter_by_outcome(board_with_races, election_data$election_results, "lost") %>%
      standardize_and_sort()
  )
}

make_general_election_report <- function(election_date, election_code = NULL, year = NULL) {
  election_data <- load_election_data(year, election_code, election_date)
  board_with_races <- join_board_with_races(election_data$psrc_boards, election_data$scheduled_races)
  races_with_race_col <- election_data$scheduled_races[, race := paste(district, office)]

  list(
    winners = filter_by_outcome(
      races_with_race_col,
      election_data$election_results,
      "won",
      c("county", "race" = "race_name")
    ) %>% standardize_and_sort(c("county", "district", "office", "candidate")),

    board_members_won = filter_by_outcome(board_with_races, election_data$election_results, "won") %>%
      standardize_and_sort(),

    board_members_lost = filter_by_outcome(board_with_races, election_data$election_results, "lost") %>%
      standardize_and_sort(),

    non_board_reelecteds = races_with_race_col %>%
      filter_by_outcome(election_data$election_results, "won", c("county", "race" = "race_name")) %>%
      .[candidate == incumbent] %>%
      .[!election_data$psrc_boards, on = c("county", "race_name" = "race", "candidate" = "full_name")] %>%
      standardize_and_sort(c("county", "district", "title", "candidate")),

    newly_electeds = races_with_race_col %>%
      filter_by_outcome(election_data$election_results, "won", c("county", "race" = "race_name")) %>%
      .[candidate != incumbent] %>%
      setnames("incumbent", "replaces") %>%
      standardize_and_sort(c("county", "district", "office", "candidate"))
  )
}
