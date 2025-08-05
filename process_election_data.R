library(magrittr)
library(data.table)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)

# Parameters & helper functions for scraping functions

## County codes mapping
COUNTY_CODES <- list(
  "King" = 17,
  "Kitsap" = 18,
  "Pierce" = 27,
  "Snohomish" = 34
)

## Filtering terms
OFFICE_TERMS        <- c("mayor", "council", "executive", "commission")
DISTRICT_TERMS      <- c("county", "city", "town", "port")
DISTRICT_EXCLUSIONS <- c("school","water","fire","sewer","parks","airport",
                         "charter review","ward","recreation")

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

source("scrape_psrc_boards.R")
source("scrape_scheduled_races.R")
source("scrape_candidate_lists.R")
source("generate_election_tracker_input.R")
source("scrape_election_results.R")

# Reporting helper functions --------------------------

join_races_w_boards_updated <- function(scheduled_races, election_tracker_input) {
  # Create lookup for PSRC name to ballot name
  psrc_lookup <- election_tracker_input[
    !is.na(ballot_name) & !not_seeking_reelection & !not_up_for_reelection,
    .(psrc_name, ballot_name, title, district, board_affiliation)
  ]

  # Join scheduled races with PSRC board info using ballot names
  result <- scheduled_races %>%
    .[psrc_lookup, on = c("incumbent" = "ballot_name"), allow.cartesian = TRUE] %>%
    .[, race := paste(i.district, office)] %>%
    .[, i.district := NULL] %>%
    .[mapply(grepl, district, race, fixed = TRUE)] %>%
    # Add psrc_name back for reference
    .[, full_name := i.psrc_name] %>%
    .[, i.psrc_name := NULL]

  return(result)
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
  unique(data)
}

# Primary Workflow --------------------
load_election_data <- function(year = NULL, election_code = NULL, election_date = NULL) {
  year <- year %||% format(Sys.Date(), "%Y")

  list(
    scheduled_races = scrape_scheduled_races(year),
    candidate_lists = scrape_candidate_lists(election_code),
    election_results = if (!is.null(election_date)) scrape_election_results(election_date),
    psrc_boards = scrape_psrc_boards()
  ) %>% lapply(setDT)
}

load_election_tracker_input <- function(input_file = "election_tracker_input.xlsx") {
  if (!file.exists(input_file)) {
    stop("Election tracker input file not found: ", input_file,
         "\nPlease run generate_election_tracker_input() first.")
  }

  election_tracker_input <- read.xlsx(input_file, sheet = 1) %>%
    as.data.table()

  message("Loaded election tracker input with ", nrow(election_tracker_input), " PSRC board members")
  return(election_tracker_input)
}

# Reporting functions ----------------------

make_candidate_filing_report <- function(election_code = NULL, year = NULL,
                                         election_tracker_input = NULL) {

  # Load election tracker input if not provided
  if (is.null(election_tracker_input)) {
    election_tracker_input <- load_election_tracker_input()
  }

  # Load other election data
  election_data <- load_election_data(year, election_code)

  # Get board members who are running (have ballot_name);
  # combine with candidate data
  board_members_running <- election_tracker_input[!is.na(ballot_name)] %>%
    .[election_data$candidate_lists[status == "Active"],
      on = c("ballot_name" = "name"),
      nomatch = NULL]

  # Add seeking_new_office flag by joining with scheduled_races
  board_members_running[election_data$scheduled_races,
                        seeking_new_office := incumbent != ballot_name,
                        on = c("district", "race" = "office")]

  board_members_running[, `:=`(
    not_seeking_reelection = NULL,
    not_up_for_reelection = NULL  # Drop duplicate county field
  )] %>%
    standardize_and_sort()

  list(
    candidate_list = election_data$candidate_lists[status == "Active"],

    board_members_running = board_members_running,

    not_seeking_reelection = election_tracker_input[
      not_seeking_reelection == TRUE
    ][, `:=`(
      not_seeking_reelection = NULL,
      not_up_for_reelection = NULL,
      seeking_new_office = NULL,
      ballot_name = NULL
    )] %>%
      standardize_and_sort(),

    not_up_for_reelection = election_tracker_input[
      not_up_for_reelection == TRUE
    ][, `:=`(
      not_seeking_reelection = NULL,
      not_up_for_reelection = NULL,
      seeking_new_office = NULL,
      ballot_name = NULL
    )] %>%
      standardize_and_sort()
  )
}

make_primary_election_report <- function(election_date, election_code = NULL, year = NULL,
                                         election_tracker_input = NULL) {

  # Load election tracker input if not provided
  if (is.null(election_tracker_input)) {
    election_tracker_input <- load_election_tracker_input()
  }

  election_data <- load_election_data(year, election_code, election_date)
  board_with_races <- join_races_w_boards_updated(election_data$scheduled_races, election_tracker_input)

  list(
    advancing = filter_by_outcome(
      election_data$candidate_lists,
      election_data$election_results,
      "advanced",
      c("county", "race" = "race_name", "full_name" = "candidate")
    ),

    board_members_advanced = filter_by_outcome(
      board_with_races,
      election_data$election_results,
      "advanced",
      c("county", "race" = "race_name", "full_name" = "ballot_name")
    )[!is.na(psrc_name)  # Filter to return only board members
    ][, `:=`(
      not_seeking_reelection = NULL,  # Drop logically obvious fields
      not_up_for_reelection = NULL
    )] %>%
      # Add seeking_new_office logic
      .[election_data$scheduled_races,
        on = c("district", "office"),
        `:=`(seeking_new_office = incumbent != ballot_name)
      ] %>%
      standardize_and_sort(),

    board_members_lost = filter_by_outcome(
      board_with_races,
      election_data$election_results,
      "lost",
      c("county", "race" = "race_name", "full_name" = "ballot_name")
    )[!is.na(psrc_name)  # Filter to return only board members
    ][, `:=`(
      not_seeking_reelection = NULL,  # Drop logically obvious fields
      not_up_for_reelection = NULL
    )] %>%
      # Add seeking_new_office logic
      .[election_data$scheduled_races,
        on = c("district", "office"),
        `:=`(seeking_new_office = incumbent != ballot_name)
      ] %>%
      standardize_and_sort(),

    not_seeking_reelection = election_tracker_input[
      not_seeking_reelection == TRUE
    ][, `:=`(
      not_seeking_reelection = NULL,
      not_up_for_reelection = NULL,
      seeking_new_office = NULL,
      ballot_name = NULL
    )] %>%
      standardize_and_sort(),

    not_up_for_reelection = election_tracker_input[
      not_up_for_reelection == TRUE
    ][, `:=`(
      not_seeking_reelection = NULL,
      not_up_for_reelection = NULL,
      seeking_new_office = NULL,
      ballot_name = NULL
    )] %>%
      standardize_and_sort()
  )
}

make_general_election_report <- function(election_date, election_code = NULL, year = NULL,
                                         election_tracker_input = NULL) {

  # Load election tracker input if not provided
  if (is.null(election_tracker_input)) {
    election_tracker_input <- load_election_tracker_input()
  }

  election_data <- load_election_data(year, election_code, election_date)
  board_with_races <- join_races_w_boards_updated(election_data$scheduled_races, election_tracker_input)
  races_with_race_col <- election_data$scheduled_races[, race := paste(district, office)]

  list(
    winners = filter_by_outcome(
      races_with_race_col,
      election_data$election_results,
      "won",
      c("county", "race" = "race_name")
    ) %>% standardize_and_sort(c("county", "district", "office", "candidate")),

    board_members_won = filter_by_outcome(
      board_with_races,
      election_data$election_results,
      "won",
      c("county", "race" = "race_name", "full_name" = "ballot_name")
    )[!is.na(psrc_name)  # Filter to return only board members
    ][, `:=`(
      not_seeking_reelection = NULL,  # Drop logically obvious fields
      not_up_for_reelection = NULL
    )] %>%
      # Add seeking_new_office logic
      .[election_data$scheduled_races,
        on = c("district", "office"),
        `:=`(seeking_new_office = incumbent != ballot_name)
      ] %>%
      standardize_and_sort(),

    board_members_lost = filter_by_outcome(
      board_with_races,
      election_data$election_results,
      "lost",
      c("county", "race" = "race_name", "full_name" = "ballot_name")
    )[!is.na(psrc_name)  # Filter to return only board members
    ][, `:=`(
      not_seeking_reelection = NULL,  # Drop logically obvious fields
      not_up_for_reelection = NULL
    )] %>%
      # Add seeking_new_office logic
      .[election_data$scheduled_races,
        on = c("district", "office"),
        `:=`(seeking_new_office = incumbent != ballot_name)
      ] %>%
      standardize_and_sort(),

    non_board_reelecteds = races_with_race_col %>%
      filter_by_outcome(election_data$election_results, "won", c("county", "race" = "race_name")) %>%
      .[candidate == incumbent] %>%
      # Exclude board members using the election tracker
      .[!election_tracker_input[!is.na(ballot_name)],
        on = c("candidate" = "ballot_name")] %>%
      standardize_and_sort(c("county", "district", "title", "candidate")),

    newly_electeds = races_with_race_col %>%
      filter_by_outcome(election_data$election_results, "won", c("county", "race" = "race_name")) %>%
      .[candidate != incumbent] %>%
      setnames("incumbent", "replaces") %>%
      standardize_and_sort(c("county", "district", "office", "candidate")),

    not_seeking_reelection = election_tracker_input[
      not_seeking_reelection == TRUE
    ][, `:=`(
      not_seeking_reelection = NULL,
      not_up_for_reelection = NULL,
      seeking_new_office = NULL,
      ballot_name = NULL
    )] %>%
      standardize_and_sort(),

    not_up_for_reelection = election_tracker_input[
      not_up_for_reelection == TRUE
    ][, `:=`(
      not_seeking_reelection = NULL,
      not_up_for_reelection = NULL,
      seeking_new_office = NULL,
      ballot_name = NULL
    )] %>%
      standardize_and_sort()
  )
}

# Workflow example -----------------------------
# eti <- generate_election_tracker_input(2025, 893)
# eti <- load_election_tracker_input()
# candidate_filing_2025 <- make_candidate_filing_report(2025, 893, eti)
# primary_election_2025 <- make_primary_election_report(2025, 893)
# general_election_2025 <- make_general_election_report(2025, 894)

