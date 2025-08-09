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
DISTRICT_EXCLUSIONS <- c("school", "water", "fire", "sewer", "parks", "airport",
                         "charter review", "ward", "recreation")
candidate_rpt_cols <- c("county", "district", "race", "name", "board_affiliation",
                         "email", "election_status", "seeking_new_office")
noncandidate_rpt_cols <- c("title","psrc_name","psrc_district","board_affiliation")


## Helper for consistent capitalization
custom_title_case <- function(text) {
  exceptions <- c("and", "of", "the")
  map_chr(text, function(single_text) {
    words <- str_split(single_text, " ")[[1]]
    words <- ifelse(tolower(words) %in% exceptions,
                    tolower(words),
                    str_to_title(words))
    str_trim(str_c(words, collapse = " "))
  })
}

source("scrape_psrc_boards.R")
source("scrape_scheduled_races.R")
source("scrape_candidate_lists.R")
source("generate_election_tracker_input.R")
source("scrape_election_results.R")

# Reporting helper functions --------------------------

join_boards_w_candidates <- function(election_tracker_input, candidate_lists) {
  # Create lookup for PSRC name to ballot name
  psrc_lookup <- election_tracker_input[
    !is.na(ballot_name) & !not_seeking_reelection & !not_up_for_reelection
  ]

  # Join scheduled races with PSRC board info using ballot names
  result <- candidate_lists %>%
    .[psrc_lookup, on = c("name" = "ballot_name"), allow.cartesian = TRUE] #%>%
    #.[mapply(grepl, psrc_district, full_race_name, fixed=TRUE)]

  return(result)
}

standardize_and_sort <- function(data, sort_cols = c("county", "district", "race", "name")) {
  # Only sort by columns that exist
  existing_cols <- intersect(sort_cols, names(data))
  if (length(existing_cols) > 0) {
    do.call(setorder, c(list(data), as.list(existing_cols)))
  }
  data[, .SD, .SDcols = intersect(candidate_rpt_cols, names(data))] %>% unique()
}

# Primary Workflow --------------------
load_election_data <- function(year = NULL, election_code = NULL, election_date = NULL, election_type = "general") {
  year <- year %||% format(Sys.Date(), "%Y")

  list(
    scheduled_races = scrape_scheduled_races(year),
    candidate_lists = scrape_candidate_lists(election_code),
    election_results = if (!is.null(election_date)) scrape_election_results(election_date, election_type),
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
  board_members_running <- join_boards_w_candidates(election_tracker_input, election_data$candidate_lists)

  list(
    candidate_list = election_data$candidate_lists[status == "Active"] %>%
      standardize_and_sort(),

    board_members_running = board_members_running %>% standardize_and_sort(),

    board_members_no_reelection = election_tracker_input[not_seeking_reelection == TRUE] %>%
      .[, .SD, .SDcols = intersect(noncandidate_rpt_cols, names(.))],

    others_no_reelection = election_data$scheduled_races[
      !election_data$candidate_lists[status=="Active"],
      on = c("district", "office"="race", "incumbent"="name")] %>%
      .[!election_tracker_input, on = c("incumbent"="ballot_name")]
  )
}

make_primary_election_report <- function(election_date, election_code = NULL, year = NULL,
                                         election_tracker_input = NULL) {

  # Load election tracker input if not provided
  if (is.null(election_tracker_input)) {
    election_tracker_input <- load_election_tracker_input()
  }

  election_data <- load_election_data(year, election_code, election_date, "primary")
  board_members_running <- join_boards_w_candidates(election_tracker_input, election_data$candidate_lists)

  list(
    advancing = election_data$candidate_lists[status=="Active"] %>%
      .[election_data$election_results[outcome == "advanced"],
        on = c("county", "name" = "candidate"), nomatch = NULL] %>%
      #.[mapply(grepl, full_race_name, race_name, fixed = TRUE)] %>%
      .[, .SD, .SDcols = names(election_data$candidate_lists)] %>%
      rbind(election_data$candidate_lists[election_status == "Advanced to General"], fill=TRUE) %>%
      standardize_and_sort(),

    board_members_advanced = board_members_running[status=="Active"] %>%
      .[election_data$election_results[outcome == "advanced"],
        on = c("county", "name" = "candidate"), nomatch = NULL] %>%
      #.[mapply(grepl, full_race_name, race_name, fixed = TRUE)] %>%
      .[, .SD, .SDcols = names(board_members_running)] %>%
      rbind(board_members_running[election_status=="Advanced to General"], fill=TRUE) %>%
      standardize_and_sort(),

    board_members_eliminated = board_members_running %>%
      .[election_data$election_results[outcome == "lost"],
        on = c("county", "name" = "candidate"), nomatch = NULL] %>%
      #.[mapply(grepl, full_race_name, race_name, fixed = TRUE)] %>%
      rbind(board_members_running[status!="Active"], fill=TRUE) %>%
      standardize_and_sort(),

    not_seeking_reelection = election_tracker_input[not_seeking_reelection == TRUE] %>%
      .[, .SD, .SDcols = intersect(noncandidate_rpt_cols, names(.))]
  )
}

make_general_election_report <- function(election_date, election_code = NULL, year = NULL,
                                         election_tracker_input = NULL) {

  # Load election tracker input if not provided
  if (is.null(election_tracker_input)) {
    election_tracker_input <- load_election_tracker_input()
  }

  election_data <- load_election_data(year, election_code, election_date, "general")
  board_members_running <- join_boards_w_candidates(election_tracker_input, election_data$candidate_lists)
  winners <- election_data$candidate_lists[status=="Active"] %>%
    .[election_data$election_results[outcome == "won"],
      on = c("county", "full_race_name" = "race_name", "name" = "candidate"), nomatch = NULL]

  list(
    won = winners %>% standardize_and_sort(),

    board_members_won = board_members_running[status=="Active"] %>%
      .[election_data$election_results[outcome == "won"],
        on = c("county", "name" = "candidate"), nomatch = NULL] %>%
      #.[mapply(grepl, full_race_name, race_name, fixed = TRUE)] %>%
      standardize_and_sort(),

    board_members_lost = board_members_running %>%
      .[election_data$election_results[outcome == "lost"],
        on = c("county", "name" = "candidate"), nomatch = NULL] %>%
      #.[mapply(grepl, full_race_name, race_name, fixed = TRUE)] %>%
      rbind(board_members_running[status!="Active"], fill=TRUE) %>%
      standardize_and_sort(),

    non_board_reelecteds = winners[name == incumbent] %>%
      # Exclude board members using the election tracker
      .[!election_tracker_input[!is.na(ballot_name)],
        on = c("name" = "ballot_name")] %>%
      standardize_and_sort(),

    newly_electeds = winners[name != incumbent] %>%
      setnames("incumbent", "replaces") %>%
      standardize_and_sort(),

    not_seeking_reelection = election_tracker_input[not_seeking_reelection == TRUE] %>%
      .[, .SD, .SDcols = intersect(noncandidate_rpt_cols, names(.))]
  )
}

# Workflow example -----------------------------
# eti <- generate_election_tracker_input(2025, 893)
# eti <- load_election_tracker_input()
# candidate_filing_2025 <- make_candidate_filing_report(2025, 893, eti)
# primary_election_2025 <- make_primary_election_report("20250805", 893, 2025, eti)
# general_election_2025 <- make_general_election_report("20251104", 894, 2025, eti)

