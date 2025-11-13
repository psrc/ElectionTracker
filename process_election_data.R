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

# Normalize race/jurisdiction strings to improve join reliability
normalize_race <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("\\b(city of|county|port of)\\b", "") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
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

standardize_and_sort <- function(data, sort_cols = c("county", "district", "race", "name"),
                                 keep_cols = candidate_rpt_cols) {
  dt <- copy(data)
  existing_cols <- intersect(sort_cols, names(dt))
  if (length(existing_cols) > 0) {
    do.call(setorder, c(list(dt), as.list(existing_cols)))
  }
  dt[, .SD, .SDcols = intersect(keep_cols, names(dt))] %>% unique()
}

# Primary Workflow --------------------
load_election_data <- function(year = NULL, election_code = NULL, election_date = NULL, election_type = "general") {
  year <- year %||% format(Sys.Date(), "%Y")

  scheduled_races <- scrape_scheduled_races(year) %>% setDT()
  scheduled_norm <- copy(scheduled_races)
  if (!"incumbent" %in% names(scheduled_norm)) scheduled_norm[, incumbent := NA_character_]
  scheduled_norm[, `:=`(
    district = as.character(district),
    county = as.character(county),
    incumbent = as.character(incumbent)
  )]
  if ("office" %in% names(scheduled_norm)) {
    scheduled_norm[, race_label := as.character(office)]
  } else if ("race" %in% names(scheduled_norm)) {
    scheduled_norm[, race_label := as.character(race)]
  } else {
    scheduled_norm[, race_label := NA_character_]
  }
  scheduled_norm[, full_race_name := paste(district, race_label)]
  scheduled_norm[, full_race_name_norm := normalize_race(full_race_name)]

  candidate_lists <- scrape_candidate_lists(election_code) %>% setDT()
  election_results <- if (!is.null(election_date)) scrape_election_results(election_date, election_type) else NULL
  if (!is.null(election_results)) setDT(election_results)
  psrc_boards <- scrape_psrc_boards() %>% setDT()

  list(
    scheduled_races = scheduled_races,
    scheduled_norm = scheduled_norm,
    candidate_lists = candidate_lists,
    election_results = election_results,
    psrc_boards = psrc_boards
  )
}

load_election_tracker_input <- function(input_file = "election_tracker_input.xlsx") {
  if (!file.exists(input_file)) {
    stop("Election tracker input file not found: ", input_file,
         "\nPlease run generate_election_tracker_input() first.")
  }

  election_tracker_input <- read.xlsx(input_file, sheet = 1) %>%
    as.data.table()

  if ("ballot_name" %in% names(election_tracker_input)) {
    election_tracker_input[, ballot_name_norm := normalize_race(ballot_name)]
  }

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
        on = c("county", "full_race_name_norm" = "race_name_norm", "name" = "candidate"), nomatch = NULL] %>%
      rbind(election_data$candidate_lists[election_status == "Advanced to General"], fill=TRUE) %>%
      standardize_and_sort(keep_cols = c(candidate_rpt_cols, "full_race_name", "votes", "percentage", "outcome")),

    board_members_advanced = board_members_running[status=="Active"] %>%
      .[election_data$election_results[outcome == "advanced"],
        on = c("county", "full_race_name_norm" = "race_name_norm", "name" = "candidate"), nomatch = NULL] %>%
      rbind(board_members_running[election_status=="Advanced to General"], fill=TRUE) %>%
      standardize_and_sort(keep_cols = c(candidate_rpt_cols, "full_race_name", "votes", "percentage", "outcome")),

    board_members_eliminated = board_members_running %>%
      .[election_data$election_results[outcome == "lost"],
        on = c("county", "full_race_name_norm" = "race_name_norm", "name" = "candidate"), nomatch = NULL] %>%
      rbind(board_members_running[status!="Active"], fill=TRUE) %>%
      standardize_and_sort(keep_cols = c(candidate_rpt_cols, "full_race_name", "votes", "percentage", "outcome")),

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
  # Use normalized scheduled races from load_election_data for consistency
  scheduled_norm <- election_data$scheduled_norm

  winners <- election_data$candidate_lists[status=="Active"] %>%
    .[election_data$election_results[outcome == "won"],
      on = c("county", "full_race_name_norm" = "race_name_norm", "name" = "candidate"), nomatch = NULL] %>%
    .[scheduled_norm[, .(county, district, race = race_label, incumbent, full_race_name, full_race_name_norm)],
      on = c("county", "full_race_name_norm"), nomatch = 0]

  list(
    won = winners %>% standardize_and_sort(keep_cols = c(candidate_rpt_cols, "full_race_name", "votes", "percentage", "outcome")),

    board_members_won = board_members_running[status=="Active"] %>%
      .[election_data$election_results[outcome == "won"],
        on = c("county", "full_race_name_norm" = "race_name_norm", "name" = "candidate"), nomatch = NULL] %>%
      standardize_and_sort(keep_cols = c(candidate_rpt_cols, "full_race_name", "votes", "percentage", "outcome")),

    board_members_lost = board_members_running %>%
      .[election_data$election_results[outcome == "lost"],
        on = c("county", "full_race_name_norm" = "race_name_norm", "name" = "candidate"), nomatch = NULL] %>%
      rbind(board_members_running[status!="Active"], fill=TRUE) %>%
      standardize_and_sort(keep_cols = c(candidate_rpt_cols, "full_race_name", "votes", "percentage", "outcome")),

    non_board_reelecteds = winners[name == incumbent] %>%
      # Exclude board members by composite key (county + normalized race + candidate name)
      .[!board_members_running[, .(county, full_race_name_norm, name)],
        on = c("county", "full_race_name_norm", "name")] %>%
      standardize_and_sort(keep_cols = c(candidate_rpt_cols, "full_race_name", "votes", "percentage", "outcome")),

    newly_electeds = winners[name != incumbent] %>%
      setnames("incumbent", "replaces") %>%
      standardize_and_sort(keep_cols = c(candidate_rpt_cols, "full_race_name", "votes", "percentage", "outcome", "replaces")),

    not_seeking_reelection = election_tracker_input[not_seeking_reelection == TRUE] %>%
      .[, .SD, .SDcols = intersect(noncandidate_rpt_cols, names(.))]
  )
}

# Diagnostic: surface normalization mismatches across sources
diagnose_race_normalization <- function(election_code = NULL, year = NULL,
                                        election_tracker_input = NULL) {
  if (is.null(election_tracker_input)) {
    election_tracker_input <- load_election_tracker_input()
  }
  election_data <- load_election_data(year, election_code)

  # Build normalized race names for scheduled races if columns exist
  scheduled <- copy(election_data$scheduled_norm)

  candidates <- copy(election_data$candidate_lists)
  if (!"full_race_name_norm" %in% names(candidates) && "full_race_name" %in% names(candidates)) {
    candidates[, full_race_name_norm := normalize_race(full_race_name)]
  }

  unmatched_scheduled <- scheduled[!(full_race_name_norm %in% candidates$full_race_name_norm)]
  unmatched_candidates <- candidates[!(full_race_name_norm %in% scheduled$full_race_name_norm)]

  # Only consider board members who are actually expected to run
  active_board_members <- election_tracker_input[!(not_seeking_reelection == TRUE | not_up_for_reelection == TRUE)]
  missing_ballot_names <- active_board_members[is.na(ballot_name) | ballot_name == ""]

  list(
    unmatched_scheduled = unmatched_scheduled[, .(district, office = race_label, full_race_name, full_race_name_norm)][order(full_race_name_norm)],
    unmatched_candidates = unmatched_candidates[, .(county, district, race, full_race_name, full_race_name_norm)][order(full_race_name_norm)],
    missing_ballot_names = missing_ballot_names[, .(psrc_name, board_affiliation, ballot_name, ballot_name_norm)],
    overlap_counts = list(
      scheduled_total = nrow(scheduled),
      candidates_total = nrow(candidates),
      scheduled_unmatched = nrow(unmatched_scheduled),
      candidates_unmatched = nrow(unmatched_candidates),
      active_board_missing_ballot_name = nrow(missing_ballot_names)
    )
  )
}

# Helper: write list of data.tables/data.frames to an Excel workbook
# Each element of `tables_list` becomes a worksheet. Names are taken from the list
# (or auto-generated) and sanitized for Excel sheet constraints.
write_report_list_xlsx <- function(tables_list, output_file = "report.xlsx") {
  if (!is.list(tables_list) || length(tables_list) == 0) {
    stop("tables_list must be a non-empty list of data.frames/data.tables")
  }

  # Sanitize worksheet names
  sanitize_name <- function(n, idx) {
    if (is.null(n) || n == "") n <- paste0("Sheet", idx)
    # Remove invalid characters and trim length to 31 (Excel limit)
    n <- gsub("[*?:\\/\\]", "_", n)
    n <- substr(n, 1, 31)
    n
  }

  wb <- createWorkbook()
  header_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2")

  for (i in seq_along(tables_list)) {
    tbl <- tables_list[[i]]
    # Coerce to data.table for consistency
    if (!is.data.table(tbl)) tbl <- as.data.table(tbl)
    sheet_name <- sanitize_name(names(tables_list)[i], i)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, tbl)
    addStyle(wb, sheet = sheet_name, style = header_style, rows = 1, cols = 1:ncol(tbl), gridExpand = TRUE)
    # Auto column widths (fallback to fixed if error)
    try(setColWidths(wb, sheet = sheet_name, cols = 1:ncol(tbl), widths = "auto"), silent = TRUE)
  }

  saveWorkbook(wb, output_file, overwrite = TRUE)
  message("Report written to ", output_file, " with ", length(tables_list), " worksheets.")
  invisible(output_file)
}

# Workflow example -----------------------------
# eti <- generate_election_tracker_input(year = 2025, election_code = 893)
# eti <- load_election_tracker_input()
# diag <- diagnose_race_normalization(election_code = 893, year = 2025, election_tracker_input = eti)
# str(diag$overlap_counts)
# View(diag$unmatched_scheduled)
# View(diag$unmatched_candidates)
# View(diag$missing_ballot_names)
# candidate_filing_2025 <- make_candidate_filing_report(election_code = 893, year = 2025, election_tracker_input = eti)
# primary_election_2025 <- make_primary_election_report(election_date = "20250805", election_code = 893, year = 2025, election_tracker_input = eti)
# general_election_2025 <- make_general_election_report(election_date = "20251104", election_code = 894, year = 2025, election_tracker_input = eti)

