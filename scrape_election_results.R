library(rvest)
library(data.table)
library(purrr)
library(stringr)
library(httr)
library(xml2)
library(magrittr)

# The statewide export has no jurisdiction field or percentages. Classify local
# races by name; the scheduled-races scraper does not expose comparable IDs.
parse_statewide_county_results <- function(county_results, county_name, election_type) {
  if (!is.list(county_results$ballotItems)) {
    stop("Missing or invalid ballotItems for ", county_name, " County")
  }

  rows <- list()
  for (contest in county_results$ballotItems) {
    if (!is.list(contest) || !is.character(contest$name) ||
        length(contest$name) != 1L || is.na(contest$name) ||
        !is.character(contest$type) || length(contest$type) != 1L ||
        is.na(contest$type) || !is.character(contest$contestType) ||
        length(contest$contestType) != 1L || is.na(contest$contestType)) {
      stop("Invalid contest metadata for ", county_name, " County")
    }

    contest_name <- tolower(contest$name)
    if (contest$type != "Local" || contest$contestType != "Candidate" ||
        !str_detect(contest_name, paste(OFFICE_TERMS, collapse = "|")) ||
        !str_detect(contest_name, "\\b(city|town|county|port)\\b") ||
        str_detect(contest_name, paste(c(DISTRICT_EXCLUSIONS, "\\butility\\b"), collapse = "|"))) {
      next
    }
    if (!is.list(contest$ballotOptions)) {
      stop("Missing or invalid ballotOptions for contest: ", contest$name)
    }
    if (!length(contest$ballotOptions)) next

    options <- lapply(contest$ballotOptions, function(option) {
      if (!is.list(option) || !is.character(option$name) ||
          length(option$name) != 1L || is.na(option$name) ||
          !is.numeric(option$voteCount) || length(option$voteCount) != 1L ||
          is.na(option$voteCount) || !is.finite(option$voteCount) ||
          option$voteCount < 0) {
        stop("Invalid candidate or vote count for contest: ", contest$name)
      }
      data.table(candidate = option$name, votes = as.numeric(option$voteCount))
    })
    contest_results <- rbindlist(options)
    # Match the XML path's county-level outcomes, including write-in votes in
    # the denominator and rankings before excluding write-ins from the output.
    total_votes <- sum(contest_results$votes)
    contest_results[, percentage := if (total_votes > 0) 100 * votes / total_votes else NA_real_]
    if (election_type == "general") {
      contest_results[, outcome := ifelse(votes == max(votes), "won", "lost")]
    } else {
      contest_results[, outcome := fcase(
        frank(-votes, ties.method = "min") <= 2, "advanced",
        default = "lost"
      )]
    }
    contest_results <- contest_results[!tolower(candidate) %in% c("yes", "no", "write-in")]
    contest_results[, `:=`(contest_name = custom_title_case(contest$name), county = county_name)]
    setcolorder(contest_results, c("contest_name", "candidate", "votes", "percentage", "county", "outcome"))
    rows[[length(rows) + 1L]] <- contest_results
  }
  rbindlist(rows)
}

scrape_statewide_results <- function(election_date, election_type) {
  url <- paste0("https://results.votewa.gov/cdn/results/washington/export-", election_date, ".json")
  counties <- names(COUNTY_CODES)
  export <- tryCatch({
    response <- GET(url, timeout(60))
    if (http_error(response)) {
      stop("HTTP status: ", status_code(response))
    }
    parsed <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"),
                                 simplifyVector = FALSE)
    if (!is.list(parsed) || !is.character(parsed$electionDate) ||
        length(parsed$electionDate) != 1L || is.na(parsed$electionDate) ||
        parsed$electionDate != format(as.Date(election_date, "%Y%m%d"), "%Y-%m-%d") ||
        !is.list(parsed$localResults)) {
      stop("Invalid statewide export or election date does not match the request")
    }
    parsed
  }, error = function(e) {
    message("Error retrieving statewide election results from ", url, ": ", conditionMessage(e))
    NULL
  })

  if (is.null(export)) {
    return(list(results = data.table(), failed_counties = counties))
  }

  results <- list()
  failed_counties <- character()
  for (county_name in counties) {
    county_data <- tryCatch({
      matches <- which(vapply(export$localResults, function(x) {
        is.list(x) && identical(x$name, paste(county_name, "County"))
      }, logical(1)))
      if (length(matches) != 1L) {
        stop("Expected exactly one county section, found ", length(matches))
      }
      parse_statewide_county_results(export$localResults[[matches]], county_name, election_type)
    }, error = function(e) {
      message("Error processing ", county_name, " County results: ", conditionMessage(e))
      NULL
    })
    if (is.null(county_data)) {
      failed_counties <- c(failed_counties, county_name)
    } else {
      results[[length(results) + 1L]] <- county_data
      message("Found ", nrow(county_data), " relevant election results for ", county_name, " County")
    }
  }
  list(results = rbindlist(results, fill = TRUE), failed_counties = failed_counties)
}

# Helper function for single county (internal use)
scrape_single_county_results <- function(election_date, county_name, election_type) {

  message("Processing ", county_name, " County election results")

  # Format URL for XML results
  base_url <- paste0("https://results.vote.wa.gov/results/", election_date, "/export/", election_date, "_", tolower(county_name), ".xml")

  tryCatch({
    # Fetch the XML data
    response <- GET(base_url, timeout(60))

    if (!http_error(response)) {
      xml_content <- content(response, "text", encoding = "UTF-8")

      # Parse XML
      xml_doc <- read_xml(xml_content)

      # Extract all Result nodes
      results <- xml_find_all(xml_doc, "//Result")

      if (length(results) > 0) {
        # Convert XML to data.table
        results_data <- map_dfr(results, function(result) {
          data.table(
            contest_name = custom_title_case(xml_text(xml_find_first(result, "RaceName"))),
            candidate = xml_text(xml_find_first(result, "Candidate")),
            party = xml_text(xml_find_first(result, "Party")),
            votes = as.numeric(xml_text(xml_find_first(result, "Votes"))),
            percentage = as.numeric(xml_text(xml_find_first(result, "PercentageOfTotalVote"))),
            jurisdiction_name = xml_text(xml_find_first(result, "JurisdictionName"))
          )
        }) %>%
          # Convert to data.table; add metadata & outcome
          as.data.table() %>%
          .[, county := county_name]

        if(election_type=="general"){
          results_data[, outcome := ifelse(votes == max(votes), "won", "lost"),
                         by = .(county, contest_name)]
        }else if(election_type=="primary"){
          results_data[, outcome := fcase(
            frank(-votes, ties.method = "min") <= 2, "advanced",
            default = "lost"
          ), by = .(county, contest_name)]
        }

        # Filter based on jurisdiction terms if specified
        if ("jurisdiction_name" %in% colnames(results_data)) {
          filtered_data <- results_data[
            jurisdiction_name %in% c("City/Town", "County", "Port") &
              !candidate %in% c("No", "Yes", "WRITE-IN") &
              str_detect(tolower(contest_name), paste(OFFICE_TERMS , collapse = "|")) &
              !str_detect(tolower(contest_name), paste(DISTRICT_EXCLUSIONS , collapse = "|"))
          ] %>%
            .[, !c("jurisdiction_name", "party")]
        } else {
          filtered_data <- results_data
        }

        if (nrow(filtered_data) > 0) {
          message("Found ", nrow(filtered_data), " relevant election results for ", county_name, " County")
        } else {
          message("No relevant local races found for ", county_name, " County (page retrieved successfully)")
        }
        return(filtered_data)

      } else {
        message("No results found in XML for ", county_name, " County")
        return(data.table())
      }
    } else {
      message("ERROR: Failed to fetch XML data for ", county_name, " County. HTTP status: ", status_code(response))
      return(structure(data.table(), retrieval_failed = TRUE))
    }

  }, error = function(e) {
    message("Error processing ", county_name, " County results: ", e$message)
    return(structure(data.table(), retrieval_failed = TRUE))
  })
}

# Returns partial results with a failed_counties attribute when retrieval fails.
scrape_election_results <- function(election_date, election_type="general") {

  # Validate date format (should be YYYYMMDD)
  if (length(election_date) != 1L || is.na(election_date) ||
      !str_detect(as.character(election_date), "^\\d{8}$")) {
    stop("Election date must be in YYYYMMDD format (e.g., '20250422')")
  }
  election_date <- as.character(election_date)
  parsed_date <- as.Date(election_date, format = "%Y%m%d")
  if (is.na(parsed_date) || format(parsed_date, "%Y%m%d") != election_date) {
    stop("Election date must be a valid calendar date in YYYYMMDD format")
  }
  if (length(election_type) != 1L || is.na(election_type) ||
      !election_type %in% c("general", "primary")) {
    stop("Election type must be 'general' or 'primary'")
  }

  message("Scraping election results for date: ", election_date, " across all counties")

  all_results <- data.table()
  failed_counties <- character()

  if (as.integer(substr(election_date, 1L, 4L)) >= 2026L) {
    statewide <- scrape_statewide_results(election_date, election_type)
    all_results <- statewide$results
    failed_counties <- statewide$failed_counties
  } else {
    # Process each county
    for (county_name in names(COUNTY_CODES)) {

      county_data <- scrape_single_county_results(election_date, county_name, election_type)

      if (nrow(county_data) > 0) {
        # Successfully retrieved page and found at least one relevant race
        all_results <- rbindlist(list(all_results, county_data), fill = TRUE)
      } else if (isTRUE(attr(county_data, "retrieval_failed"))) {
        # The county helper records HTTP, network, and parsing failures.
        # A successfully retrieved county with no relevant races is not a failure.
        failed_counties <- c(failed_counties, county_name)
      }

      # Add delay to be respectful to the server
      Sys.sleep(1)
    }
  }

  # Report results
  if (length(failed_counties) > 0) {
    message("\n", strrep("=", 60))
    message("DATA RETRIEVAL FAILED FOR SOME COUNTIES")
    message(strrep("=", 60))
    message("Failed counties: ", paste(failed_counties, collapse = ", "))
    message("Successfully collected data from: ", paste(setdiff(names(COUNTY_CODES), failed_counties), collapse = ", "))
    if (as.integer(substr(election_date, 1L, 4L)) >= 2026L) {
      message("\nCheck the statewide export: https://results.votewa.gov/cdn/results/washington/export-",
              election_date, ".json")
    } else {
      message("\nFor failed counties, check these URLs manually:")
      for (county in failed_counties) {
        url <- paste0("https://results.vote.wa.gov/results/", election_date, "/export/", election_date, "_", tolower(county), ".xml")
        message("   ", county, ": ", url)
      }
    }
    message(strrep("=", 60))
  }

  message("Total election results collected: ", nrow(all_results))

  # Only sort if we have data
  if (nrow(all_results) > 0) {
    # Remove duplicates and sort
    all_results <- all_results %>%
      unique() %>%
      setorder(county, contest_name, candidate)
  } else {
    all_results <- data.table(
      contest_name = character(), candidate = character(), votes = numeric(),
      percentage = numeric(), county = character(), outcome = character()
    )
  }

  attr(all_results, "failed_counties") <- failed_counties
  return(all_results)
}
