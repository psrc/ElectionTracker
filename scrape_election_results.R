library(rvest)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(httr)
library(xml2)

# County codes mapping
COUNTY_CODES <- list(
  "King" = "king",
  "Kitsap" = "kitsap",
  "Pierce" = "pierce",
  "Snohomish" = "snohomish"
)

# Filtering terms for jurisdiction types
JURISDICTION_TERMS <- c("county", "city", "town", "municipal")

# Helper function for single county (internal use)
scrape_single_county_results <- function(election_date, county_name, county_code) {

  message("Processing ", county_name, " County election results")

  # Format URL for XML results
  base_url <- paste0("https://results.vote.wa.gov/results/", election_date, "/export/", election_date, "_", county_code, ".xml")

  tryCatch({
    # Fetch the XML data
    response <- GET(base_url)

    if (!http_error(response)) {
      xml_content <- content(response, "text", encoding = "UTF-8")

      # Parse XML
      xml_doc <- read_xml(xml_content)

      # Extract all Result nodes
      results <- xml_find_all(xml_doc, "//Result")

      if (length(results) > 0) {
        # Convert XML to data frame
        results_data <- map_dfr(results, function(result) {
          tibble(
            race_name = xml_text(xml_find_first(result, "RaceName")),
            candidate = xml_text(xml_find_first(result, "Candidate")),
            party = xml_text(xml_find_first(result, "Party")),
            votes = as.numeric(xml_text(xml_find_first(result, "Votes"))),
            percentage = as.numeric(xml_text(xml_find_first(result, "PercentageOfTotalVote"))),
            jurisdiction_name = xml_text(xml_find_first(result, "JurisdictionName"))
          )
        }) %>%
          # Add metadata
          mutate(
            county = county_name
          )

        # Filter based on jurisdiction terms if specified
        if ("jurisdiction_name" %in% colnames(results_data)) {
          filtered_data <- results_data %>%
            filter(jurisdiction_name %in% c("City/Town", "County") &
                     !candidate %in% c("No", "Yes", "WRITE-IN") &
                     str_detect(tolower(race_name), paste(c("mayor", "council", "executive", "commissioner"), collapse = "|"))
            ) %>%
            select(-any_of(c("jurisdiction_name", "party")))
        } else {
          filtered_data <- results_data
        }

        message("Found ", nrow(filtered_data), " election results for ", county_name, " County")
        return(filtered_data)
      } else {
        message("No results found in XML for ", county_name, " County")
        return(tibble())
      }
    } else {
      message("Failed to fetch XML data for ", county_name, " County. HTTP status: ", status_code(response))
      return(tibble())
    }

  }, error = function(e) {
    message("Error processing ", county_name, " County results: ", e$message)
    return(tibble())
  })
}

# Main vectorized function
scrape_election_results <- function(election_date) {

  # Validate date format (should be YYYYMMDD)
  if (!str_detect(election_date, "^\\d{8}$")) {
    stop("Election date must be in YYYYMMDD format (e.g., '20250422')")
  }

  message("Scraping election results for date: ", election_date, " across all counties")

  all_results <- tibble()
  failed_counties <- character()

  # Process each county
  for (county_name in names(COUNTY_CODES)) {
    county_code <- COUNTY_CODES[[county_name]]

    county_data <- scrape_single_county_results(election_date, county_name, county_code)

    if (nrow(county_data) > 0) {
      all_results <- bind_rows(all_results, county_data)
    } else {
      failed_counties <- c(failed_counties, county_name)
    }

    # Add delay to be respectful to the server
    Sys.sleep(1)
  }

  # Report results
  if (length(failed_counties) > 0) {
    message("\n", strrep("=", 60))
    message("DATA RETRIEVAL FAILED FOR SOME COUNTIES")
    message(strrep("=", 60))
    message("Failed counties: ", paste(failed_counties, collapse = ", "))
    message("Successfully collected data from: ", paste(setdiff(names(COUNTY_CODES), failed_counties), collapse = ", "))
    message("\nFor failed counties, check these URLs manually:")

    for (county in failed_counties) {
      county_code <- COUNTY_CODES[[county]]
      url <- paste0("https://results.vote.wa.gov/results/", election_date, "/export/", election_date, "_", county_code, ".xml")
      message("   ", county, ": ", url)
    }
    message(strrep("=", 60))
  }

  message("Total election results collected: ", nrow(all_results))

  # Only sort if we have data
  if (nrow(all_results) > 0) {
    # Remove duplicates and sort
    all_results <- all_results %>%
      distinct() %>%
      arrange(county, race_name, candidate)
  }

  return(all_results)
}

