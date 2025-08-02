library(rvest)
library(data.table)
library(purrr)
library(stringr)
library(httr)
library(xml2)
library(magrittr)

# Helper function for single county (internal use)
scrape_single_county_results <- function(election_date, county_name, election_type) {

  message("Processing ", county_name, " County election results")

  # Format URL for XML results
  base_url <- paste0("https://results.vote.wa.gov/results/", election_date, "/export/", election_date, "_", tolower(county_name), ".xml")

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
        # Convert XML to data.table
        results_data <- map_dfr(results, function(result) {
          data.table(
            race_name = xml_text(xml_find_first(result, "RaceName")),
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
                         by = .(county, race_name)]
        }else if(election_type=="primary"){
          results_data[, outcome := fcase(
            frank(-votes, ties.method = "min") <= 2, "advanced",
            default = "lost"
          ), by = .(county, race_name)]
        }

        # Filter based on jurisdiction terms if specified
        if ("jurisdiction_name" %in% colnames(results_data)) {
          filtered_data <- results_data[
            jurisdiction_name %in% c("City/Town", "County") &
              !candidate %in% c("No", "Yes", "WRITE-IN") &
              str_detect(tolower(race_name), paste(OFFICE_TERMS , collapse = "|"))
          ] %>%
            .[, !c("jurisdiction_name", "party")]
        } else {
          filtered_data <- results_data
        }

        message("Found ", nrow(filtered_data), " election results for ", county_name, " County")
        return(filtered_data)
      } else {
        message("No results found in XML for ", county_name, " County")
        return(data.table())
      }
    } else {
      message("Failed to fetch XML data for ", county_name, " County. HTTP status: ", status_code(response))
      return(data.table())
    }

  }, error = function(e) {
    message("Error processing ", county_name, " County results: ", e$message)
    return(data.table())
  })
}

# Main vectorized function
scrape_election_results <- function(election_date, election_type="general") {

  # Validate date format (should be YYYYMMDD)
  if (!str_detect(election_date, "^\\d{8}$")) {
    stop("Election date must be in YYYYMMDD format (e.g., '20250422')")
  }

  message("Scraping election results for date: ", election_date, " across all counties")

  all_results <- data.table()
  failed_counties <- character()

  # Process each county
  for (county_name in names(COUNTY_CODES)) {

    county_data <- scrape_single_county_results(election_date, county_name, election_type)

    if (nrow(county_data) > 0) {
      all_results <- rbindlist(list(all_results, county_data), fill = TRUE)
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
      url <- paste0("https://results.vote.wa.gov/results/", election_date, "/export/", election_date, "_", tolower(county_name), ".xml")
      message("   ", county, ": ", url)
    }
    message(strrep("=", 60))
  }

  message("Total election results collected: ", nrow(all_results))

  # Only sort if we have data
  if (nrow(all_results) > 0) {
    # Remove duplicates and sort
    all_results <- all_results %>%
      unique() %>%
      setorder(county, race_name, candidate)
  }

  return(all_results)
}
