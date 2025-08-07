library(rvest)
library(readr)
library(data.table)
library(purrr)
library(stringr)
library(httr)
library(magrittr)

# Function to get the current election code
get_current_election_code <- function() {
  base_url <- "https://voter.votewa.gov/CandidateList.aspx"

  tryCatch({
    message("Determining current election code...")

    # Make a GET request that will follow redirects
    response <- GET(base_url, config = config(followlocation = TRUE))

    # Get the final URL after redirects
    final_url <- response$url
    message("Final URL after redirect: ", final_url)

    # Extract election code from the URL using regex
    election_code_match <- str_match(final_url, "e=([0-9]+)")

    if (!is.na(election_code_match[1, 2])) {
      election_code <- as.numeric(election_code_match[1, 2])
      message("Current election code detected: ", election_code)
      return(election_code)
    } else {
      message("Could not extract election code from URL: ", final_url)
      message("Falling back to default election code: 893")
      return(893)
    }

  }, error = function(e) {
    message("Error determining current election code: ", e$message)
    message("Falling back to default election code: 893")
    return(893)
  })
}

# Helper function for single county (internal use)
scrape_single_county_candidates <- function(election_code, county_name, county_code) {

  message("Processing ", county_name, " County candidates")

  base_url <- paste0("https://voter.votewa.gov/CandidateList.aspx?e=", election_code, "&c=", county_code)

  tryCatch({
    # First get the page to extract the form data needed for PostBack
    page <- read_html(base_url)

    # Extract the form data needed for ASP.NET PostBack
    viewstate <- page %>%
      html_node("input[name='__VIEWSTATE']") %>%
      html_attr("value")

    event_validation <- page %>%
      html_node("input[name='__EVENTVALIDATION']") %>%
      html_attr("value")

    # Try CSV export via PostBack if we have the required form data
    if (length(viewstate) > 0 && !is.na(viewstate[1]) &&
        length(event_validation) > 0 && !is.na(event_validation[1])) {

      message("Attempting CSV export for ", county_name)

      # Prepare PostBack data for CSV export
      post_data <- list(
        "__EVENTTARGET" = "ctl00$ContentPlaceHolder1$grdCandidates$ctl00$ctl02$ctl00$ExportToCsvButton",
        "__EVENTARGUMENT" = "",
        "__VIEWSTATE" = viewstate[1],
        "__EVENTVALIDATION" = event_validation[1]
      )

      # Additional form fields that might be needed
      additional_fields <- page %>%
        html_nodes("input[type='hidden']") %>%
        set_names(html_attr(., "name")) %>%
        map_chr(~ html_attr(.x, "value")) %>%
        discard(is.na)

      # Merge additional fields (only if they don't already exist)
      post_data <- c(post_data, additional_fields[!names(additional_fields) %in% names(post_data)])

      # Submit the PostBack request
      csv_response <- POST(
        base_url,
        body = post_data,
        encode = "form",
        add_headers(
          "Content-Type" = "application/x-www-form-urlencoded",
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        )
      )

      # Check if we got CSV data
      if (!http_error(csv_response)) {
        content_type <- headers(csv_response)$`content-type` %||% ""
        csv_content <- content(csv_response, "text", encoding = "UTF-8")

        # Check if response looks like CSV
        if (str_detect(content_type, "csv|text") || str_detect(csv_content, "^[^<]*,.*\n")) {
          message("Successfully got CSV data for ", county_name)

          # Parse CSV and convert to data.table
          candidate_data <- read_csv(csv_content, show_col_types = FALSE) %>%
            as.data.table() %>%
            # Clean column names
            setnames(., tolower(gsub("\\W", "_", names(.)))) %>%
            .[, .(district, race, name, mailing_address, email, status, election_status)] %>%
            # Add county information and full_race_name for matching
            .[, district := ifelse(grepl("^County", trimws(district)), paste(county_name, "County"),
                                  custom_title_case(district))] %>%
            .[, `:=`(county = county_name,
                     full_race_name = paste(district, race))]
          filtered_data <- candidate_data[
            str_detect(tolower(district), paste0(DISTRICT_TERMS, collapse = "|")) &
            str_detect(tolower(race), paste0(OFFICE_TERMS, collapse = "|")) &
            !str_detect(tolower(full_race_name), paste0(DISTRICT_EXCLUSIONS, collapse = "|"))
          ]

          message("Found ", nrow(filtered_data), " candidates for ", county_name, " County")
          return(filtered_data)
        }
      }
    }

    # Return empty data.table if everything failed
    message("CSV export failed for ", county_name, " County")
    return(data.table())

  }, error = function(e) {
    message("Error processing ", county_name, " County candidates: ", e$message)
    return(data.table())
  })
}

# Main vectorized function
scrape_candidate_lists <- function(election_code = NULL) {

  # If no election code provided, determine the current one
  if (is.null(election_code)) {
    election_code <- get_current_election_code()
  }

  # Ensure we have a valid election code
  if (is.null(election_code) || is.na(election_code) || election_code == "") {
    message("Warning: Could not determine election code, using default 893")
    election_code <- 893
  }

  message("Scraping candidate lists for election code: ", election_code, " across all counties")

  all_candidates <- data.table()
  failed_counties <- character()

  # Process each county
  for (county_name in names(COUNTY_CODES)) {
    county_code <- COUNTY_CODES[[county_name]]

    county_data <- scrape_single_county_candidates(election_code, county_name, county_code)

    if (nrow(county_data) > 0) {
      all_candidates <- rbindlist(list(all_candidates, county_data), fill = TRUE)
    } else {
      failed_counties <- c(failed_counties, county_name)
    }

    # Add delay to be respectful to the server
    Sys.sleep(2)
  }

  # Report results
  if (length(failed_counties) > 0) {
    message("\n", strrep("=", 60))
    message("CSV EXPORT FAILED FOR SOME COUNTIES")
    message(strrep("=", 60))
    message("Failed counties: ", paste(failed_counties, collapse = ", "))
    message("Successfully collected data from: ", paste(setdiff(names(COUNTY_CODES), failed_counties), collapse = ", "))
    message("\nFor failed counties, manually download CSV files from:")

    for (county in failed_counties) {
      county_code <- COUNTY_CODES[[county]]
      url <- paste0("https://voter.votewa.gov/CandidateList.aspx?e=", election_code, "&c=", county_code)
      message("   ", county, ": ", url)
    }
    message(strrep("=", 60))
  }

  message("Total candidates collected: ", nrow(all_candidates))

  # Only sort if we have data
  if (nrow(all_candidates) > 0) {
    # Remove duplicates
    all_candidates <- all_candidates %>% unique()

    if (all(c("district", "race") %in% colnames(all_candidates))){
      all_candidates <- all_candidates %>%
        setorder(county, district, race)
    } else {
      all_candidates <- all_candidates %>%
        setorder(county)
    }
  }

  return(all_candidates)
}
