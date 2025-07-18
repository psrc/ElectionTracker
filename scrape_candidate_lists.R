library(rvest)
library(readr)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(httr)

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

    viewstate_generator <- page %>%
      html_node("input[name='__VIEWSTATEGENERATOR']") %>%
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
        "__VIEWSTATEGENERATOR" = if(length(viewstate_generator) > 0) viewstate_generator[1] else "",
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

          # Parse CSV
          candidate_data <- read_csv(csv_content, show_col_types = FALSE) %>%
            # Clean column names
            rename_with(~ tolower(gsub("[^A-Za-z0-9]", "_", .x))) %>%
            select(c("district", "race", "name", "status", "election_status")) %>%
            # Add county information
            mutate(district = str_to_title(district),
                   county   = county_name)

          # Filter based on district and office terms
          district_col <- colnames(candidate_data)[str_detect(colnames(candidate_data), "district_type|district|jurisdiction")]
          office_col <- colnames(candidate_data)[str_detect(colnames(candidate_data), "race|office|position|title")]

          if (length(district_col) > 0 && length(office_col) > 0) {
            filtered_data <- candidate_data %>%
              filter(
                str_detect(tolower(.data[[district_col[1]]]), paste(DISTRICT_TERMS, collapse = "|")) &
                  str_detect(tolower(.data[[office_col[1]]]), paste(OFFICE_TERMS, collapse = "|"))
              )
          } else {
            filtered_data <- candidate_data
          }

          message("Found ", nrow(filtered_data), " candidates for ", county_name, " County")
          return(filtered_data)
        }
      }
    }

    # Fallback to HTML table parsing
    message("CSV export failed, using HTML parsing for ", county_name)

    # Look for table with candidate data
    tables <- page %>% html_table(fill = TRUE)

    if (length(tables) > 0) {
      # Find the main candidates table (usually the largest one)
      main_table <- tables[[which.max(sapply(tables, nrow))]]

      if (nrow(main_table) > 0 && ncol(main_table) > 5) {
        # Clean column names
        colnames(main_table) <- tolower(gsub("[^A-Za-z0-9]", "_", colnames(main_table)))

        # Add county information
        main_table$county <- county_name
        main_table$county_code <- county_code
        main_table$election_code <- election_code

        # Filter based on district and office terms
        district_col <- colnames(main_table)[str_detect(colnames(main_table), "district_type|district|jurisdiction")]
        office_col <- colnames(main_table)[str_detect(colnames(main_table), "race|office|position|title")]

        if (length(district_col) > 0 && length(office_col) > 0) {
          filtered_table <- main_table %>%
            filter(
              str_detect(tolower(.data[[district_col[1]]]), paste(DISTRICT_TERMS, collapse = "|")) &
                str_detect(tolower(.data[[office_col[1]]]), paste(OFFICE_TERMS, collapse = "|"))
            )
        } else {
          # If we can't find the right columns, include all data for manual filtering
          filtered_table <- main_table
        }

        message("Found ", nrow(filtered_table), " candidates for ", county_name, " County (HTML parsing)")
        return(filtered_table)
      }
    }

    # Return empty tibble if everything failed
    message("No candidate data found for ", county_name, " County")
    return(tibble())

  }, error = function(e) {
    message("Error processing ", county_name, " County candidates: ", e$message)
    return(tibble())
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

  all_candidates <- tibble()
  failed_counties <- character()

  # Process each county
  for (county_name in names(COUNTY_CODES)) {
    county_code <- COUNTY_CODES[[county_name]]

    county_data <- scrape_single_county_candidates(election_code, county_name, county_code)

    if (nrow(county_data) > 0) {
      all_candidates <- bind_rows(all_candidates, county_data)
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
    # Remove duplicates and sort using proper column references
    all_candidates <- all_candidates %>%
      distinct()

    # Find appropriate columns for sorting
    district_col <- colnames(all_candidates)[str_detect(colnames(all_candidates), "district_type|district|jurisdiction")]
    office_col <- colnames(all_candidates)[str_detect(colnames(all_candidates), "race|office|position|title")]

    if (length(district_col) > 0 && length(office_col) > 0) {
      all_candidates <- all_candidates %>%
        arrange(county, .data[[district_col[1]]], .data[[office_col[1]]])
    } else {
      all_candidates <- all_candidates %>%
        arrange(county)
    }
  }

  return(all_candidates)
}
