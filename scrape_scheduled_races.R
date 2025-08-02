library(rvest)
library(data.table)
library(purrr)
library(stringr)
library(httr)
library(readr)
library(magrittr)

# Helper function for single county (internal use)
scrape_single_county_races <- function(year, county_name, county_code) {

  message("Processing ", county_name, " County races for year: ", year)

  base_url <- paste0("https://voter.votewa.gov/ScheduledRaces.aspx?y=", year, "&c=", county_code)

  tryCatch({
    # First get the page to extract the form data needed for PostBack
    page <- read_html(base_url)

    # Extract the form data needed for ASP.NET PostBack
    viewstate <- page %>%
      html_node("input[name='__VIEWSTATE__']") %>%
      html_attr("value")

    event_validation <- page %>%
      html_node("input[name='__EVENTVALIDATION']") %>%
      html_attr("value")

    # Try CSV export via PostBack if we have the required form data
    if (!is.na(viewstate) && !is.na(event_validation)) {
      message("Attempting CSV export for ", county_name, " County")

      # Prepare PostBack data for CSV export
      post_data <- list(
        "__EVENTTARGET" = "ctl00$ContentPlaceHolder1$grdScheduled$ctl00$ctl02$ctl00$ExportToCsvButton",
        "__EVENTARGUMENT" = "",
        "__VIEWSTATE__" = viewstate,
        "__EVENTVALIDATION" = event_validation
      )

      # Additional form fields that might be needed
      additional_fields <- page %>%
        html_nodes("input[type='hidden']") %>%
        set_names(html_attr(., "name")) %>%
        map_chr(~ html_attr(.x, "value")) %>%
        discard(is.na)

      # Merge additional fields (avoid duplicating the ones we already have)
      post_data <- c(post_data, additional_fields[!names(additional_fields) %in% names(post_data)])

      # Submit the PostBack request
      csv_response <- httr::POST(
        base_url,
        body = post_data,
        encode = "form",
        httr::add_headers(
          "Content-Type" = "application/x-www-form-urlencoded",
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        )
      )

      # Check if we got CSV data
      if (!httr::http_error(csv_response)) {
        content_type <- httr::headers(csv_response)$`content-type`
        csv_content <- httr::content(csv_response, "text", encoding = "UTF-8")

        # Check if response looks like CSV
        if (str_detect(content_type %||% "", "csv|text") || str_detect(csv_content, "^[^<]*,.*\n")) {
          message("Successfully got CSV data for ", county_name, " County")

          # Parse CSV and convert to data.table
          races_data <- readr::read_csv(csv_content, show_col_types = FALSE) %>%
            as.data.table() %>%
            # Clean column names
            setnames(., tolower(gsub("[^A-Za-z0-9]", "_", names(.)))) %>%
            .[, .(district, lead_county, office, incumbent)] %>%
            setnames("lead_county", "county") %>%
            .[, `:=`(district = fcase(district=="County", paste(county_name, "County"),
                                      default=custom_title_case(district)),
                     county = county_name)
            ]

          # Filter based on district and office terms using the actual column names from CSV
          if ("district" %in% colnames(races_data) && "office" %in% colnames(races_data)) {
            filtered_data <- races_data[
              str_detect(tolower(district), paste0(DISTRICT_TERMS, collapse = "|")) &
                str_detect(tolower(office), paste0(OFFICE_TERMS, collapse = "|")) &
                !str_detect(tolower(district), "district")
            ] %>%
            # Add county information
            .[, `:=`(district = fcase(grepl("^County", district), paste(county_name, "County"),
                                      default=custom_title_case(district)),
                     county = county_name)
            ]
          } else {
            # If column names don't match expectations, return all data
            message("Expected columns 'district' and 'office' not found for ", county_name, ". Returning all data.")
            filtered_data <- races_data
          }

          message("Found ", nrow(filtered_data), " scheduled races for ", county_name, " County")
          return(filtered_data)
        }
      }
    }

    # Return empty data.table if CSV export failed
    message("CSV export failed for ", county_name, " County")
    return(data.table())

  }, error = function(e) {
    message("Error processing ", county_name, " County: ", e$message)
    return(data.table())
  })
}

# Main vectorized function
scrape_scheduled_races <- function(year = 2025) {

  message("Scraping scheduled races for year: ", year, " across all counties")

  all_races <- data.table()
  failed_counties <- character()

  # Process each county
  for (county_name in names(COUNTY_CODES)) {
    county_code <- COUNTY_CODES[[county_name]]

    county_data <- scrape_single_county_races(year, county_name, county_code)

    if (nrow(county_data) > 0) {
      all_races <- rbindlist(list(all_races, county_data), fill = TRUE)
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
      url <- paste0("https://voter.votewa.gov/ScheduledRaces.aspx?y=", year, "&c=", county_code)
      message("   ", county, ": ", url)
    }
    message(strrep("=", 60))
  }

  message("Total scheduled races collected: ", nrow(all_races))

  # Only sort if we have data
  if (nrow(all_races) > 0) {
    # Remove duplicates and sort using proper column references
    all_races <- all_races %>%
      unique() %>%
      setorder(county, district, office)
  }

  return(all_races)
}
