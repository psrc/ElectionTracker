library(rvest)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# Function 4: Scrape PSRC board members
scrape_psrc_boards <- function() {

  message("Scraping PSRC board members")

  # Define board URLs and abbreviations
  boards <- data.frame(
    url = c(
      "https://www.psrc.org/board/executive-board",
      "https://www.psrc.org/board/operations-committee",
      "https://www.psrc.org/board/economic-development-district-board",
      "https://www.psrc.org/board/growth-management-policy-board",
      "https://www.psrc.org/board/transportation-policy-board"
    ),
    abbrev = c("EB", "OC", "EDD", "GMPB", "TPB"),
    stringsAsFactors = FALSE
  )

  target_titles <- c("Mayor Pro Tem", "Mayor", "Councilmember", "Commissioner", "Executive")

  all_board_members <- map_dfr(1:nrow(boards), function(i) {
    board <- boards[i, ]
    message("Processing board: ", board$abbrev)

    tryCatch({
      page <- read_html(board$url)

      # Extract member sections - both Members and Alternates
      member_sections <- page %>%
        html_nodes(".item-list")

      # Process each section (Members and Alternates)
      section_data <- map_dfr(member_sections, function(section) {

        # Get the section title (Members or Alternates)
        section_title <- section %>%
          html_node("h3") %>%
          html_text(trim = TRUE)

        if (is.na(section_title) || !section_title %in% c("Members", "Alternates")) {
          return(tibble())
        }

        role <- ifelse(section_title == "Members", "Member", "Alternate")

        # Extract all list items
        list_items <- section %>%
          html_nodes("li")

        if (length(list_items) == 0) {
          return(tibble())
        }

        # Extract data from each list item
        member_data <- map_dfr(list_items, function(item) {

          # Get the title and name from the h5 a element
          title_name <- item %>%
            html_node("h5 a") %>%
            html_text(trim = TRUE)

          if (is.na(title_name)) {
            return(tibble())
          }

          # Extract office and name using regex for target titles
          title_pattern <- paste0("^(", paste(target_titles, collapse = "|"), ")\\s+(.+)$")
          title_match <- str_match(title_name, title_pattern)

          if (is.na(title_match[1])) {
            return(tibble())
          }

          office <- title_match[2]
          full_name <- str_trim(title_match[3])

          # Split name into first and last
          name_parts <- str_split(full_name, "\\s+")[[1]]
          if (length(name_parts) >= 2) {
            first_name <- name_parts[1]
            last_name <- paste(name_parts[2:length(name_parts)], collapse = " ")
          } else {
            first_name <- full_name
            last_name <- ""
          }

          # Get district - corrected selector
          district <- item %>%
            html_node(".views-field-field-type-2 .field-content div") %>%
            html_text(trim = TRUE)

          if (is.na(district)) {
            district <- ""
          }

          tibble(
            office = office,
            first_name = first_name,
            last_name = last_name,
            district = district,
            role = role,
            board_affiliation = board$abbrev
          )
        })

        return(member_data)
      })

      return(section_data)

    }, error = function(e) {
      message("Error processing board ", board$abbrev, ": ", e$message)
      return(tibble())
    })

    # Add delay
    Sys.sleep(1)
  })

  # Remove any potential duplicates and empty rows
  all_board_members <- all_board_members %>%
    filter(nzchar(first_name)) %>%
    distinct(first_name, last_name, board_affiliation, .keep_all = TRUE)

  message("Found ", nrow(all_board_members), " PSRC board members")
  return(all_board_members)
}
