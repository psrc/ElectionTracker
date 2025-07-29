library(rvest)
library(data.table)
library(purrr)
library(stringr)
library(magrittr)

# Function 4: Scrape PSRC board members
scrape_psrc_boards <- function() {

  message("Scraping PSRC board members")

  # Define board URLs and abbreviations
  boards <- data.table(
    url = c(
      "https://www.psrc.org/board/executive-board",
      "https://www.psrc.org/board/operations-committee",
      "https://www.psrc.org/board/economic-development-district-board",
      "https://www.psrc.org/board/growth-management-policy-board",
      "https://www.psrc.org/board/transportation-policy-board"
    ),
    abbrev = c("EB", "OC", "EDD", "GMPB", "TPB")
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
          return(data.table())
        }

        # Extract all list items
        list_items <- section %>%
          html_nodes("li")

        if (length(list_items) == 0) {
          return(data.table())
        }

        # Extract data from each list item
        member_data <- map_dfr(list_items, function(item) {

          # Get the title and name from the h5 a element
          title_name <- item %>%
            html_node("h5 a") %>%
            html_text(trim = TRUE)

          if (is.na(title_name)) {
            return(data.table())
          }

          # Extract title and name using regex for target titles
          title_pattern <- paste0("^(", paste(target_titles, collapse = "|"), ")\\s+(.+)$")
          title_match <- str_match(title_name, title_pattern)

          if (is.na(title_match[1])) {
            return(data.table())
          }

          title <- title_match[2]
          full_name <- str_trim(title_match[3])

          # Get district - corrected selector
          district <- item %>%
            html_node(".views-field-field-type-2 .field-content div") %>%
            html_text(trim = TRUE) %>% custom_title_case()

          if (is.na(district)) {
            district <- ""
          }

          data.table(
            title = title,
            full_name = full_name,
            district = district,
            board_affiliation = board$abbrev
          )
        })

        return(member_data)
      })

      return(section_data)

    }, error = function(e) {
      message("Error processing board ", board$abbrev, ": ", e$message)
      return(data.table())
    })

    # Add delay
    Sys.sleep(1)
  }) %>%
    as.data.table()

  # Remove any potential duplicates and empty rows
  all_board_members <- all_board_members[nzchar(full_name)] %>%
    unique(by = c("full_name", "board_affiliation"))

  board_members <- all_board_members[, .(board_affiliation = paste(board_affiliation, collapse = ", ")),
                by = .(title, full_name, district)]

  message("Found ", nrow(all_board_members), " PSRC board members")
  return(board_members)
}
