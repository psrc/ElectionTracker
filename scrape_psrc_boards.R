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
    url = paste0("https://www.psrc.org/board/",
                 c("executive-board",
                   "operations-committee",
                   "economic-development-district-board",
                   "growth-management-policy-board",
                   "transportation-policy-board")),
    abbrev = c("EB", "OC", "EDD", "GMPB", "TPB")
  )

  target_titles <- c("Deputy Mayor","Mayor Pro Tem", "Mayor", "Councilmember", "Council President",
                     "Commissioner", "Executive", "Representative", "Senator")

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
          psrc_name <- str_trim(title_match[3])

          # Get district - corrected selector
          psrc_district <- item %>%
            html_node(".views-field-field-type-2 .field-content div") %>%
            html_text(trim = TRUE) %>% custom_title_case()

          if (is.na(psrc_district)) {
            psrc_district <- ""
          }

          data.table(
            title = title,
            psrc_name = psrc_name,
            psrc_district = psrc_district,
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
  all_board_members <- all_board_members[nzchar(psrc_name)] %>%
    unique(by = c("psrc_name", "board_affiliation"))

  board_members <- all_board_members[, .(board_affiliation = paste(board_affiliation, collapse = ", ")),
                by = .(title, psrc_name, psrc_district)]

  message("Found ", nrow(all_board_members), " PSRC board members")
  return(board_members)
}
