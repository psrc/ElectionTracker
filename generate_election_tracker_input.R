library(data.table)
library(stringr)
library(openxlsx)
library(magrittr)
library(purrr)

# Helper function for fuzzy name matching
# Helper function for name matching with middle names/initials and suffixes
find_best_name_match <- function(psrc_name, candidate_names, threshold = 0.8) {
  if (length(candidate_names) == 0) return(NA_character_)

  # Helper function to normalize names for comparison
  normalize_name <- function(name) {
    # Convert to lowercase and trim whitespace
    name <- str_trim(tolower(name))

    # Remove suffixes (Sr., Sr, Jr., Jr)
    name <- str_remove(name, "\\s*,?\\s*(sr\\.?|jr\\.?)\\s*$")

    # Split into parts
    parts <- str_split(name, "\\s+")[[1]]

    if (length(parts) >= 2) {
      # Extract first and last name (assuming last name is the final part)
      first_name <- parts[1]
      last_name <- parts[length(parts)]

      # Extract middle names/initials (everything between first and last)
      middle_parts <- if (length(parts) > 2) parts[2:(length(parts)-1)] else character(0)

      # Convert middle names to initials only (take first character)
      middle_initials <- str_sub(middle_parts, 1, 1)

      return(list(
        first = first_name,
        last = last_name,
        middle_initials = middle_initials,
        full_normalized = paste(first_name, last_name)
      ))
    }

    return(list(
      first = name,
      last = "",
      middle_initials = character(0),
      full_normalized = name
    ))
  }

  # Normalize the PSRC name
  psrc_normalized <- normalize_name(psrc_name)

  # Find matches
  matches <- map_dfr(seq_along(candidate_names), function(i) {
    candidate_normalized <- normalize_name(candidate_names[i])

    # Must have exact match on first and last name
    first_match <- psrc_normalized$first == candidate_normalized$first
    last_match <- psrc_normalized$last == candidate_normalized$last

    if (first_match && last_match) {
      # Calculate a match score
      score <- 1.0  # Base score for first/last name match

      # Check middle name compatibility
      psrc_middles <- psrc_normalized$middle_initials
      candidate_middles <- candidate_normalized$middle_initials

      # Middle names are compatible if:
      # 1. Both have no middle names/initials
      # 2. One is subset of the other (allowing for missing middle names)
      # 3. All present initials match

      if (length(psrc_middles) == 0 && length(candidate_middles) == 0) {
        # Both have no middle names - perfect match
        middle_compatible <- TRUE
      } else if (length(psrc_middles) == 0 || length(candidate_middles) == 0) {
        # One has middle names, one doesn't - still compatible
        middle_compatible <- TRUE
      } else {
        # Both have middle names - check if they're compatible
        # Compatible if all overlapping positions match
        min_length <- min(length(psrc_middles), length(candidate_middles))
        middle_compatible <- all(psrc_middles[1:min_length] == candidate_middles[1:min_length])
      }

      if (middle_compatible) {
        return(data.frame(
          index = i,
          candidate = candidate_names[i],
          score = score,
          stringsAsFactors = FALSE
        ))
      }
    }

    return(data.frame(
      index = integer(0),
      candidate = character(0),
      score = numeric(0),
      stringsAsFactors = FALSE
    ))
  })

  # Return best match if any found
  if (nrow(matches) > 0) {
    best_match <- matches[which.max(matches$score), ]
    return(best_match$candidate)
  }

  # No fallback fuzzy matching - return NA if no exact first/last match found
  return(NA_character_)
}

generate_election_tracker_input <- function(year = NULL, election_code = NULL,
                                            output_file = "election_tracker_input.xlsx") {

  message("Generating election tracker input file...")

  # Load all election data
  election_data <- load_election_data(year, election_code)

  # Get active candidates only
  active_candidates <- election_data$candidate_lists[status == "Active"]

  # Create the election tracker input data
  election_tracker_input <- election_data$psrc_boards %>%
    copy() %>%
    .[, `:=`(
      ballot_name = NA_character_,
      not_seeking_reelection = FALSE,
      not_up_for_reelection = FALSE,
      seeking_new_office = FALSE
    )]

  # Helper function to check if names match (accounting for middle names/suffixes)
  check_name_appears <- function(target_name, name_list) {
    if (length(name_list) == 0 || all(is.na(name_list))) return(FALSE)

    # First try exact match
    if (any(tolower(target_name) == tolower(name_list), na.rm = TRUE)) {
      return(TRUE)
    }

    # Then try our sophisticated matching
    match_result <- find_best_name_match(target_name, name_list[!is.na(name_list)])
    return(!is.na(match_result))
  }

  # For each PSRC board member, determine their election status
  for (i in 1:nrow(election_tracker_input)) {
    psrc_name <- election_tracker_input$psrc_name[i]
    psrc_district <- election_tracker_input$psrc_district[i]  # Updated variable name
    title <- election_tracker_input$title[i]

    # Check if this person appears as an incumbent in scheduled_races
    is_incumbent <- check_name_appears(psrc_name, election_data$scheduled_races$incumbent)

    # Check if this person appears as a candidate (any status)
    appears_as_candidate <- check_name_appears(psrc_name, election_data$candidate_lists$name)

    # Check if this person appears as an ACTIVE candidate
    appears_as_active_candidate <- check_name_appears(psrc_name, active_candidates$name)

    # Logic for not_up_for_reelection:
    # TRUE when there is NO candidate with that name AND NO incumbent with that name
    if (!appears_as_candidate && !is_incumbent) {
      election_tracker_input$not_up_for_reelection[i] <- TRUE
    }

    # Logic for not_seeking_reelection:
    # TRUE when they are listed as incumbent but NOT an active candidate
    if (is_incumbent && !appears_as_active_candidate) {
      election_tracker_input$not_seeking_reelection[i] <- TRUE
    }

    # Only try to find ballot name matches if they're potentially running
    if (!election_tracker_input$not_up_for_reelection[i] &&
        !election_tracker_input$not_seeking_reelection[i]) {

      # PASS 1: Try exact match first (most efficient)
      exact_match <- active_candidates[tolower(name) == tolower(psrc_name)]

      if (nrow(exact_match) > 0) {
        election_tracker_input$ballot_name[i] <- exact_match$name[1]
      } else {
        # PASS 2: Broader search with sophisticated matching

        # ADD DEBUGGING HERE (only for broader search):
        cat("\n=== Debug for:", psrc_name, "===\n")
        cat("PSRC District:", psrc_district, "\n")  # Updated variable name
        cat("Title:", title, "\n")

        # Look for relevant candidates in their district/race
        relevant_candidates <- active_candidates[
          str_detect(tolower(paste(psrc_district, race)), tolower(psrc_district)) |
            str_detect(tolower(race), tolower(gsub("member|councilmember", "council", title)))
        ]

        if (nrow(relevant_candidates) > 0) {
          # Try to find best name match
          best_match <- find_best_name_match(psrc_name, relevant_candidates$name)
          cat("Best match found:", best_match, "\n")
          election_tracker_input$ballot_name[i] <- best_match
        }

        # If still no match found AND they're an incumbent, mark as not seeking reelection
        if (is.na(election_tracker_input$ballot_name[i]) && is_incumbent) {
          election_tracker_input$not_seeking_reelection[i] <- TRUE
          cat("No candidate found for incumbent", psrc_name, "- marked as not seeking reelection\n")
        }
      }
    }
  }

  # After all name matching is complete, calculate seeking_new_office
  # for board members who have ballot_name matches
  for (i in 1:nrow(election_tracker_input)) {
    if (!is.na(election_tracker_input$ballot_name[i]) &&
        !election_tracker_input$not_seeking_reelection[i] &&
        !election_tracker_input$not_up_for_reelection[i]) {

      ballot_name <- election_tracker_input$ballot_name[i]

      # Find their candidate record to get district and race
      candidate_record <- active_candidates[name == ballot_name]

      if (nrow(candidate_record) > 0) {
        candidate_district <- candidate_record$district[1]
        candidate_race <- candidate_record$race[1]

        # Look up in scheduled_races to see if they're the incumbent
        scheduled_match <- election_data$scheduled_races[
          district == candidate_district & office == candidate_race
        ]

        if (nrow(scheduled_match) > 0) {
          incumbent_name <- scheduled_match$incumbent[1]
          # seeking_new_office = TRUE when incumbent != ballot_name
          if (!is.na(incumbent_name) && incumbent_name != ballot_name) {
            election_tracker_input$seeking_new_office[i] <- TRUE
          }
        }
      }
    }
  }

  # Create Excel workbook with formatting
  wb <- createWorkbook()
  addWorksheet(wb, "PSRC_Election_Tracker")

  # Write data
  writeData(wb, "PSRC_Election_Tracker", election_tracker_input)

  # Add some basic formatting
  headerStyle <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2")
  addStyle(wb, "PSRC_Election_Tracker", headerStyle, rows = 1, cols = 1:ncol(election_tracker_input))

  # Create styles for checkboxes
  checkboxStyle <- createStyle(halign = "center")

  # Apply checkbox styling to the boolean columns
  not_seeking_col <- which(names(election_tracker_input) == "not_seeking_reelection")
  not_up_col <- which(names(election_tracker_input) == "not_up_for_reelection")
  seeking_new_col <- which(names(election_tracker_input) == "seeking_new_office")

  # Apply styling to each checkbox column separately
  addStyle(wb, "PSRC_Election_Tracker", checkboxStyle,
           rows = 2:(nrow(election_tracker_input) + 1), cols = not_seeking_col)

  addStyle(wb, "PSRC_Election_Tracker", checkboxStyle,
           rows = 2:(nrow(election_tracker_input) + 1), cols = not_up_col)

  addStyle(wb, "PSRC_Election_Tracker", checkboxStyle,
           rows = 2:(nrow(election_tracker_input) + 1), cols = seeking_new_col)

  # Set column widths
  setColWidths(wb, "PSRC_Election_Tracker", cols = 1:ncol(election_tracker_input),
               widths = c(15, 25, 20, 20, 25, 20, 20, 18))

  # Save file
  saveWorkbook(wb, output_file, overwrite = TRUE)

  message("Election tracker input file saved as: ", output_file)
  message("Please review and correct the ballot_name matches and checkbox fields.")
  message("Total PSRC board members: ", nrow(election_tracker_input))
  message("Automatically matched names: ", sum(!is.na(election_tracker_input$ballot_name)))
  message("Flagged as not up for reelection: ", sum(election_tracker_input$not_up_for_reelection))
  message("Flagged as not seeking reelection: ", sum(election_tracker_input$not_seeking_reelection))

  return(election_tracker_input)
}
