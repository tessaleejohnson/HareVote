# External Dependencies:
#
# tidyverse
#------------------------------------------------


#' import_data
#'
#' Import CSV file from Qualtrics.
#'
#' @param path String. Path to Qualtrics CSV file.
#' @param item_text String. Text of Qualtrics item prompt that should be
#' deleted.
#'
#' @return A tibble containing nominee rankings: nominees in columns; rankings
#' in rows.
#'
#' @export
#'
import_data <-
  function(
    path,
    item_text = paste0(
      "Rank the nominees to vote for SMEP President as follows:\n1 = ",
      "first choice...5 = last choice\nStart by clicking or tapping on a ",
      "name, and a number indicating its rank will appear to the left. ",
      "Rearrange the names by dragging them in your preferred rank order. - "
    )
  ) {
    # STEP 1:
    # import CSV file from qualtrics

    # read in the CSV (& remove the first row of qualtrics item info)
    readr::read_csv(file = path, skip = 1) %>%
      # keep only the nominee columns (contain item_text)
      dplyr::select(., tidyselect::contains(item_text)) %>%
      # remove item text from column names
      dplyr::rename_with(.fn = ~{stringr::str_remove(.x, item_text)}) %>%
      # delete row of qualtrics item option IDs
      dplyr::slice(., -1) %>%
      # convert data to numeric
      dplyr::mutate(., dplyr::across(where(is.character), as.numeric))
  }


#' tabulate_results
#'
#' Tabulate proportion of first-choice votes for each nominee.
#'
#' @param .data Dataframe or tibble. Nominees in columns; rankings in rows.
#'
#' @return A tibble with two columns: \code{name}, containing the name of the
#' nominee, and \code{pct}, containing the proportion of first-choice votes
#' the corresponding nominee received.
#'
#' @export
#'
tabulate_results <-
  function(.data) {
    # STEP 2:
    # tabulate proportion of first-choice votes for each nominee

    purrr::imap_dfr(.data, ~{
      # calculate percentage of first-choice votes for each nominee
      pct <- sum(.x == 1, na.rm = TRUE) / length(.x)
      # output results
      list(name = .y, pct = pct)
    })
  }


#' check_proportion
#'
#' Check the maximum proportion of first-choice votes for remaining nominees.
#'
#' @param .results Dataframe or tibble. Output from
#' \code{\link{tabulate_results}}.
#' @param .name String. The column name containing nominees' names.
#' @param .pct String. The column name containing nominees' first-choice vote
#' proportions.
#'
#' @return A numeric scalar. The maximum first-choice vote proportion among
#' nominees.
#'
#' @export
#'
check_proportion <-
  function(.results, .name = "name", .pct = "pct") {
    # STEP 2a:
    # check the maximum proportion of first-choice votes for remaining nominees

    # convert column names to symbols for use in dplyr functions
    sym_nm <- rlang::sym(.name)
    sym_pct <- rlang::sym(.pct)

    # keep only the maximum vote percentage
    .results %>%
      dplyr::filter(., !!sym_pct == max(!!sym_pct)) %>%
      dplyr::select(., !!sym_pct) %>%
      unlist(.) %>%
      max(.)
  }


#' print_max_results
#'
#' Find the winner and print the announcement.
#'
#' @param .majority A numeric scalar between 0 and 1. The maximum proportion of
#' first-choice votes among nominees.
#'
#' @inheritParams check_proportion
#'
#' @return A string. An announcement of the nominee earning the maximum
#' proportion of first-choice votes.
#'
#' @export
#'
find_max_result <-
  function(.results, .majority, .name = "name", .pct = "pct") {
    # STEP 2b:
    # find the winner and print the announcement

    # convert column names to symbols for use in dplyr functions
    sym_nm <- rlang::sym(.name)
    sym_pct <- rlang::sym(.pct)

    # find the maximum
    .results %>%
      dplyr::filter(., !!sym_pct == .majority) %>%
      dplyr::mutate(
        .data = .,
        !!sym_pct := paste0(round(!!sym_pct, 2) * 100, "%"))
  }


#' detect_lowest
#'
#' Find the nominee with the lowest number of first-choice votes (input is
#' output of \code{\link{tabulate_results}} function).
#'
#' @inheritParams check_proportion
#'
#' @return A string. The name of the nominee with the lowest number of
#' first-choice votes.
#'
#' @export
#'
detect_lowest <-
  function(.results, .name = "name", .pct = "pct") {
    # STEP 3:
    # find the nominee with the lowest number of first-choice votes (input is
    # output of `tabulate_results` function)

    # convert column names to symbols for use in dplyr functions
    sym_nm <- rlang::sym(.name)
    sym_pct <- rlang::sym(.pct)

    .results %>%
      # keep only the nominees with the least number of first-choice votes
      dplyr::filter(
        .data = .,
        !!sym_pct == min(!!sym_pct)
      ) %>%
      # select only the nominees' names
      dplyr::select(., !!sym_nm) %>%
      # convert to unnamed vector
      unlist(.) %>%
      unname(.)
  }


#' redistribute_votes
#'
#' If no simple majority exists, remove the nominee with the least first
#' votes & redistribute among remaining nominees.
#'
#' @param .remove A string. The name of the nominee with the lowest number of
#' first-choice votes.
#'
#' @inheritParams tabulate_results
#'
#' @return A tibble containing updated nominee rankings: nominees in columns;
#' rankings in rows.
#'
#' @export
#'
redistribute_votes <-
  function(.data, .remove) {
    # STEP 4:
    # if no simple majority exists, remove the nominee with the least first
    # votes & redistribute among remaining nominees

    # store the number of nominees with the least first-choice votes
    l <- length(.remove)

    # loop over the number of least first-choice nominees
    for (i in seq_len(l)) {
      # convert nominee name to symbol for use in dplyr function
      rmv_sym <- rlang::sym(.remove[i])

      # update the ballot data in place
      .data <- .data %>%
        # evaluate each ballot...
        dplyr::rowwise(.) %>%
        dplyr::mutate(
          .data = .,
          dplyr::across(
            .cols = -tidyselect::all_of(.remove[i]),
            # up-rank nominees ranked below global lowest first-choice nominee
            .fns = ~{ifelse(.x > !!rmv_sym, .x - 1, .x)}
          )
        ) %>%
        # remove rowwise/ballot-wise grouping
        dplyr::ungroup(.) %>%
        # remove global lowest first-choice nominee
        dplyr::select(., -tidyselect::all_of(.remove[i]))
    }
    # output updated ballots
    .data
  }


#' identify_winner
#'
#' Iterate over steps 2-4 until a nominee has a 50% + 1 majority of first-choice
#' votes.
#'
#' @inheritParams tabulate_results
#' @inheritParams check_proportion
#'
#' @return A list of string elements - the announcement(s) printed by
#' \code{\link{print_max_result}}.
#'
#' @export
#'
identify_winner <-
  function(.data, .name = "name", .pct = "pct") {
    # STEP 5:
    # iterate over steps 2-4 until a nominee has a 50% + 1 majority

    # convert column names to symbols for use in dplyr functions
    sym_nm <- rlang::sym(.name)
    sym_pct <- rlang::sym(.pct)

    # step 2 - tabulate initial results
    results <- .data %>% tabulate_results(.)

    # step 2a - find the maximum vote percentage
    maj <- results %>% check_proportion(.)

    # step 2b - check if there's a winner
    if (maj > 0.5) {

      # print the winner
      winner <-
        list(
          paste0(
            "Iteration = 1", "; ", find_max_result(results, maj)[, 1], " wins!"
          )
        )

    } else {

      # set index - start with 2 because we are now updating original ballot
      i <- 2

      # create storage container for winner progress
      winner <- list("Iteration = 1; Simple majority not achieved.")

      # step 2c - if there's no winner, iterate over steps 3, 4, 2
      while (maj <= 0.5) {

        # step 3 - find nominee with lowest first-choice votes
        remove <- results %>% detect_lowest(.)

        winner[[i]] <- paste0("Iteration = ", i, "; ", remove, " removed.")

        # step 4 - update ballots in place
        .data <-
          .data %>%
          redistribute_votes(., remove)

        # step 2 - tabulate new results
        results <- .data %>% tabulate_results(.)

        # step 2a - find the maximum vote percentage
        maj <- results %>% check_proportion(.)

        # update index i
        i <- i + 1

      }

      winner[[i]] <-
        paste0(
          "Iteration = ", i, "; ", find_max_result(results, maj)[, 1], " wins!"
        )

      # output results
      winner

    }

  }
