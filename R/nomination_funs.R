# External Dependencies:
#
# tidyverse
#------------------------------------------------


#' import_nominations
#'
#' Import CSV file from Qualtrics.
#'
#' @param path String. Path to Qualtrics CSV file.
#' @param item_text String. Text of Qualtrics item prompt.
#' @param col_name String. Name to be passed to the column of nominees.
#'
#' @return A tibble containing nominees.
#'
#' @export
#'
import_nominations <-
  function(path, item_text = "QID", col_name = "Nominees") {
    # STEP 1:
    # import CSV file from qualtrics

    # read in the CSV (& remove the first row of qualtrics item info)
    readr::read_csv(file = path, skip = 2) %>%
      # keep only the nominee columns (contain item_text)
      dplyr::select(., tidyselect::contains(item_text)) %>%
      # remove item text from column names
      dplyr::rename_with(.fn = ~{col_name})
  }


#' count_nominees
#'
#' @param .data Dataframe/tibble. The data output by
#' \code{\link{import_nominations}}.
#' @param .limit Either \code{NULL} (if no limit desired) or a numeric scalar
#' if output nominations should be limited to only the most frequently nominated
#' nominees. For example, if \code{.limit = 5}, then only the nominees with
#' the top 5 nomination counts will be output. This preserves ties in case
#' several nominees are nominated the same number of times.
#'
#' @inheritParams import_nominations
#'
#' @return A tibble of nominee names and nomination counts.
#'
#' @export
#'
count_nominees <-
  function(.data, .limit = NULL, col_name = "Nominees") {
    # convert data column name to symbol for use with dplyr
    sym_nm <- rlang::sym(col_name)

    # count the frequency of nominations for each nominee
    nom_count <-
      .data %>%
      dplyr::count(., !!sym_nm)

    # keep only the top X number of nominees
    if (!is.null(.limit)) {
      # select the top X unique nomination counts - allows us to preserve ties
      keep_count <-
        nom_count %>%
        dplyr::select(., n) %>%
        unique(.) %>%
        head(., .limit) %>%
        unlist(.)

      # update the frequency of nominations in place
      nom_count <-
        nom_count %>%
        dplyr::slice(., which(n %in% keep_count))
    }

    # output the final tabulation sorted in descenting count order
    nom_count %>%
      dplyr::arrange(., dplyr::desc(n))

  }
