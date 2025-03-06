#' Locate xtremas in a series
#'
#' @param x a series of numeric data
#' @param last logical value indicating which value (and index) to keep in case of duplicate xtremas,
#' @return
#' A tibble of Min and Max indexes and their corresponding values
#'
#' @import dplyr
#' @importFrom futile.logger flog.trace flog.debug
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
locate_xtrem <- function (x, last = FALSE)
{
  # use rle to deal with duplicates
  x_rle <- rle(x)

  # force the first value to be identified as an extrema
  first_value <- x_rle$values[1] - x_rle$values[2]
  flog.trace("First value was identified as an extrema")

  # differentiate the series, keep only the sign, and use 'rle' function to
  # locate increase or decrease concerning multiple successive values.
  # The result values is a series of (only) -1 and 1.
  #
  # ! NOTE: with this method, last value will be considered as an extrema
  diff_sign_rle <- c(first_value, diff(x_rle$values)) %>% sign() %>% rle()

  # this vector will be used to get the initial positions
  diff_idx <- cumsum(diff_sign_rle$lengths)

  # find min and max
  diff_min <- diff_idx[diff_sign_rle$values < 0]
  diff_max <- diff_idx[diff_sign_rle$values > 0]

  # get the min and max indexes in the original series
  x_idx <- cumsum(x_rle$lengths)
  if (last) {
    flog.debug("In case of duplicates, only last indexes will be returned")
    min <- x_idx[diff_min]
    max <- x_idx[diff_max]
  } else {
    flog.debug("In case of duplicates, only first indexes will be returned")
    min <- x_idx[diff_min] - x_rle$lengths[diff_min] + 1
    max <- x_idx[diff_max] - x_rle$lengths[diff_max] + 1
  }
  # just get number of occurences
  min_nb <- x_rle$lengths[diff_min]
  max_nb <- x_rle$lengths[diff_max]

  # format the result as a tibble
  flog.trace("Returning the final result as a tibble object")
  bind_rows(
    tibble(Idx = min, Values = x[min], NB = min_nb, Status = "min"),
    tibble(Idx = max, Values = x[max], NB = max_nb, Status = "max")) %>%
    arrange(.data$Idx) %>%
    mutate(Last = last) %>%
    mutate_at(vars(.data$Idx, .data$NB), as.integer)
}


#' Remove noise after xtremas detection
#'
#' @param xtrem_data a tibble containing xtrem data
#' @param last a logical indicating whether the function should keep the first or last index in case of duplicate xtrem
#' @param thresh numeric value in [0;1] representing threshold to detect artefacts
#' @param ... additional parameters used only in recursive calls
#'
#' @return a clean xtrem data tibble without artefacts
#'
#' @import dplyr
#' @importFrom futile.logger flog.trace flog.debug
#' @importFrom magrittr %>%
#'
#' @export
remove_noise <- function(xtrem_data, last = FALSE, thresh = .05, ...) {
  dots <- list(...)
  if (!length(dots)) {
    flog.trace("First call to the function 'remove_noise()'")
    # xtrem_data <- xtrem_data %>%
    #   mutate(Noise = detect_noise(Values, thresh)) %>%
    #   filter(!Noise) %>%
    #   select(-Noise) %>%
    #   remove_xtrem_duplicates(last)
    i <- 1; n <- nrow(xtrem_data)
  } else {
    flog.trace("Recursive call to the function 'remove_noise()'")
    i <- dots[[1]]; n <- dots[[2]]
  }
  if (i == nrow(xtrem_data)) {
    # last row -> exit
    flog.debug("'remove_noise()' ended after deleting %i rows",
              n - nrow(xtrem_data))
    return(xtrem_data)
  } else {
    d <- abs(xtrem_data$Values[i] - xtrem_data$Values[i+1])
    if (d < thresh) {
      # if the next xtrema value is above the threshold:
      # 1. remove it
      # 2. remove duplicates xtremas (between row[i] and row[i+2])
      xtrem_data <- xtrem_data %>%
        slice(-(i+1)) %>%
        remove_xtrem_duplicates(last)
      flog.trace("Row %i was removed !", i+1)
      # let i as is, since the row has probably changed
    } else {
      flog.trace("Nothing to do for index: %i", i)
      i <- i+1
    }
    # recursive call
    return(remove_noise(xtrem_data, last, thresh, i, n))
  }
}


detect_noise <- function(vect, thresh) {
  test <- function(x) {
    abs(x[1] - x[2]) < thresh & abs(x[2] - x[3]) < thresh
  }
  zoo::rollapply(vect, width = 3, fill = FALSE, FUN = test)
}
