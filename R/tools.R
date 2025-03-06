#' Remove duplicates xtremas
#'
#' @param data xtrem data tibble with potential duplicates
#' @param last lagical idincating which value (and index)
#' to keep in case of duplicate xtremas
#'
#' @return xtrem data tibble without duplicate xtremas
#'
#' @import zoo
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
remove_xtrem_duplicates <- function(data, last) {
  test_status <- data %>%
    arrange(.data$Idx, .data$Values) %>%
    pull(.data$Status) %>%
    zoo::rollapply(width = 2, partial = TRUE,
                   align ="l",
                   FUN = function(d) length(d) > 1 & d[1] == d[2])

  artefact_idx <- sapply(which(test_status), function(i) {
    if (data$Values[i] == data$Values[i+1]) {
      ifelse(last, i, i+1)
    } else if (data$Values[i] > data$Values[i+1]) {
      ifelse(data$Status[i] == "min", i, i+1)
    } else {
      ifelse(data$Status[i] == "min", i+1, i)
    }
  })

  data %>% arrange(.data$Idx, .data$Values) %>%
    mutate(Artefact = row_number() %in% artefact_idx) %>%
    filter(!.data$Artefact) %>%
    select(-.data$Artefact)
}
