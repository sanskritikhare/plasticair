#' Loads the Break Free From Plastic dataset.
#'
#' @return A tibble containing plastic waste data.
#' @importFrom readr read_csv
#' @export
#'
#' @examples
#' load_data()
load_data <- function() {

  readr::read_csv(
    "https://github.com/rfordatascience/tidytuesday/tree/f52bf606b830a0fdec1ef50780834558b77c3b59/data/2021/2021-01-26"
  )

}
