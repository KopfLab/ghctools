#' @keywords internal
"_PACKAGE"

#' @importFrom glue glue collapse
#' @importFrom dplyr select rename mutate data_frame tbl_df matches everything mutate_at filter
#' @importFrom purrr pmap map_chr map_lgl map map2
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_c str_interp str_replace str_detect
#' @import ghql
#' @import httr
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# standard OR function
`%||%` <- function (a, b) {
  if (!is.null(a)) a else b
}

# quiets concerns of R CMD check about . that appears in pipelines 
# and some very commonly used variable names used in NSE commands
utils::globalVariables(c("."))

# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}