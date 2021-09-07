#' Assign volume esitmation method in to each individuals
#'
#' @param data The data that records observations of each individuals size measurements
#'             with at least the following columns: "Taxon"
#' @param method_file The file that contains the pairwise. If NULL, the method file will use the default
#'                    "biovolume_method" data.
#' @return
#' @export
#'
#' @examples
#' a <- data.frame(Taxon = c("Polychaeta", "Oligochaeta", "Sipuncula", "Not in the column"))
#' assign_method(a)
assign_method <- function(data, method_file = NULL) {

  # initial if_else control flow --------------------------------------------
  if (is.character(data)) {
    if (grepl(".xlsx", data)) { # measurement_file have ".xlsx"
      data <- read_xlsx(data) %>% data.frame()
    }
    if (grepl(".csv", data)) { # measurement_file have ".xlsx"
      data <- read.csv(data) %>% data.frame()
    }
  } else if (is.object(data)) { # measurement_file = object
    data <- data.frame(data)
  } else {
    message("Neither measurement_file nor object")
    stop()
  }

  # assign method -----------------------------------------------------------
  if (is.null(method_file)) {
    method_file <- biovolume_method
  } else if (is.object(method_file)) {
    method_file <- method_file
  } else {
    message("method file is neither NULL or an object")
    stop()
  }

  # separate simple cases and exceptional cases
  simple <- method_file[is.na(method_file$Note), ] %>% select(Taxon, Method, C)
  exceptional <- method_file[!is.na(method_file$Note), ] %>% select(Taxon, Note, Method, C)

  # merging data
  result_simple <-
    data %>%
    filter(Taxon %in% simple$Taxon) %>%
    left_join(simple)

  result_exceptional <-
    data %>%
    filter(Taxon %in% exceptional$Taxon) %>%
    left_join(exceptional)

  # assign conversion factors for organisms that uses LWR
  full_join(result_exceptional, result_simple)
}
