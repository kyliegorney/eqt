#' Compare object components
#' 
#' @param ... One or more objects.
#' @param which Character indicating the component of interest.
#' 
#' @noRd

all_identical <- function(..., which) {
  object <- list_dots(...)
  all(vapply(object[-1], function(x) {
    identical(x[[which]], object[[1]][[which]])
  }, logical(1L)))
}

#' Get sample scores
#' 
#' @noRd

get_scores <- function(raw_scores, scales) {
  scales[match(raw_scores, scales[, "raw"]), ]
}

#' List dots
#' 
#' @param ... Dots passed from another function.
#' 
#' @details
#' If the only object in `...` is a list, then the list is returned. Else, the
#' components of `...` are returned as a list.
#' 
#' @noRd

list_dots <- function(...) {
  if (...length() == 1L && all(class(..1) == "list")) {
    ..1
  } else {
    list(...)
  }
}

#' Remove null values from list
#' 
#' @param x A list.
#' 
#' @noRd 

remove_null <- function(x) {
  x[lengths(x) > 0L]
}
