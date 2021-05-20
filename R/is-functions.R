#' Is an object of a particular class?
#' 
#' @description Test whether an object is of a particular class.
#' 
#' @param ... One or more objects to be tested.
#' 
#' @inherit eqt author
#' 
#' @seealso [boot()], [eqt()], [multipleqt()]
#' 
#' @export

is.eqt <- function(...) {
  object <- list_dots(...)
  vapply(object, inherits, logical(1L), what = "eqt")
}

#' @rdname is.eqt
#' @export
is.multipleqt <- function(...) {
  object <- list_dots(...)
  vapply(object, inherits, logical(1L), what = "multipleqt")
}

#' @rdname is.eqt
#' @export
is.boot <- function(...) {
  object <- list_dots(...)
  vapply(object, inherits, logical(1L), what = "boot")
}
