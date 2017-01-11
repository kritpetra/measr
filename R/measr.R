
#' Create a \code{measr} Object
#'
#' Creates a \code{measr} object with the given value and standard deviation.
#'
#' @param val The measured value
#' @param err  The standard deviation of the measured value. This is usually
#' the measurement error
#' @return
#' A \code{measr} object with the given value and error. If one of the
#' parameters given cannot be coerced to a numeric object via
#' \code{\link{as.numeric}}, then \code{NA}s can be introduced in the return
#' value.
measr <- function(val, err) {

  if (!is.numeric(val) || !is.numeric(err)) {
    val <- as.numeric(val)
    err <- as.numeric(err)
  }

  structure(list(val = val, err = err), class = "measr")
}

#  Alias for measr()
#' @rdname measr
`%pm%` <- function(val, err) {
  measr(val, err)
}

# Print the measr object as val±err
print.measr <- function(x) {
  mapply(cat, x$val, "±", x$err, "\n")
}

# If using first implementation, we need the length to return the amount of values,
# not 2. !!! Confuses str() since it depends on length()
length.measr <- function(x) {
  length(x$val)
}

# Combines the measr objects into a list.
c.measr <- function(...) {
  structure(list(val = unlist(lapply(list(...), `[[`, "val")),
                 err = unlist(lapply(list(...), `[[`, "err"))),
            class = "measr")
}

#' Is an Object a Measured Value?
#' Checks whether all the objects are of 'measr' type. Returns a logical vector.
is.measr <- function(x) {
  inherits(x, "measr")
}

#' Coerce to Measurement
#'
#' Coerce an R object into a
as.measr <- function(x) {
  stopifnot(is.numeric(x) || is.measr(x))

  if (!is.measr(x) && is.numeric(x)) {
    measr(x, err = 0)
  } else x
}
