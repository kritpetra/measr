
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

  structure(list(val = val, err = abs(err)), class = "measr")
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

#' Is an Object a Measured Value?
#' 
#' Checks whether the object is of \code{measr} type.
#' 
#' @param x The object to be tested
#' @return \code{TRUE} if the object is a \code{measr} object. Otherwise, 
#'     returns \code{FALSE}.
is.measr <- function(x) {
  inherits(x, "measr")
}

# Combines the measr objects into a list.
# Note: for this method to be dispatched, the first argument must be a measr
# object.
c.measr <- function(...) {
  x <- lapply(list(...), as.measr)
  structure(list(val = unlist(lapply(x, `[[`, "val")),
                 err = unlist(lapply(x, `[[`, "err"))),
            class = "measr")
}

#' Coerce to Measurement
#'
#' \code{as.measr} will attempt to convert the specified object to a  
#' \code{measr} object.
#' 
#' @param x The object to be coerced
#' @return A \code{measr} object, or \code{NA} if it cannot be coerced.
#' 
as.measr <- function(x) {
  if (!is.measr(x)) {
    measr(as.numeric(x), err = 0)
  } else x
}
