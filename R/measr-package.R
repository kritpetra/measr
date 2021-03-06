#' measr - Error propagation for measured values
#'
#' The \code{measr} package provides objects and functions for one to easily
#' work with measurements errors and propagation of those errors, common to
#' research in the physical sciences. It introduces a special type of a numeric
#' object, \code{measr}, which stores both the value and its uncertainty.
#' Errors are automatically propagated to the results of arithmetic operations.
#'
#' @examples
#' x <- measr(27.30, 0.5)
#' y <- measr(12.36, 0.1)
#' x * y
#'
#' @docType package
#' @name measr-package
NULL
