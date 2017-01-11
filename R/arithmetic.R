`+.measr` <- function(x, y) {
  structure(list(val = x$val + y$val,
                 err = sqrt(x$val^2 + y$val^2)),
            class = "measr")
}
`-.measr` <- function(x, y) {
  structure(list(val = x$val - y$val,
                 err = sqrt(x$val^2 + y$val^2)),
            class = "measr")
}
`*.measr` <- function(x, y) {
  structure(list(val = x$val * y$val,
                 err = abs(x$val * y$val) *
                             sqrt((x$err/x$val)^2 + (y$err/y$val)^2)),
            class = "measr")
}
`/.measr` <- function(x, y) {
  structure(list(val = x$val / y$val,
                 err = abs(x$val / y$val) *
                             sqrt((x$err/x$val)^2 + (y$err/y$val)^2)),
            class = "measr")
}
`^.measr` <- function(x, n) {
  #only works if n is exact
  #stopifnot(is.numeric(n) || n$err == 0, is.measr(x))

  structure(list(val = x$val^n,
                 err = abs(x$val^n) * abs(n) * x$err / abs(x$val)),
            class = "measr")

}
