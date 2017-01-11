# Error propagation for common mathematical operators

`+.measr` <- function(x, y) {
  x <- as.measr(x)
  y <- as.measr(y)
  
  structure(list(val = x$val + y$val,
                 err = sqrt(x$err^2 + y$err^2)),
            class = "measr")
}

`-.measr` <- function(x, y) {
  x <- as.measr(x)
  y <- as.measr(y)
  
  structure(list(val = x$val - y$val,
                 err = sqrt(x$err^2 + y$err^2)),
            class = "measr")
}

`*.measr` <- function(x, y) {
  x <- as.measr(x)
  y <- as.measr(y)
  
  structure(list(val = x$val * y$val,
                 err = abs(x$val * y$val) *
                   sqrt((x$err/x$val)^2 + (y$err/y$val)^2)),
            class = "measr")
}

`/.measr` <- function(x, y) {
  x <- as.measr(x)
  y <- as.measr(y)
  
  structure(list(val = x$val / y$val,
                 err = abs(x$val / y$val) *
                   sqrt((x$err/x$val)^2 + (y$err/y$val)^2)),
            class = "measr")
}

`^.measr` <- function(x, n) {
  x <- as.measr(x)
  n <- as.measr(n)
  
  if (n$err == 0) {
    
    # Simple case if n is exactly known
    structure(list(val = x$val^n$val,
                   err = abs(x$val^n$val*n$val*x$err/x$val)),
              class = "measr")
    
  } else if (x$err == 0) {
    
    # If the base x is exactly known
    structure(list(val = x$val^n$val,
                   err = abs(x$val^n$val*log(x$val)*n$err)),
              class = "measr")
  } else {
    
    # General case
    structure(list(val = x$val^n$val,
                   err = abs(x$val^n$val) * 
                     sqrt((n$val*x$err/x$val)^2 + (log(x$val)*n$err)^2)), 
              class = "measr")
  }
  
}

log.measr <- function(x, base = exp(1)) {
  
  # Meant to be used when base is exactly known
  structure(list(val = log(x$val, base),
                 err = abs(x$err/x$val/log(base))), 
            class = "measr")
}

log10.measr <- function(x) {
  structure(list(val = log10(x$val),
                 err = abs(x$err/x$val/log(10))), 
            class = "measr")
}

exp.measr <- function(x) {
  structure(list(val = exp(x$val),
                 err = abs(exp(x$val)*x$err)),
            class = "measr")
}

sin.measr <- function(x) {
  structure(list(val = sin(x$val),
                 err = abs(cos(x$val)*x$err)),
            class = "measr")
}

cos.measr <- function(x) {
  structure(list(val = cos(x$val),
                 err = abs(sin(x$val)*x$err)),
            class = "measr")
}