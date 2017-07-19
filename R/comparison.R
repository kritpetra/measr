#' Strict comparison
#'
#' Returns \code{TRUE} if and only if the objects' values and errors are equal.
#'
#' @param e1 First \code{measr} object
#' @param e2 Second \code{measr} object
#' @return
#' Boolean representing if the objects represent two measurements that have the 
#' same mean and measurement error.
`===.measr` = function(e1, e2) {
  return(e1$val == e2$val && e1$err == e2$err)
}

#' @rdname `===.measr`
`!==.measr` = function(e1, e2) {
  return(e1$val != e2$val || e1$err != e2$err)
}


#' Measurement comparison
#'
#' Compares two \code{measr} objects and returns \code{TRUE} if the two have 
#' standard uncertainty ranges that overlap.
#' 
#' @param e1 First \code{measr} object
#' @param e2 Second \code{measr} object
#' @return
#' Whether or not the two uncertainty ranges overlap 
`==.measr` = function(e1, e2) {
  return(e1$val - e1$err <= e2$val + e2$err && e2$val - e2$err <= e1$val + e1$err)
}

#' @rdname `==.measr`
`!=.measr` = function(e1, e2) {
  !(e1 == e2)
}


`<.measr` = function(e1, e2) {
  return(e1$val + e1$err < e2$val - e2$err)  
}

`<=.measr` = function(e1, e2) {
  return(e1 < e2 || e1 == e2)  
}

`>.measr` = function(e1, e2) {
  return(e1$val - e1$err > e2$val + e2$err)
}

`>=.measr` = function(e1, e2) {
  return(e1 > e2 || e1 == e2)  
}