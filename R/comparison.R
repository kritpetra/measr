`==.measr` = function(e1, e2) {
  return(e1$val == e2$val && e1$err == e2$err)
}

`!=.measr` = function(e1, e2) {
  return(e1$val != e2$val || e1$err != e2$err)
}

# Checks if the values are equal with `conf`% confidence, using a two-sample t-test
`%==%.measr` = function(e1, e2, conf = 0.95) {
  # TODO
}