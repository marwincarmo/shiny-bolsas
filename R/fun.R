no_filter <- function(input, val) {
  if (is.null(input)) {
    unique(val)
  } else {
    input
  }
}