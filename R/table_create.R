shinyInput_col <- function(FUN, len, id, ...) {
  
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
  
}
shinyInput_row <- function(FUN, i, id, ...) {
  inputs <- as.character(FUN(paste0(id, i), ...))
}
