# Internal helper: replace NA with blank string in a data.frame before printing.
# na.print = "" alone fails for character-coerced NA and <NA> strings.
blank_na <- function(df) {
  df[] <- lapply(df, function(col) {
    col <- as.character(col)
    col[col == "NA"]   <- ""
    col[col == "<NA>"] <- ""
    col
  })
  df
}
