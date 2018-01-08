#' Convert Rmd to ipynb (jupyter notebooks)
#'
#' Converts Rmd file to a jupyter notebook
#'
#' @param x Name of Rmd file
#' @return Saves file with .ipynb extension
#' @export
rmd2jupyter <- function(x) {
  save_as <- gsub("\\.Rmd", ".ipynb", x)
  con <- file(x)
  x <- readLines(con, warn = FALSE)
  close(con)
  ## strip yaml
  if (grepl("^---", x[1])) {
    yaml_end <- grep("^---", x)[2]
    x <- x[-c(1:yaml_end)]
  }
  chunks <- grep("^```", x)
  lns <- unique(sort(c(1, chunks, chunks - 1L, length(x))))
  if (chunks[length(chunks)] == length(x)) lns <- c(lns, length(x))
  chunks <- matrix(lns, ncol = 2, byrow = TRUE)
  chunks <- data.frame(chunks)
  names(chunks) <- c("start", "end")
  codes <- grep("^```", x)
  codes <- codes[seq(1, length(codes), 2)]
  chunks$cell_type <- ifelse(chunks$start %in% codes, "code", "markdown")
  x <- gsub("^```.*", "", x)
  for (i in seq_len(nrow(chunks))) {
    s <- paste0(x[(chunks$start[i]):(chunks$end[i])], "\n")
    if (s[1] == "\n" & length(s) > 2L) s <- s[-1]
    if (s[1] == "\n" & length(s) > 2L) s <- s[-1]
    if (s[length(s)] == "\n" & length(s) > 2L) s <- s[-length(s)]
    if (s[length(s)] == "\n" & length(s) > 2L) s <- s[-length(s)]
    chunks$source[i] <- I(list(s))
    #paste(
    #x[(chunks$start[i]):(chunks$end[i])], collapse = "\n"
    #)
  }
  cells <- Map(format_cell, chunks$cell_type, chunks$source)
  x <- jsonlite::prettify(format_cells(cells))
  x <- gsub("count\": \"\"", "count\": null", x)
  x <- gsub("metadata\": \"\"", "metadata\": {}", x)
  x <- gsub("outputs\": \"\"", "outputs\": []", x)
  cat(x, file = save_as)
  message(paste("file saved as", save_as))
}



format_cell <- function(cell_type,
                        source) {
  if (cell_type == "code") {
    x <- list(cell_type = cell_type,
         execution_count = "",
         metadata = "",
         outputs = "",
         source = source)
  } else {
    x <- list(cell_type = cell_type,
         metadata = "",
         source = source)
  }
  x
}


format_cells <- function(cells) {
  x <- list(cells = unname(cells),
    metadata = list(
      "anaconda-cloud" = "",
      "kernelspec" = list(
        "display_name" = "R",
        "langauge" = "R",
        "name" = "ir"),
      "language_info" = list(
        "codemirror_mode" = "r",
        "file_extension" = ".r",
        "mimetype" = "text/x-r-source",
        "name" = "R",
        "pygments_lexer" = "r",
        "version" = "3.4.1")
    ),
    "nbformat" = 4,
    "nbformat_minor" = 1)
  jsonlite::toJSON(x, auto_unbox = TRUE)
}

