match_text <- function(text, to_be_matched_against = c("char", "text", "int", "date", "float", "numeric", "real", "bool")) {
  text <- tolower(text)
  f <- function(text) {
    function(matched_against) {
      if (grepl(pattern = matched_against, text)) {
        return(TRUE)
      } else return(FALSE)
    }
  }

  res <- vapply(to_be_matched_against, f(text), TRUE)
  return(names(res)[res])
}

# -------------------------------------------------

change_data_type <- function(db_data_type) {
  return(switch(db_data_type,
                integer = ,
                int = "integer",
                float = ,
                real = ,
                numeric = "numeric",
                character = ,
                text = ,
                char = "character",
                date = "date",
                bool = "logical"
  ))
}

# ----------------------------------------------

formatted_date <- function(entry, t_format = c("dmy", "mdy", "ymd")) {

  t_format <- match.arg(t_format)

  f <- switch(t_format,
              dmy = "%d%.%m%.%Y",
              mdy = "%m%.%d%.%Y",
              ymd = "%Y%.%m%.%d")

  char_date <- as.character(readr::parse_date(entry, f))

  return(char_date)

}
