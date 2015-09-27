match_text <- function(text, to_be_matched_against = c("char", "text", "int", "date", "float", "numeric", "real", "bool", "timestamp")) {
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
                timestamp = ,
                character = ,
                text = ,
                char = "character",
                date = "date",
                bool = "logical"
  ))
}

# ----------------------------------------------

formatted_date <- function(entry, t_format = c("dmy", "mdy", "ymd"), ...) {

  t_format <- match.arg(t_format)

  f <- switch(t_format,
              dmy = "%d%.%m%.%Y",
              mdy = "%m%.%d%.%Y",
              ymd = "%Y%.%m%.%d")

  char_date <- readr::parse_date(entry, f, ...)

  return(char_date)

}

#-----------------------------------------------

parse_val <- function(val, type_data, date_format, ...) {

  val <- switch(type_data,
                date = formatted_date(val, date_format, ...),
                numeric = readr::parse_double(val, ...),
                integer = readr::parse_integer(val, ...),
                character = readr::parse_character(val, ...),
                logical = readr::parse_integer(1*readr::parse_logical(val, ...)))

  format_error <- attr(val, "problems")
  attr(val, "format_error") <- FALSE
  if (!is.null(format_error)) {
    attr(val, "problems") <- NULL
    attr(val, "format_error") <- TRUE
  }
  return(val)

}
