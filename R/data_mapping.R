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

# validate valToDB to user defined validation rules -----------------------------

validate <- function(val, condition) {
# condition is quoted expressions produced from lazydots
# val is the value which needs to be validated

  validate_result <- vapply(condition, function(x) {
    res <- lazyeval::lazy_eval(x, data = list(`..` = val))
    res
  }, logical(1L))

  err_msg <- paste0("<VALIDATION FAILURE FOR RULE(S)> ", paste0(vapply(condition[!validate_result], function(x) deparse(lazyeval::interp(x[["expr"]], .values = list(`..` = val))), character(1L)), collapse = "; "))

  return(list(result = all(validate_result), err_msg = err_msg))
}

# check input for correctness (for add_valToDB and update) --------------------------------

corrected_input <- function(col, val) {

  val1 <- parse_val(val, col$get_typeData(), col$get_date_input())

  if (attr(val1, "format_error")) {
    stop(paste0(col$get_nameTable(), "-", col$get_name(), ". Format of ", val, " is not ", col$get_typeData()), call. = FALSE)
  }

  if (!is.null(col$get_validationStatements())) {
    validate_res <- validate(val1, col$get_validationStatements())
    if (!validate_res$result) {
      stop(validate_res$err_msg, call. = FALSE)
    }
  }

  return(val1)

}
