#' @useDynLib dbfrontendR
#' @importFrom Rcpp sourceCpp
NULL

# functions used for createTable, taken verbatim from dplyr-----------------

sql_vector <- function (x, parens = NA, collapse = " ", con = NULL)
{
  if (is.na(parens)) {
    parens <- length(x) > 1L
  }
  x <- names_to_as(x, con = con)
  x <- paste(x, collapse = collapse)
  if (parens)
    x <- paste0("(", x, ")")
  sql(x)
}

#-------------------------------------------------
names_to_as <- function (x, con = NULL)
{
  names <- names2(x)
  as <- ifelse(names == "", "", paste0(" AS ", dplyr::sql_escape_ident(con,
                                                                       names)))
  paste0(x, as)
}

#---------------------------------------------------
names2 <- function (x)
{
  names(x) %||% rep("", length(x))
}

#---------------------------------------------------

`%||%` <- function (x, y)
  if (is.null(x)) y else x

#--------------------------------------------------

sql_escape_ident.DBIConnection <- function(con, x) {
  dplyr::sql_quote(x, '"')
}

#--------------------------------------------------

sql_escape_ident.NULL <- sql_escape_ident.DBIConnection

#----------------------------------------------------

cur_timestamp <- function(digits = 3L) {

  options(digits.secs = digits)
  res <- format(Sys.time(), "%Y%m%d%H%M%OS")
  options(digits.secs = NULL)
  return(res)

}

#---------------------------------------------------

uid <- function(digits = 16L) {
  return(paste0(sample(c(as.character(0:9), letters[1:6]),size = digits, replace = TRUE), collapse = ""))
}

#--------------------------------------------------

is_nothing_df <- function(df) {

  if (is.null(df)) {
    return(TRUE)
  }

  d <- dim(df)
  dd <- d[1]*d[2]

  if (dd == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}

#-------------------------------------------

na_error <- function(x, err_msg) {
  if (is.na(x)) {
    stop(err_msg)
  }
  return(x)
}
