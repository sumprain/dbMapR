#' @useDynLib dbfrontendR
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr "%>%"
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
    stop(err_msg,call. = FALSE)
  }
  return(x)
}

#---------------------------------------------

compact <- function(x) {
  retx <- vapply(x, function(y) return(is.null(y)||is.na(y)), logical(1L))
  return(x[!retx])
}

#---------------------------------------------
all_elements_equal <- function(x) {

  if (length(unique(x)) == 1L) {
    return(TRUE)
  } else FALSE

}

#---------------------------------------------

check_fk_val_generic <- function(src, col, val) {

  refTable <- col$get_refTable()
  refCol <- col$get_refCol()

  chk_status <- TRUE
  err_msg <- NULL

  if (is.null(val) || is.na(val)) {
    chk_status <- FALSE
    err_msg <- "FK cannot be nothing"
    return(list(chk_status = chk_status, err_msg = err_msg))
  }

  poss_vals <- dplyr::collect(dplyr::tbl(src, refTable) %>% dplyr::select_(.dots = refCol))

  if (!(val %in% poss_vals[[refCol]])) {
    err_msg <- paste0("Value of ", val, " to be added to ", col$get_name(), " is not contained in PK: ", refTable, "-", refCol)
    chk_status <- FALSE
  }

  return(list(chk_status = chk_status, err_msg = err_msg))
}
