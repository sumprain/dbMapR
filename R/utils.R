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

# LIST MANIPULATION FUNCTIONS START --------------------------------------------------

remove_from_list = function(list) {
  return(list[-1])
}

#------------------------------------------------------

add_val_to_list = function(list, val, id, lim) {

  if (length(list) >= lim) {
    list <- remove_from_list(list)
  }

  list[[length(list) + 1]] <- val
  names(list)[length(list)] <- id

  return(list)
}

#----------------------------------------------------

empty_list = function(list) {
  list <- NULL
  return(vector("list"))
}

# LIST MANIPULATION FUNCTIONS END -------------------
#----------------------------------------------------

create_id <- function() {
  return(paste0(format(Sys.time(), "%Y%m%d%H%M%S"), abs(round(stats::rnorm(n = 1,mean = 10,sd = 10),digits = 0))))
}

#---------------------------------------------------

getColumnInfo <- function(src, tbl_name) {
  # src: src of dplyr
  if (inherits(src, "src_postgres")) {
    dfCol <- RPostgreSQL::dbGetQuery(src$con, paste0("SELECT column_name, is_nullable, udt_name, column_default FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = ", dplyr::escape(tbl_name)))
  }
  return(dfCol)
}

#----------------------------------------------------

getKeyInfo <- function(src, key_name = c("PRIMARY KEY", "FOREIGN KEY"), tbl_name) {

  key_name <- match.arg(key_name)

  if (inherits(src, "src_postgres")) {

    sSQL2 <- " AND tc.table_name = "

    if (key_name == "PRIMARY KEY") {
      sSQL1 <- "SELECT tc.table_name, kcu.column_name FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = "
    } else {
      sSQL1 <- "SELECT tc.constraint_name, tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name, rc.update_rule As update_rule, rc.delete_rule AS delete_rule FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name JOIN information_schema.referential_constraints AS rc ON rc.constraint_name = tc.constraint_name WHERE constraint_type = "
    }

      df <- RPostgreSQL::dbGetQuery(src$con, paste0(sSQL1, dplyr::escape(key_name), sSQL2, dplyr::escape(tbl_name)))
      d <- dim(df)
      dd <- d[1]*d[2]
      if (dd == 0) {
        if (key_name == "PRIMARY KEY") {
          df <- null_df_to_na(df, c("column_name", "isPK"))
        } else {
          df <- null_df_to_na(df, c("column_name", "foreign_table_name", "foreign_column_name", "update_rule", "delete_rule", "isFK"))
        }

      } else {
        if (key_name == "PRIMARY KEY") {
          df <- df %>% dplyr::select_("column_name") %>% unique %>% dplyr::mutate_(isPK = ~ rep(1, nrow(.)))

        } else {
          df <- df %>% dplyr::select_("column_name", "foreign_table_name", "foreign_column_name", "update_rule", "delete_rule") %>% unique %>% dplyr::mutate_(isFK = ~ rep(1, nrow(.)))
        }
      }
  }

  return(df)
}

# -------------------------------------------------

match_text <- function(text, to_be_matched_against = c("char", "text", "int", "date", "float", "numeric", "real", "bool")) {
  text <- tolower(text)
  f <- function(text) {
    function(matched_against) {
      if (grepl(pattern = matched_against, text)) {
        return(TRUE)
      } else return(FALSE)
    }
  }

  res <- sapply(to_be_matched_against, f(text))
  return(names(res)[res])
}

# -------------------------------------------------

change_data_type <- function(db_data_type) {
  return(switch(db_data_type,
    int = "integer",
    float = ,
    real = ,
    numeric = "numeric",
    text = ,
    char = "character",
    date = "date",
    bool = "logical"
  ))
}

# ------------------------------------------------

null_df_to_na <- function(df, col_names) {
  d <- dim(df)
  dd <- d[1]*d[2]
  if (dd == 0) {
    return(dplyr::as_data_frame(setNames(as.list(rep(NA_character_, length(col_names))), col_names)))
  } else return(df)
}

# ----------------------------------------------
