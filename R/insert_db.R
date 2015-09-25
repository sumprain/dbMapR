# make function to INSERT data into database ---------
## It will be called from table class

## insert PK

insert_pk_val <- function(src, col) {

  if (col$get_typeData() == "integer") {
    pkVals <- dplyr::collect(dplyr::tbl(src, col$get_nameTable()) %>% dplyr::select_(.dots = col$get_name()))[[col$get_name()]]
    val <- nextNumber(pkVals)
  } else {
    val <- uid()
  }

  col$set_PKNextVal(val)
  return(val)

}

## insert FK

check_fk_val <- function(src, col) {

  refTable <- col$get_refTable()
  refCol <- col$get_refCol()

  poss_vals <- dplyr::collect(dplyr::tbl(src, refTable) %>% dplyr::select_(.dots = refCol))

  if (!(col$get_valToDB() %in% poss_vals[[refCol]]))
    stop(paste0("Value of ", col$get_valToDB(), " to be added to ", col$get_name(), " is not contained in PK: ", refTable, "-", refCol))

  invisible(NULL)
}

## check for columns with no values with isRequired and delete the columns if required

check_for_nothing <- function(col) {

  isnull <- is.null(col$get_valToDB())
  isna <- is.na(col$get_valToDB())
  defaultVal <- col$get_defaultVal()
  is_default_available <- !(is.null(defaultVal) | is.na(defaultVal) | defaultVal == "")
  isreqd <- (col$get_isRequired() == 1)
  is_val_nothing <- (isnull | isna)

  if (is_val_nothing && isreqd) {
    if (!is_default_available) {
      stop(paste0("No value is available for ", col$get_name(), " of ", col$get_nameTable()))
    }
  }

  invisible(NULL)
}

prepare_cols_for_insertion <- function(src, table) {

  cols <- table$get_nameColumns()
  vals <- setNames(vector("list", length(cols)), cols)

  for (i in cols) {
    coli <- table$get_columns()[[i]]

    # check for PK
    if (coli$isPK() == 1) {
      vals[[i]] <- insert_pk_val(src, coli)
      next()
    }

    # check for FK
    if (coli$isFK() == 1) {
      check_fk_val(src, coli)
    }

    # check for NULL field
    check_for_nothing(coli)

    vals[[i]] <- coli$get_valToDB()
  }

  vals <- lapply(vals, function(x) {
    if (is.null(x) | (is.na(x))) {
      x <- NULL
    }
  })

  return(vals)
}

## make INSERT statement

insert_into_table <- function(src, table) {

  list_col_val <- prepare_cols_for_insertion(src, table)

  cols <- names(list_col_val)

  s_insert <- dplyr::build_sql(dplyr::sql("INSERT INTO "), dplyr::escape(table$get_name()), dplyr::sql(" "), dplyr::escape(cols), dplyr::sql(" VALUES "), dplyr::escape(list_col_val))

  if (inherits(src, "src_postgres")) {
    RPostgreSQL::dbSendQuery(src$con, s_insert)
  } else if (inherits(src, "src_postgres")) {
    RSQLite::dbSendQuery(src$con, s_insert)
  }
  ## TODO: capture any error from the INSERT action and pass it as value to be later on added to shinywidget validation.
  ## TODO: check if TRANSACTION can be added to INSERT above

  return(NULL)
}
