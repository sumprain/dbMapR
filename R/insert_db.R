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

  if (is.null(col$get_valToDB()) || is.na(col$get_valToDB())) {
    stop("FK cannot be nothing")
  }
  #browser()
  poss_vals <- dplyr::collect(dplyr::tbl(src, refTable) %>% dplyr::select_(.dots = refCol))

  if (!(col$get_valToDB() %in% poss_vals[[refCol]])) {
    invalid_val <- col$get_valToDB()
    col$revert_valToDB_null()
    stop(paste0("Value of ", invalid_val, " to be added to ", col$get_name(), " is not contained in PK: ", refTable, "-", refCol))
  }

  invisible(NULL)
}

## check for columns with no values with isRequired and delete the columns if required

is_nothing_allowed <- function(col) {

  is_val_nothing <- (is.null(col$get_valToDB()) || is.na(col$get_valToDB()))
  defaultVal <- col$get_defaultVal()
  is_default_available <- !(is.null(defaultVal) || is.na(defaultVal))
  isreqd <- (col$get_isRequired() == 1)

  if ((col$get_isFK() == 1) && is_val_nothing) {
    return(FALSE)
  }

  if (is_val_nothing && isreqd) {
    if (!is_default_available) {
      return(FALSE)
    } else return(TRUE)
  } else return(TRUE)

}

prepare_cols_for_insertion <- function(src, table) {

  cols <- table$get_nameColumns()
  vals <- setNames(vector("list", length(cols)), cols)

  for (i in cols) {
    coli <- table$get_columns()[[i]]

    # check for PK
    if (coli$get_isPK() == 1) {
      vals[[i]] <- insert_pk_val(src, coli)
      next()
    }

    # check for FK
    if (coli$get_isFK() == 1) {
      check_fk_val(src, coli)
    }

    # check for NULL field
    if (!is_nothing_allowed(coli)) {
      stop(paste0("No value is available for ", coli$get_name(), " of ", coli$get_nameTable()))
    }

    vals[[i]] <- coli$get_valToDB()
  }

  return(compact(vals))
}

## make INSERT statement

insert_into_table <- function(src, table) {

  list_col_val <- prepare_cols_for_insertion(src, table)

  cols <- names(list_col_val)

  s_insert <- dplyr::build_sql(dplyr::sql("INSERT INTO "), dplyr::escape(table$get_name()), dplyr::sql(" "), dplyr::escape(cols), dplyr::sql(" VALUES "), dplyr::escape(unname(list_col_val)))

  res <- DBI::dbSendQuery(src$con, s_insert)
  DBI::dbClearResult(res)

  ## TODO: capture any error from the INSERT action and pass it as value to be later on added to shinywidget validation.
  ## TODO: check if TRANSACTION can be added to INSERT above

  return(NULL)
}

#--------------------------------------------------------------------------

revert_vals_to_null <- function(table) {

  cols <- table$get_columns()

  lapply(cols, function(x) {
    x$revert_valToDB_null()
  })

  invisible(NULL)
}
