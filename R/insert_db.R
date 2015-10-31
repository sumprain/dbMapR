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

  val <- col$get_valToDB()
  chk_fk <- check_fk_val_generic(src, col, val)

  if (!chk_fk$chk_status) {
    col$revert_valToDB_null()
    stop(chk_fk$err_msg, call. = FALSE)
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

prepare_cols_for_insertion <- function(src, table, name_token_col = NULL) {

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

    # will automatically fill up the timestamp column (token column)
    if (!is.null(name_token_col) && (coli$get_name() == name_token_col)) {
      coli$add_valToDB(cur_timestamp())
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

insert_into_table <- function(src, table, name_token_col = NULL) {

  list_col_val <- prepare_cols_for_insertion(src, table, name_token_col)

  cols <- names(list_col_val)

  s_insert <- dplyr::build_sql(dplyr::sql("INSERT INTO "), dplyr::escape(table$get_name()), dplyr::sql(" "), dplyr::escape(cols), dplyr::sql(" VALUES "), dplyr::escape(unname(list_col_val)))

  err_ind <- err_from_db(src, DBI::dbSendQuery(src$con, s_insert))

  invisible(err_ind)
}

#--------------------------------------------------------------------------

insert_into_queue_valToDB <- function(table, token_col_name = NULL) {

  cols <- table$get_columns()
  pk_id <- cols[[table$get_PKColumn()]]$get_PKNextVal()

  lapply(cols, function(x) {
    l_str <- setNames(vector("list", 3), c("val_to_db", "pk_id", "time_stamp"))
    if (!((x$get_isPK() == 1) || (x$get_name() == token_col_name))) {
      l_str[["val_to_db"]] <- x$get_valToDB()
      l_str[["pk_id"]] <- pk_id
      l_str[["time_stamp"]] <- ifelse(!is.null(token_col_name), cols[[token_col_name]]$get_valToDB(), NULL)
      push(x$get_queue_valToDB(), l_str)
      l_str <- NULL
    }
  })
  invisible(NULL)

}
#--------------------------------------------------------------------------

revert_vals_to_null <- function(table) {

  cols <- table$get_columns()

  lapply(cols, function(x) {
    x$revert_valToDB_null()
  })

  invisible(NULL)
}
