initiate_updateContainer <- function() {

  e <- new.env(parent = emptyenv())

  assign("orig_val", NULL, envir = e)
  assign("modified_val", NULL, envir = e)
  assign("pk_id", NULL, envir = e)
  assign("time_stamp", NULL, envir = e)

  lockEnvironment(e)

  return(e)
}

#-------------------------------------------

assign_vars <- function(e, orig_val, pk_id, time_stamp = NULL) {

  assign("orig_val", orig_val, envir = e)
  assign("pk_id", pk_id, envir = e)
  assign("time_stamp", time_stamp, envir = e)

  lockBinding("orig_val", e)
  lockBinding("pk_id", e)
  lockBinding("time_stamp", e)

  invisible(e)
}

#-----------------------------------------------

retrieve_row <- function(src, table, pk_id) {

  row <- dplyr::collect(dplyr::tbl(src, table$get_name()) %>% dplyr::filter_(.dots = ~ (dplyr::sql(table$get_PKColumn()) == pk_id)))

  if (nrow(row) == 0L) {
    stop(paste0("Table ", table$get_name(), " does not have PK value: ", pk_id, "."), call. = FALSE)
  }

  if (nrow(row) > 1L) {
    stop(paste0("PK id: ", pk_id, " has more than one row. Kindly check."), call. = FALSE)
  }

  return(row)

}

#--------------------------------------------------

fill_update_info <- function(table, df_vals, token_col_name = NULL) {

  stopifnot(nrow(df_vals) == 1L)

  pk_val <- df_vals[[table$get_PKColumn()]]

  time_stamp <- ifelse(!is.null(token_col_name), df_vals[[token_col_name]], NULL)

  lapply(table$get_columns(), function(x) {
    col_name <- x$get_name()
    if (!(col_name %in% c(table$get_PKColumn(), token_col_name))) {
      assign_vars(x$get_updateContainer(), df_vals[[col_name]], pk_val, time_stamp)
    }
  })
  return(NULL)
}

#----------------------------------------------------

check_fk_val_update <- function(src, col, val) {

  chk_fk <- check_fk_val_generic(src, col, val)

  if (!chk_fk$chk_status) {
    stop(chk_fk$err_msg, call. = FALSE)
  }

  invisible(NULL)
}

#---------------------------------------------------------------

col_names_for_update <- function(table, token_col_name = NULL) {

  col_names <- table$get_nameColumns()
  pk_col_name <- table$get_PKColumn()

  col_names <- col_names[!(col_names %in% c(pk_col_name, token_col_name))]

  return(col_names)

}

#---------------------------------------------------------------

update_table <- function(src, table, token_col_name = NULL) {

  # 1. get all values from table object

  pk_col_name <- table$get_PKColumn()
  col_names <- col_names_for_update(table, token_col_name)
  cols <- table$get_columns()[col_names]

  type_pk <- table$get_columns()[[pk_col_name]]$get_typeData()

  # 2. get all the modified values from the updateContainer environment.

  vals_to_be_updated <- lapply(cols, function(x) {

    val <- x$get_updateContainer()[["modified_val"]]

  # 2a. check FK constraint

    if (x$get_isFK()) {
      check_fk_val_update(src, x, val)
    }

    if (!is.null(val)) {
      val_modified <- corrected_input(x, val)
    } else {
      val_modified <- NULL
    }

    return(val_modified)

  })

  names(vals_to_be_updated) <- col_names

  fun_type <- if (type_pk == "integer") {
    return(integer(1L))
  } else if (type_pk == "character") {
    return(character(1L))
  } else if (type_pk == "numeric") {
    return(numeric(1L))
  }

  # 3. get all pk_vals from the environment

  pk_vals <- vapply(cols, function(x) x$get_updateContainer()[["pk_id"]], FUN = fun_type)

  # 3a. check if any of the pk vals are different from others, if so raise error

  if (!all_elements_equal(pk_vals)) {
    stop("All PK values are not same for columns for updating.")
  }

  # 4. only carry out next statements, if token_col_name is not NULL

  if (!is.null(token_col_name)) {

    time_stamps <- vapply(cols, function(x) x$get_updateContainer()[["time_stamp"]], FUN = character(1L))

    # 4a. check if any of the time stamp is different from others, if so raise error

    if (!all_elements_equal(time_stamps)) {
      stop("All time stamp values are not same for columns for updating.")
    }
    # 5. get present time stamp from database and check if it is different from the time stamp present in the environment.

    present_time_stamp <- dplyr::collect(dplyr::tbl(src, table$get_name()) %>% dplyr::filter_(.dots = ~ (dplyr::sql(table$get_PKColumn()) == pk_vals[1])))[[token_col_name]]

    if (!(time_stamps[1] %same_time% present_time_stamp)) {
      stop(paste0("The row has been modified from the time it was retrieved for update.\nOriginal timestamp: ", time_stamps[1], ". Time stamp obtained now from database: ", present_time_stamp), call. = FALSE)
    }
  }

  # 6. compact list to include only the non null values

  vals_to_be_updated <- compact(vals_to_be_updated)

  # 7. make appropriate SQL update statement

  # UPDATE <table_name> SET <col1> = <val1>, <col2> = <val2> WHERE <pk_col> = <pk_val>

  update_ <- dplyr::sql("UPDATE ")
  table_ <- dplyr::sql(dplyr::escape(table$get_name()))
  set_ <- dplyr::sql(" SET ")
  col_names <- dplyr::escape(dplyr::sql(names(vals_to_be_updated)))
  col_vals <- dplyr::escape(unname(vals_to_be_updated),collapse = NULL, parens = FALSE)
  cols_ <- dplyr::sql(paste0(paste0(col_names, " = ", col_vals), collapse = ", "))
  where_ <- dplyr::sql(" WHERE ")
  pk_col <- dplyr::escape(pk_col_name)
  pk_val <- dplyr::escape(pk_vals[1])
  pk_ <- dplyr::sql(paste0(pk_col, " = ", pk_val))

  if (!is.null(token_col_name)) {
    time_stamp <- dplyr::sql(paste0(", ", dplyr::sql(token_col_name), " = ", dplyr::escape(cur_timestamp())))
    final_ <- dplyr::sql(paste0(update_, table_, set_, cols_, time_stamp, where_, pk_))
  } else {
    final_ <- dplyr::sql(paste0(update_, table_, set_, cols_, where_, pk_))
  }

  err_ind <- err_from_db(src, DBI::dbSendQuery(src$con, final_))

  ## TODO: make the updateContainer empty after successful update and store them in a queue.

  invisible(err_ind)

}

#-----------------------------------------------------

insert_into_queue_valToBeUpdated <- function(table, token_col_name = NULL) {

  col_names <- col_names_for_update(table, token_col_name)

  cols <- table$get_columns()[col_names]

  lapply(cols, function(x) {
    l_str <- as.list(x$get_updateContainer())
    push(x$get_queue_valToBeUpdated(), l_str)
    x$revert_updateContainer_null()         # revert updateContainer to NULL
    l_str <- NULL
  })

  invisible(NULL)

}

#----------------------------------------------------

revert_env_null <- function(env) {

  objs <- objects(env)

  lapply(objs, function(x) {
    if (bindingIsLocked(x, env)) {
      unlockBinding(x, env)
    }
    assign(x, NULL, envir = env)
  })

  return(NULL)

}
