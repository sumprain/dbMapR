#' Get Primary Key of a table and find the next available value for the same.
#'
#' @param src The \code{src_*} functions for database (postgresql and sqlite) from \code{dplyr} package.
#' @param table String representing table name of the data base.
#'
#' @return Returns a data frame with table name, column name, type of data and next available number as columns.
#' @export
getPK <- function(src, table) {

   # RSQLite::dbGetQuery(src$con, "PRAGMA table_info(child)")
#   cid name      type notnull dflt_value pk
#   1   0   id   integer       0       <NA>  1
#   2   1 name character       0       <NA>  0
#
  if (!(table %in% dplyr::db_list_tables(src$con)))
    stop(paste0(table, " is not a table in database."))

  if (inherits(src, "src_postgres")) {
    sInit <- "SELECT c.column_name, c.data_type FROM information_schema.table_constraints tc JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name) JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema AND tc.table_name = c.table_name AND ccu.column_name = c.column_name where constraint_type = 'PRIMARY KEY' and tc.table_name =  "
    sSql <- paste0(sInit, " ", dplyr::escape(table))
    pk <- dplyr::collect(dplyr::tbl(src, dplyr::sql(sSql)))
  } else if (inherits(src, "src_sqlite")) {
    sInit <- "PRAGMA table_info("
    sSql <- paste0(sInit, dplyr::escape(table), ")")
    pkDf <- RSQLite::dbGetQuery(src$con, sSql)
    pk <-  pkDf %>% filter(pk == 1) %>% dplyr::select(name, type) %>% dplyr::rename(column_name = name, data_type = type)
  }

  if (all((dim(pk) == c(0,0)))) {
    warning(paste0(table, " does not have a primary key. It is advised that a primary key is present."))
    return(dplyr::data_frame(column_name = NA, data_type = NA, nextVal = NA))
  } else {
    pkVals <- dplyr::collect(dplyr::tbl(src, table) %>% dplyr::select_(.dots = pk[[1]]))
    if (all((dim(pkVals) == c(0,0)))) {
      pkVals <- numeric()
      return(dplyr::bind_cols(pk, dplyr::data_frame(next_val = nextNumber(pkVals))))  # nextNumber is defined as Cpp function.
    } else {
      pkVals <- pkVals[[1]]
      if (is.integer(pkVals)) {
        return(dplyr::bind_cols(pk, dplyr::data_frame(next_val = nextNumber(pkVals))))  # nextNumber is defined as Cpp function.
      } else {
        message(paste0(table, " has primary key values of type: ", pk$data_type, ". User to provide next PK value."))
        return(dplyr::bind_cols(pk, dplyr::data_frame(next_val = NA)))
      }
    }


  }
}
