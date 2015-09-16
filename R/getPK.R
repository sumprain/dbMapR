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
  #browser()
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
    return(dplyr::data_frame(table = table, column_name = NA, data_type = NA, nextVal = NA))
  } else {
    pkVals <- dplyr::collect(dplyr::tbl(src, table) %>% dplyr::select_(.dots = pk[[1]]))
    if (all((dim(pkVals) == c(0,0)))) {
      pkVals <- numeric()
    } else pkVals <- pkVals[[1]]
    return(dplyr::bind_cols(dplyr::data_frame(table = table), pk, dplyr::data_frame(nextVal = nextNumber(pkVals))))  # nextNumber is defined as Cpp function.
  }
}
