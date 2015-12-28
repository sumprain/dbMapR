#' @export
dbTableClass <- R6::R6Class('dbTableClass',
                            public = list(
                              initialize = function(tbl_name, src, parentDB, date_input = c("dmy", "mdy", "ymd")) {
                                #browser()
                                if (!(tbl_name %in% dplyr::db_list_tables(src$con)))
                                  stop(paste0(tbl_name, " not present in the DB."))
                                
                                date_input <- match.arg(date_input)
                                self$set_name(tbl_name)
                                private$src <- src
                                self$set_parentDB(parentDB)

                                df_col1 <- getColumnInfo(private$src, self$get_name())
                                df_col_pk <- getKeyInfo_pk(private$src, self$get_name())
                                df_col_fk <- getKeyInfo_fk(private$src, self$get_name())

                                df_col <- df_col1 %>% dplyr::left_join(df_col_pk, by = c("column_name" = "column_name")) %>% dplyr::left_join(df_col_fk, by = c("column_name" = "column_name"))
                                df_col[, c("isPK", "isFK")][is.na(df_col[, c("isPK", "isFK")])] <- 0
                                  #df_col[is.na(df_col)] <- ""
                                self$set_PKColumn(df_col[df_col$isPK == 1, "column_name", drop = TRUE])
                                self$set_nameColumns(df_col[, "column_name", drop = TRUE])
                                self$set_dfForeignKey(df_col_fk)
                                private$fill_with_cols(df_col, date_input)
                                invisible(self)

                              },

                              set_name = function(name) {
                                private$name <- as.character(name)
                                invisible(self)
                              },

                              set_PKColumn = function(PKColumn) {
                                private$PKColumn <- PKColumn
                                invisible(self)
                              },

                              set_nameColumns = function(nameColumns) {
                                private$nameColumns <- nameColumns
                                invisible(self)
                              },

                              set_dfForeignKey = function(dfForeignKey) {
                                private$dfForeignKey <- dfForeignKey
                                invisible(self)
                              },
                              
                              set_masterTable = function(tbl_name = NULL) {
                                if (is.null(tbl_name)) {
                                  private$masterTable <- NULL
                                } else {
                                  dffk <- private$dfForeignKey
                                  fk_tbls <- dffk[(dffk$isFK == 1) & !is.na(dffk$isFK), "foreign_table_name"][[1]]
                                  if ((length(fk_tbls) > 0L) && (tbl_name %in% fk_tbls)) {
                                    private$masterTable <- tbl_name
                                  } else {
                                    stop("Suggested table name is not amongst the list of linked tables.",call. = FALSE)
                                  }
                                }
                                invisible(self)
                              },
                              
                              set_parentDB = function(db) {
                                private$parentDB <- db
                                invisible(self)
                              },

                              get_name = function() {
                                return(private$name)
                              },

                              get_columns = function() {
                                return(private$columns)
                              },

                              get_PKColumn = function() {
                                return(private$PKColumn)
                              },

                              get_nameColumns = function() {
                                return(private$nameColumns)
                              },

                              get_dfForeignKey = function() {
                                return(private$dfForeignKey)
                              },
                              
                              get_masterTable = function() {
                                private$masterTable
                              },

                              get_parentDB = function() {
                                private$parentDB
                              },
                              
                              insertIntoDB = function(token_col_name = NULL) {

                                if (length(private$PKColumn) == 0L) {
                                  stop(paste0("Table: ", private$name, " does not have PK. Every table should have a PK."), call. = FALSE)
                                }

                                err_ind <- insert_into_table(private$src, self, token_col_name)

                                if (!err_ind$is_err) {
                                  insert_into_queue_valToDB(self, token_col_name)
                                  revert_vals_to_null(self)

                                }

                                invisible(err_ind)

                              },

                              retrieveRowForUpdate = function(pk_id, token_col_name = NULL) {
                                df_row <- retrieve_row(private$src, self, pk_id)
                                fill_update_info(self, df_row, token_col_name)
                                invisible(self)
                              },

                              updateToDB = function(token_col_name = NULL) {

                                err_ind <- update_table(private$src, self, token_col_name)

                                if (!err_ind$is_err) {
                                  insert_into_queue_valToBeUpdated(self, token_col_name)
                                }

                                invisible(err_ind)
                              }),

                            private = list(

                              name = NULL,               # database name of table
                              columns = list(),          # list containing the columns (dbColumnClass)
                              PKColumn = NULL,           # name of PK column
                              nameColumns = NULL,        # vector representing name of columns
                              dfForeignKey = NULL,       # dataframe containing the FK details with col names: col_name, foreign_tbl_name, foreign_col_name
                              masterTable = NULL,        # master table (one of the tables with PK linking with FK cols)
                              parentDB = NULL,           # parent DB object
                              src = NULL,

                              fill_with_cols = function(df_col, date_input) {

                                for (i in 1:nrow(df_col)) {
                                  intdf <- df_col[i, ]
                                  vars_2b_locked <- c("name", "nameTable", "parentTable", "isPK", 
                                                      "isFK", "refTable", "refCol", 
                                                      "updateRule", "deleteRule", 
                                                      "typeData", "varSize", "defaultVal", "isRequired")
                                  intdf[["udt_name"]] <- change_data_type(match_text(intdf[["udt_name"]]))
                                  private$columns[[intdf[["column_name"]]]] <- dbColumnClass$new(name = intdf[["column_name"]],
                                          nameTable = self$get_name(),
                                          parentTable = self,
                                          isPK = as.integer(intdf[["isPK"]]),
                                          isFK = as.integer(intdf[["isFK"]]),
                                          refTable = intdf[["foreign_table_name"]],
                                          refCol = intdf[["foreign_column_name"]],
                                          update_rule = intdf[["update_rule"]],
                                          delete_rule = intdf[["delete_rule"]],
                                          typeData = intdf[["udt_name"]],
                                          varSize = intdf[["var_size"]],
                                          isRequired = intdf[["is_required"]],
                                          defaultVal = intdf[["column_default"]],
                                          date_input = date_input)
                                  lapply(vars_2b_locked, function(x) {
                                    lockBinding(x, private$columns[[intdf[["column_name"]]]]$.__enclos_env__$private)
                                  })
                                }
                                invisible(NULL)
                              }
                            ))
