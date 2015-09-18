#' @export
dbTableClass <- R6::R6Class('dbTableClass',
                            public = list(
                              initialize = function(tbl_name, method = c("extract_from_db", "create_from_scratch"), src, date_input = c("dmy", "mdy", "ymd")) {
                                #browser()
                                if (!(tbl_name %in% dplyr::db_list_tables(src$con)))
                                  stop(paste0(tbl_name, " not present in the DB."))
                                date_input <- match.arg(date_input)
                                self$set_name(tbl_name)
                                if (!is.null(method)) {
                                  method <- match.arg(method)
                                } else {
                                  method <- "extract_from_db"        # we will change this part of code later on when we add other methods
                                }

                                if (method == "extract_from_db") {
                                  df_col1 <- getColumnInfo(src, self$get_name())
                                  df_col_pk <- getKeyInfo(src, "PRIMARY KEY", self$get_name())
                                  df_col_fk <- getKeyInfo(src, "FOREIGN KEY", self$get_name())

                                  df_col <- df_col1 %>% left_join(df_col_pk, by = c("column_name" = "column_name")) %>% left_join(df_col_fk, by = c("column_name" = "column_name"))
                                  df_col[, c("isPK", "isFK")][is.na(df_col[, c("isPK", "isFK")])] <- 0
                                  df_col[is.na(df_col)] <- ""
                                  self$set_dfPKColumn(getPK(src, self$get_name()))
                                  self$set_nameColumns(df_col[, "column_name", drop = TRUE])
                                  self$set_dfForeignKey(df_col_fk)
                                  private$fill_with_cols(df_col, date_input)

                                } else if (method == "create_from_scratch") {
                                  # TODO: fill later on
                                }
                                invisible(self)

                              },

                              set_name = function(name) {
                                private$name <- as.character(name)
                                invisible(self)
                              },

                              set_dfPKColumn = function(dfPKColumn) {       # contains next PK value also. dataframe with column_name, data_type, next_val
                                private$dfPKColumn <- dfPKColumn
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

                              get_name = function() {
                                return(private$name)
                              },

                              get_columns = function() {
                                return(private$columns)
                              },

                              get_dfPKColumn = function() {
                                return(private$dfPKColumn)
                              },

                              get_nameColumns = function() {
                                return(private$nameColumns)
                              },

                              get_dfForeignKey = function() {
                                return(private$dfForeignKey)
                              },

                              insertIntoDB = function() {},
                              updateToDB = function() {},
                              deleteRow = function() {}
                            ), private = list(

                              name = NULL,               # database name of table
                              columns = list(),          # list containing the columns (dbColumnClass)
                              dfPKColumn = NULL,       # name of PK column
                              nameColumns = NULL,        # vector representing name of columns
                              dfForeignKey = NULL,       # dataframe containing the FK details with col names: col_name, foreign_tbl_name, foreign_col_name

                              fill_with_cols = function(df_col, date_input) {

                                for (i in 1:nrow(df_col)) {
                                  intdf <- df_col[i, ]
                                  intdf[["udt_name"]] <- change_data_type(match_text(intdf[["udt_name"]]))
                                  private$columns[[intdf[["column_name"]]]] <- dbColumnClass$new(name = intdf[["column_name"]],
                                          nameTable = self$get_name(),
                                          isPK = as.integer(intdf[["isPK"]]),
                                          PKNextVal = private$dfPKColumn[["next_val"]],
                                          isFK = as.integer(intdf[["isFK"]]),
                                          refTable = intdf[["foreign_table_name"]],
                                          refCol = intdf[["foreign_column_name"]],
                                          update_rule = intdf[["update_rule"]],
                                          delete_rule = intdf[["delete_rule"]],
                                          typeData = intdf[["udt_name"]],
                                          isRequired = 1 * (intdf[["is_nullable"]] == "NO"),
                                          defaultVal = intdf[["column_default"]],
                                          date_input = date_input)
                                }
                                invisible(NULL)
                              }
                            ))
