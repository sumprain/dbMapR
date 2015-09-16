dbDatabaseClass <- R6::R6Class('dbDatabaseClass',
                               public = list(
                                 initialize = function(src) {
                                   self$set_connection(src)
                                   self$set_name(src$info$dbname)
                                   self$set_nameTables(dplyr::db_list_tables(src$con))
                                   private$populateTables(private$connection, private$nameTables)
                                   reg.finalizer(self, function(self) self$disconnect(), onexit = TRUE)
                                 },

                                 set_name = function(name) {
                                   private$name <- name
                                   invisible(self)
                                 },

                                 set_connection = function(src) {
                                   stopifnot(inherits(src, "src_postgres"))
                                   private$connection <- src
                                   invisible(self)
                                 },

                                 set_nameTables = function(nameTables) {
                                   private$nameTables <- nameTables
                                   invisible(self)
                                 },

                                 get_name = function() {
                                   return(private$name)
                                 },

                                 get_nameTables = function() {
                                   return(private$nameTables)
                                 },

                                 get_tables = function() {
                                  return(private$tables)
                                 },

                                 disconnect = function() {
                                   if (inherits(self$connection, "src_sqlite")) {
                                     RSQLite::dbDisconnect(self$connection$con)
                                   } else if (inherits(self$connection, "src_postgres")) {
                                     RPostgreSQL::dbDisconnect(self$connection$con)
                                     private$connection <- NULL
                                   }
                                 }),
                               private = list(
                                 name = NULL,             # name of database
                                 connection = NULL,       # src object from dplyr
                                 tables = list(),         # list of tables in database
                                 nameTables = NULL,        # character vector of names of tables

                                 populateTables = function(src, nameTables) {
                                   for (i in 1:length(nameTables)) {
                                     private$tables[[nameTables[i]]] <- dbTableClass$new(nameTables[i], "extract_from_db", src)
                                   }
                                 }
                               ))


dbTableClass <- R6::R6Class('dbTableClass',
                            public = list(
                              initialize = function(tbl_name, method = c("extract_from_db", "create_from_scratch"), src) {
                                #browser()
                                if (!(tbl_name %in% dplyr::db_list_tables(src$con)))
                                  stop(paste0(tbl_name, " not present in the DB."))

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
                                  #browser()
                                  private$fill_with_cols(df_col)
                                  self$set_namePKColumn(df_col[df_col$isPK == 1, "column_name", drop = TRUE])
                                  self$set_nameColumns(df_col[, "column_name", drop = TRUE])
                                  self$set_dfForeignKey(df_col_fk)
                                } else if (method == "create_from_scratch") {
                                  # TODO: fill later on
                                }
                                invisible(self)

                              },

                              set_name = function(name) {
                                private$name <- as.character(name)
                                invisible(self)
                              },

                              set_namePKColumn = function(namePKColumn) {
                                private$namePKColumn <- namePKColumn
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

                              get_namePKColumn = function() {
                                return(private$namePKColumn)
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
                              namePKColumn = NULL,       # name of PK column
                              nameColumns = NULL,        # vector representing name of columns
                              dfForeignKey = NULL,       # dataframe containing the FK details with col names: col_name, foreign_tbl_name, foreign_col_name

                              fill_with_cols = function(df_col) {

                                for (i in 1:nrow(df_col)) {
                                  intdf <- df_col[i, ]
                                  intdf[["udt_name"]] <- change_data_type(match_text(intdf[["udt_name"]]))
                                  private$columns[[intdf[["column_name"]]]] <- dbColumnClass$new(name = intdf[["column_name"]],
                                                    nameTable = self$get_name(),
                                                    isPK = as.integer(intdf[["isPK"]]),
                                                    isFK = as.integer(intdf[["isFK"]]),
                                                    refTable = intdf[["foreign_table_name"]],
                                                    refCol = intdf[["foreign_column_name"]],
                                                    typeData = intdf[["udt_name"]],
                                                    isRequired = 1*(intdf[["is_nullable"]] == "NO"),
                                                    defaultVal = intdf[["column_default"]])
                                }
                                invisible(NULL)
                              }
                            ))


dbColumnClass <- R6::R6Class('dbColumnClass',
                        public = list(

                          initialize = function(name,
                                                nameTable,
                                                isPK = NULL,
                                                isFK = NULL,
                                                refTable = NULL,
                                                refCol = NULL,
                                                typeData = NULL,
                                                isRequired = NULL,
                                                defaultVal = NULL,
                                                cacheVal = 5) {

                            self$set_name(name)
                            self$set_nameTable(nameTable)
                            self$set_isPK(isPK)
                            self$set_isFK(isFK)
                            self$set_refTable(refTable)
                            self$set_refCol(refCol)
                            self$set_typeData(typeData)
                            self$set_isRequired(isRequired)
                            self$set_defaultVal(defaultVal)
                            self$set_cacheVal(cacheVal)

                            invisible(self)
                          },

                        set_name = function(name) {
                            private$name <- as.character(name)
                            invisible(self)
                          },

                        set_nameTable = function(nameTable) {
                            private$nameTable <- as.character(nameTable)
                            invisible(self)
                          },

                        set_isPK = function(isPK) {
                            stopifnot(is.logical(isPK) | isPK %in% c(0, 1))
                            private$isPK <- 1*isPK
                            invisible(self)
                          },

                        set_isFK = function(isFK) {
                            stopifnot(is.logical(isFK) | isFK %in% c(0, 1))
                            private$isFK <- 1*isFK
                            invisible(self)
                          },

                        set_refTable = function(refTable) {
                            private$refTable <- as.character(refTable)
                            invisible(self)
                          },

                        set_refCol = function(refCol) {
                            private$refCol <- as.character(refCol)
                            invisible(self)
                          },

                        set_typeData = function(typeData = c("character", "numeric", "integer", "date", "logical", "TIMESTAMP", "SERIAL")) {
                            stopifnot(length(typeData) == 1)
                            private$typeData <- match.arg(typeData)
                            invisible(self)
                          },

                        set_isRequired = function(isRequired) {
                            stopifnot(is.logical(isRequired) | isRequired %in% c(0, 1))
                            private$isRequired <- 1*isRequired
                            invisible(self)
                          },

                        set_defaultVal = function(defaultVal) {
                            if (is.null(private$typeData)) {
                              private$defaultVal <- as.character(defaultVal)
                            } else {
                              defaultVal <- switch(private$typeData,
                                                   character = ,
                                                   date = as.character(defaultVal),
                                                   numeric = as.numeric(defaultVal),
                                                   integer = as.integer(defaultVal),
                                                   logical = as.logical(defaultVal),
                                                   SERIAL = ,
                                                   TIMESTAMP = NULL)
                              private$defaultVal <- defaultVal
                            }
                            invisible(self)
                          },

                        add_valFromDB = function(val) {
                          private$valFromDB <- add_val_to_list(private$valFromDB, val, id, private$cacheVal)
                          invisible(self)
                        },

                        add_valToDB = function(val) {

                          # TODO: Add validation code for checking consistency of the inputted val against typeData.
                          private$valToDB <- add_val_to_list(private$valToDB, val, id, private$cacheVal)
                          invisible(self)
                        },

                        empty_valFromDB = function() {
                          private$valFromDB <- empty_list(private$valFromDB)
                          invisible(self)
                        },

                        empty_valToDB = function() {
                          private$valToDB <- empty_list(private$valToDB)
                          invisible(self)
                        },

                        set_cacheVal = function(cacheVal) {
                          private$cacheVal <- cacheVal
                          invisible(self)
                        },

                        get_name = function() {
                            return(private$name)
                        },

                        get_nameTable = function() {
                            return(private$nameTable)
                        },

                        get_isPK = function() {
                            return(private$isPK)
                        },

                        get_isFK = function() {
                            return(private$isFK)
                        },

                        get_refTable = function() {
                            return(private$refTable)
                        },

                        get_refCol = function() {
                            return(private$refCol)
                        },

                        get_typeData = function() {
                            return(private$typeData)
                        },

                        get_isRequired = function() {
                            return(private$isRequired)
                        },

                        get_defaultVal = function() {
                            return(private$defaultVal)
                        },

                        get_valFromDB = function(id = NULL) {
                          if (is.null(id)) {
                            return(private$valFromDB)
                          } else {
                            return(private$valFromDB[id])
                          }
                        },

                        get_valToDB = function(id = NULL) {
                          if (is.null(id)) {
                            return(private$valToDB)
                          } else {
                            return(private$valToDB[id])
                          }
                        }),

                        private = list(
                          name = NULL,         # database name of column
                          nameTable = NULL,    # database name of table which contains the column
                          isPK = NULL,         # Is the column primary key column (TRUE, FALSE)
                          isFK = NULL,         # Is the colum foreign key column (TRUE, FALSE)
                          refTable = NULL,     # If FK, database name of the referenced table (we will assume that the referenced column is                                                   the PK of refTable)
                          refCol = NULL,       # name of the PK column of FK table
                          typeData = NULL,     # Character vector representing the type of data which will be stored in the column (one of the previously set values only)
                          isRequired = NULL,   # Whether the column can be kept empty
                          defaultVal = NULL,   # Default value of the column
                          valFromDB = list(),    # vector of values from database (result of some query)
                          valToDB = list(),      # vector of values from front (to be inserted into database)
                          cacheVal = NULL      # integer denoting number of list of values to be stored
                    ))


