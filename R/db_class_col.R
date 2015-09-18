#' @export
dbColumnClass <- R6::R6Class('dbColumnClass',
                        public = list(

                          initialize = function(name,
                                                nameTable,
                                                isPK = NULL,
                                                PKNextVal = NULL,
                                                isFK = NULL,
                                                refTable = NULL,
                                                refCol = NULL,
                                                update_rule = NULL,
                                                delete_rule = NULL,
                                                typeData = NULL,
                                                isRequired = NULL,
                                                defaultVal = NULL,
                                                date_input = c("dmy", "mdy", "ymd"),
                                                cacheVal = 5L) {

                            private$date_input <- match.arg(date_input)
                            self$set_name(name)
                            self$set_nameTable(nameTable)
                            self$set_isPK(isPK)
                            self$set_PKNextVal(PKNextVal)
                            self$set_isFK(isFK)
                            self$set_refTable(refTable)
                            self$set_refCol(refCol)
                            self$set_updateRule(update_rule)
                            self$set_deleteRule(delete_rule)
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

                        set_PKNextVal = function(PKNextVal) {
                          if (self$get_isPK() == 1) {
                            private$PKNextVal <- PKNextVal
                          }
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

                        set_updateRule = function(update_rule) {

                          if (!is.null(update_rule) && (update_rule != "")) {
                            if (!(update_rule %in% c("CASCADE", "NO ACTION", "SET NULL"))) {
                              warning(paste0("update_rule should be one of the CASCADE, NO ACTION, SET NULL for ", self$get_nameTable(), ": ", self$get_name()))
                              private$updateRule <- NULL
                            } else {
                              private$updateRule <- update_rule
                            }
                          }

                          invisible(self)
                        },

                      set_deleteRule = function(delete_rule) {

                          if (!is.null(delete_rule) && (delete_rule != "")) {
                            if (!(delete_rule %in% c("CASCADE", "NO ACTION", "SET NULL"))) {
                              warning(paste0("update_rule should be one of the CASCADE, NO ACTION, SET NULL for ", self$get_nameTable(), ": ", self$get_name()))
                              private$deleteRule <- NULL
                            } else {
                              private$deleteRule <- delete_rule
                            }
                          }

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

                          val <- switch(private$typeData,
                                        date = formatted_date(val, private$date_input),
                                        numeric = as.numeric(val),
                                        integer = as.integer(val),
                                        character = as.character(val),
                                        logical = as.logical(val))

                          private$valToDB <- val
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

                        get_PKNextVal = function() {
                            return(private$PKNextVal)
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

                        get_updateRule = function() {
                          return(private$updateRule)
                        },

                        get_deleteRule = function() {
                          return(private$deleteRule)
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

                        get_valToDB = function() {
                          return(private$valToDB)
                        }),

                        private = list(
                          name = NULL,         # database name of column
                          nameTable = NULL,    # database name of table which contains the column
                          isPK = NULL,         # Is the column primary key column (TRUE, FALSE)
                          PKNextVal = NULL,
                          isFK = NULL,         # Is the colum foreign key column (TRUE, FALSE)
                          refTable = NULL,     # If FK, database name of the referenced table (we will assume that the referenced column is                                                   the PK of refTable)
                          refCol = NULL,       # name of the PK column of FK table
                          updateRule = NULL,
                          deleteRule = NULL,
                          typeData = NULL,     # Character vector representing the type of data which will be stored in the column (one of the previously set values only)
                          isRequired = NULL,   # Whether the column can be kept empty
                          defaultVal = NULL,   # Default value of the column
                          valFromDB = list(),    # vector of values from database (result of some query)
                          valToDB = NULL,      # vector of values from front (to be inserted into database)
                          cacheVal = NULL,      # integer denoting number of list of values to be stored
                          date_input = NULL
                    ))


