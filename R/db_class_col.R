#' @export
dbColumnClass <- R6::R6Class('dbColumnClass',
                        public = list(

                          initialize = function(name,
                                                nameTable,
                                                isPK = NULL,
                                                isFK = NULL,
                                                refTable = NULL,
                                                refCol = NULL,
                                                update_rule = NULL,
                                                delete_rule = NULL,
                                                typeData = NULL,
                                                varSize = NULL,
                                                isRequired = NULL,
                                                defaultVal = NULL,
                                                date_input = c("dmy", "mdy", "ymd"),
                                                cacheVal = 5L) {

                            private$date_input <- match.arg(date_input)
                            self$set_name(name)
                            self$set_nameTable(nameTable)
                            self$set_isPK(isPK)
                            self$set_isFK(isFK)
                            self$set_refTable(refTable)
                            self$set_refCol(refCol)
                            self$set_updateRule(update_rule)
                            self$set_deleteRule(delete_rule)
                            self$set_typeData(typeData)
                            self$set_varSize(varSize)
                            self$set_isRequired(isRequired)
                            self$set_defaultVal(defaultVal)
                            self$set_cacheVal(cacheVal)

                            private$queue_valToDB <- queue(max_length = cacheVal)
                            private$queue_valToBeUpdated <- queue(max_length = cacheVal)
                            private$updateContainer <- initiate_updateContainer()

                            invisible(self)
                          },

                        set_name = function(name) {
                            private$name <- as.character(name)
                            invisible(self)
                          },
                        
                        set_label = function(text) {
                          private$label <- as.character(text)
                          invisible(self)
                        },

                        set_nameTable = function(nameTable) {
                            private$nameTable <- as.character(nameTable)
                            invisible(self)
                          },

                        set_isPK = function(isPK) {
                            stopifnot(is.logical(isPK) | isPK %in% c(0, 1))
                            private$isPK <- as.integer(1*isPK)
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
                            private$isFK <- as.integer(1*isFK)
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

                        set_textColFK = function(textColFK) {
                            private$textColFK <- as.character(textColFK)
                            invisible(self)
                          },

                        set_updateRule = function(update_rule) {

                          if (!is.null(update_rule) && (!is.na(update_rule))) {
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

                          if (!is.null(delete_rule) && (!is.na(delete_rule))) {
                            if (!(delete_rule %in% c("CASCADE", "NO ACTION", "SET NULL"))) {
                              warning(paste0("update_rule should be one of the CASCADE, NO ACTION, SET NULL for ", self$get_nameTable(), ": ", self$get_name()))
                              private$deleteRule <- NULL
                            } else {
                              private$deleteRule <- delete_rule
                            }
                          }

                          invisible(self)
                        },

                        set_typeData = function(typeData = c("character", "numeric", "integer", "date", "logical", "timestamp")) {
                            stopifnot(length(typeData) == 1)
                            private$typeData <- match.arg(typeData)
                            invisible(self)
                          },

                        set_varSize = function(varSize) {
                          private$varSize <- as.integer(varSize)
                          invisible(self)
                          },

                        set_isRequired = function(isRequired) {
                            stopifnot(is.logical(isRequired) | isRequired %in% c(0, 1))
                            private$isRequired <- as.integer(1*isRequired)
                            invisible(self)
                          },

                        set_defaultVal = function(defaultVal) {

                          if (is.null(private$typeData)) {
                            private$defaultVal <- as.character(defaultVal)
                          } else if (!is.null(defaultVal)) {
                            private$defaultVal <- corrected_input(defaultVal, self)
                          }
                          invisible(self)
                        
                        },

                        set_validationStatements = function(...) {

                          private$validation_statements <- lazyeval::lazy_dots(...)

                        },

                        add_valToDB = function(val) {

                          private$valToDB <- corrected_input(val, self)

                          invisible(self)
                        },

                        revert_valToDB_null = function() {
                          private$valToDB <- NULL
                          invisible(self)
                        },

                        revert_updateContainer_null = function() {
                          revert_env_null(private$updateContainer)
                          invisible(NULL)
                        },

                        set_cacheVal = function(cacheVal) {
                          private$cacheVal <- cacheVal
                          invisible(self)
                        },

                        set_isSelect = function(isSelect) {
                          stopifnot(is.logical(isSelect) | isSelect %in% c(0, 1))
                          private$isSelect <- as.integer(1*isSelect)
                          invisible(self)
                        },
                      
                        set_selectValCol = function(valCol) {
                          if (is.null(private$typeData)) {
                            private$selectValCol <- as.character(valCol)
                          } else if (!is.null(valCol)) {
                            private$selectValCol <- unlist(lapply(valCol, corrected_input, col = self))
                          }
                          invisible(self)
                        },
                      
                        set_selectTextCol = function(textCol) {
                          private$selectTextCol <- as.character(textCol)
                          invisible(self)
                        },
                      
                        get_name = function() {
                            return(private$name)
                        },
                      
                        get_label = function() {
                          return(private$label)
                        },

                        get_nameTable = function() {
                            return(private$nameTable)
                        },

                        get_date_input = function() {
                          return(private$date_input)
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

                        get_textColFK = function() {
                            return(private$textColFK)
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

                        get_varSize = function() {
                          return(private$varSize)
                        },

                        get_isRequired = function() {
                          return(private$isRequired)
                        },

                        get_defaultVal = function() {
                          return(private$defaultVal)
                        },

                        get_valToDB = function() {
                          return(private$valToDB)
                        },

                        get_queue_valToDB = function() {
                          return(private$queue_valToDB)
                        },

                        get_queue_valToBeUpdated = function() {
                          return(private$queue_valToBeUpdated)
                        },

                        get_updateContainer = function() {
                          return(private$updateContainer)
                        },

                        get_validationStatements = function() {
                          return(private$validation_statements)
                        },
                      
                        get_isSelect = function() {
                          return(private$isSelect)
                        },
                      
                        get_selectValCol = function() {
                          return(private$selectValCol)
                        },
                      
                        get_selectTextCol = function() {
                          return(private$selectTextCol)
                        }

                      ),

                        private = list(
                          name = NULL,         # database name of column
                          nameTable = NULL,    # database name of table which contains the column
                          label = NULL,        # label for shiny front end
                          isPK = NULL,         # Is the column primary key column (1, 0)
                          PKNextVal = NULL,
                          isFK = NULL,         # Is the colum foreign key column (1, 0)
                          refTable = NULL,     # If FK, database name of the referenced table (we will assume that the referenced column is                                                   the PK of refTable)
                          refCol = NULL,       # name of the PK column of FK table
                          textColFK = NULL,    # name of the column of the table for displaying text for FK
                          updateRule = NULL,
                          deleteRule = NULL,
                          typeData = NULL,     # Character vector representing the type of data which will be stored in the column (one of the previously set values only)
                          varSize =  NULL,
                          isRequired = NULL,   # Whether the column can be kept empty (1, 0)
                          defaultVal = NULL,   # Default value of the column
                          validation_statements = NULL,
                          cacheVal = NULL,      # integer denoting number of list of values to be stored
                          valToDB = NULL,      # vector of values from front (to be inserted into database)
                          queue_valToDB = NULL,
                          queue_valToBeUpdated = NULL,
                          updateContainer = NULL,
                          date_input = NULL,
                          isSelect = NULL,
                          selectValCol = NULL,   # if select widget, vector of values
                          selectTextCol = NULL   # if select widget, vector of texts
                    ))


