#' @export
dbDatabaseClass <- R6::R6Class('dbDatabaseClass',   ## TODO: make a mechanism so that only required tables are loaded and later on more tables can be added or deleted to dbClass

                               public = list(
                                 initialize = function(src, tbls_tobe_inserted = NULL, date_input = c("dmy", "mdy", "ymd"), method = c("extract_from_db", "create_from_scratch")) {
                                   date_input <- match.arg(date_input)
                                   method <- match.arg(method)
                                   private$method <- method
                                   self$set_connection(src)
                                   self$set_name(src$info$dbname)

                                   if (private$method == "extract_from_db") {
                                     self$set_nameTables(dplyr::db_list_tables(src$con))
                                     private$populateTables(private$connection, private$nameTables, date_input, private$method)
                                   }
                                 },

                                 set_name = function(name) {
                                   private$name <- name
                                   invisible(self)
                                 },

                                 set_connection = function(src) {
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
                                   DBI::dbDisconnect(private$connection$con)
                                   private$connection <- NULL
                                   return("Successfully disconnected")
                                 }),

                               private = list(
                                 name = NULL,             # name of database
                                 connection = NULL,       # src object from dplyr
                                 tables = list(),         # list of tables in database
                                 nameTables = NULL,        # character vector of names of tables
                                 method = NULL,

                                 populateTables = function(src, nameTables, date_input, method) {
                                   for (i in 1:length(nameTables)) {
                                     private$tables[[nameTables[i]]] <- dbTableClass$new(nameTables[i], method, src, date_input)
                                   }
                                 }
                               ))
