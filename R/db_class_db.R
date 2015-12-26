#' @export
dbDatabaseClass <- R6::R6Class('dbDatabaseClass',

                               public = list(
                                 initialize = function(src, tbls_tobe_inserted = NULL, date_input = c("ymd", "dmy", "mdy")) {
                                   date_input <- match.arg(date_input)
                                   self$set_connection(src)
                                   self$set_name(src$info$dbname)

                                   self$set_nameTables(dplyr::db_list_tables(src$con))
                                   private$populateTables(private$connection, private$nameTables, date_input)
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
                                 },
                                 
                                 tbls_tobe_kept = function(tbl_names) {
                                   if (!(all(tbl_names %in% private$nameTables))) {
                                     stop("One or more table names to be subsetted are not included in original table name list.", call. = FALSE)
                                   }
                                   private$tables <- private$tables[tbl_names]
                                   self$set_nameTables(tbl_names)
                                   invisible(self)
                                 }),

                               private = list(
                                 name = NULL,             # name of database
                                 connection = NULL,       # src object from dplyr
                                 tables = list(),         # list of tables in database
                                 nameTables = NULL,        # character vector of names of tables
                                 
                                 populateTables = function(src, nameTables, date_input) {
                                   for (i in 1:length(nameTables)) {
                                     private$tables[[nameTables[i]]] <- dbTableClass$new(nameTables[i], src, date_input)
                                   }
                                 }
                               ))
