#' @export
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
