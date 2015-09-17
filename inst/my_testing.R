foo <- function(x = c("a", "b")) {
  #browser()
  x <- match.arg(x,several.ok = TRUE)
  return(x)
}

foo <- function(type = c("aa", "xb", "c", "d")) {
  type <- match.arg(type)
  return(switch(type,
         a = 1,
         b = 2,
         c = ,
         d = 0))
}

# running predefined functions on each of the  arguments programatically------
foo <- function(a = NULL, b = NULL, c = NULL) {

  l_args <- as.list(match.call())
  #browser()
  if (length(l_args) == 1)
    return(NULL)

  l_args[1] <- NULL

  #browser()
  for (i in 1:length(l_args)) {
    eval(call(paste0("set_", names(l_args)[i]), l_args[[i]]),envir = e)
  }

}

e <- new.env(parent = emptyenv())

e$set_a <- function(a) {
  print(paste("a:", a))
}

e$set_b <- function(b) {
  print(paste("b:", b))
}

e$set_c <- function(c) {
  print(paste("c:", c))
}

# list primary and foreign key constraint ----------

#SELECT tc.constraint_name, tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = 'FOREIGN KEY' AND tc.table_name='mytable';

# list column information -------------------------

# SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = ", dplyr::escape(x))


# check the interdependancy between two classses ---------------------------

class_a <- R6::R6Class("class_a",
                       public = list(
                         initialize = function(name) {
                           self$set_name(name)
                         },
                         set_name = function(name) {
                           private$name <- name
                         },
                         get_name = function() {
                           return(private$name)
                         }
                       ),
                       private = list(
                         name = NULL
                       )
                       )

class_b <- R6::R6Class("class_b",
                       public = list(
                         make_class_a = function(num = 2) {
                           #browser()
                           for (i in 1:num) {
                             private$classes_a[[i]] <- class_a$new(paste0("classA", i))
                             print(private$classes_a[[i]])
                           }

                         },

                         get_classes_a = function() {
                           return(private$classes_a)
                         }
                       ), private = list(
                         classes_a = list()
                       ))


print.class_a <- function(obj) {
  print(as.character(obj$get_name()))
}

# checking which of the following is faster

df <- data_frame(x = 1:3, y = 3:5)

microbenchmark::microbenchmark(df %>% mutate(z = rep(1, nrow(df))),
                               df %>% mutate(z = rep(1, length(df$x))))

# making function for partial matching

match_text <- function(text, to_be_matched_against) {

  foo <- function(text) {
    function(matched_against) {
      if (grepl(pattern = matched_against, text)) {
        return(TRUE)
      } else return(FALSE)
    }
  }

  res <- sapply(to_be_matched_against, foo(text))
  return(names(res)[res])
}

# left_join on NULL dataframes

dfx <- data_frame(a = 1:5, b = letters[1:5])
dfy <- data_frame()
bind_rows(dfy, data_frame(a = NA, c = NA))
left_join(dfx, dfy, by = c("a" = "a"))


# getting on update and on delete rules

# information_schema.referential_constraints

## constraint_name
## update_rule, delete_rule

src <- src_postgres(dbname = "patient_hemat",user = "mom",password = "docpapu2001")
sSQL1 <- "SELECT tc.constraint_name, tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = "

sSQL1 <- "SELECT tc.constraint_name, tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name, rc.update_rule As update_rule, rc.delete_rule AS delete_rule FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name JOIN information_schema.referential_constraints AS rc ON rc.constraint_name = tc.constraint_name WHERE constraint_type = "

sSQL2 <- " AND tc.table_name = "

tbls <- db_list_tables(src$con)

for (i in tbls) {
  print(RPostgreSQL::dbGetQuery(src$con, paste0(sSQL1, dplyr::escape("FOREIGN KEY"), sSQL2, dplyr::escape(i))))
  cat("\n\n")
}
