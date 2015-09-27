# making a sqlite database

src_sq <- dplyr::src_sqlite("test.sqlite3",create = TRUE)

DBI::dbSendQuery(src_sq$con, "CREATE TABLE table1 (
                 id integer PRIMARY KEY,
                 colchar varchar NOT NULL,
                 colint integer DEFAULT 1,
                 colreal real,
                 coldate date,
                 colbool boolean NOT NULL,
                 coltimestamp timestamp NOT NULL)
                 ")

DBI::dbSendQuery(src_sq$con, "CREATE TABLE table2 (
                 id character PRIMARY KEY,
                 fk integer REFERENCES table1 (id),
                 colchar varchar,
                 coltimestamp timestamp NOT NULL)
                 ")

DBI::dbSendQuery(src_sq$con, "CREATE TABLE table3 (
                fk_2 character REFERENCES table2 (id),
                colreal real DEFAULT 1.008,
                coltimestamp timestamp NOT NULL
                )")



DBI::dbDisconnect(src_sq$con)

dbfrontendR::createTable(src_sq, "parent", types = c(id = "integer", name = "character(100)", dob = "date"), typePK = "id", typeRequired = c("id", "name"))

dbfrontendR::createTable(src_sq, "child", types = c(id = "integer", name = "character(100)", dob = "date", parent_id = "integer"), typePK = "id", typeRequired = c("id", "name", "parent_id"), typeFK = list(parent_id = "parent"), onUpdate = "cascade", onDelete = "noAction")

RSQLite::dbSendQuery(src_sq$con, "INSERT INTO parent (id, name, dob) VALUES (1, 'suman', '2015-01-01')")
RSQLite::dbSendQuery(src_sq$con, "INSERT INTO parent (id, name, dob) VALUES (2, 'mom', '2015-01-02')")

RSQLite::dbSendQuery(src_sq$con, sql("INSERT INTO parent (id, name, dob) VALUES (3, 'binu', '2015-01-03')"))

RSQLite::dbSendQuery(src_sq$con, build_sql(sql("INSERT INTO "), escape("parent"), sql(" "), escape(c("id", "name", "dob")), sql(" VALUES "), escape(list(8L, "raj", "07-44-2016")),con = src_sq$con))

RSQLite::dbGetQuery(src_sq$con, "SELECT * FROM parent")

db1 <- dbDatabaseClass$new(src_sq, date_input = "ymd", method = "extract_from_db")

# insert data

cols <- db1$get_tables()$parent$get_columns()
cols$name$add_valToDB("sss")
cols$dob$add_valToDB("2015/10/10")

db1$get_tables()$parent$insertIntoDB()

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

e$set_a <- functdemangle( "std::map<std::string,double>" )demangle( "std::map<std::string,double>" )demangle( "std::map<std::string,double>" )ion(a) {
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

# understanding date formatting

x <- "2015/09/08"
is.Date(x)

is.Date(ymd(x))

as.Date(35, origin= "2015/09/09") #format = "%d/%m/%Y")


# check speed of various method dispatch system

class_cpos <- R6::R6Class(classname = "class_a1",
                       public = list(
                         initialize = function(name) {
                           self$set_name(name)
                         },
                         set_name = function(name) {
                           private$name <- name
                         },
                         get_name = function() {
                           return(private$name)
                         },

                         print = function() {
                           foo(private$name)
                         }
                       ),
                       private = list(
                         name = NULL
                       )
)

class_cneg <- R6::R6Class(classname = NULL,
                        public = list(
                          initialize = function(name) {
                            self$set_name(name)
                          },
                          set_name = function(name) {
                            private$name <- name
                          },
                          get_name = function() {
                            return(private$name)
                          },

                          print = function() {
                            foo(private$name)
                          }
                        ),
                        private = list(
                          name = NULL
                        )
)


print.class_a1 <- function(obj) {
  return(paste0("my name is ", obj$get_name()))
}

foo <- function(x) {
  return(paste0("my name is ", x))
}
cl1 <- class_cpos$new("suman")
cl2 <- class_cneg$new("suman")

microbenchmark::microbenchmark(cl1$print(),
                               cl2$print(),
                               print(cl1))

# make function to INSERT data into database. It will be called from table class

prepare_cols_for_insertion <- function(src, table) {

  cols <- table$get_nameColumns()
  vals <- setNames(vector("list", length(cols)), cols)

  for (i in cols) {
    coli <- table$get_columns()[[i]]

    # check for PK
    if (coli$isPK() == 1) {
      vals[[i]] <- insert_pk_val(src, coli)
      next()
    }

    # check for FK
    if (coli$isFK() == 1) {
      check_fk_val(src, coli)
    }

    # check for NULL field
    check_for_nothing(coli)


    vals[[i]] <- coli$get_valToDB()
  }

  vals <- lapply(vals, function(x) {
    if (is.null(x) | (is.na(x))) {
      x <- NULL
    }
  })

  return(vals)
}

## make INSERT COLUMN

insert_into_table <- function(list_col_val) {
  cols <- names(list_col_val)

  col_str <- paste0("(", paste0(dplyr::escape(cols), collapse = ", "), ")")
  # the prototype
  # RSQLite::dbSendQuery(src_sq$con, build_sql(sql("INSERT INTO "), escape("parent"), sql(" "), escape(c("id", "name", "dob")), sql(" VALUES "), escape(list(8L, "raj", "07-44-2016")),con = src_sq$con))


}

## insert PK

insert_pk_val <- function(src, col) {

  if (col$get_typeData() == "integer") {
    pkVals <- dplyr::collect(dplyr::tbl(src, col$get_nameTable()) %>% dplyr::select_(.dots = col$get_name()))[[col$get_name()]]
    val <- nextNumber(pkVals)
  } else {
  val <- uid()
  }

  col$set_PKNextVal(val)
  return(val)

}
dplyr::build_sql(dplyr::sql("PRAGMA table_info("), dplyr::escape(tbl_name), dplyr::sql(")"))
## insert FK

check_fk_val <- function(src, col) {

  refTable <- col$get_refTable()
  refCol <- col$get_refCol()

  poss_vals <- dplyr::collect(dplyr::tbl(src, refTable) %>% dplyr::select_(.dots = refCol))

  if (!(col$get_valToDB() %in% poss_vals[[refCol]]))
    stop(paste0("Value of ", col$get_valToDB(), " to be added to ", col$get_name(), " is not contained in PK: ", refTable, "-", refCol))

  invisible(NULL)
}

## check for columns with no values with isRequired and delete the columns if required

check_for_nothing <- function(col) {

  isnull <- is.null(col$get_valToDB())
  isna <- is.na(col$get_valToDB())
  defaultVal <- col$get_defaultVal()
  is_default_available <- !(is.null(defaultVal) | is.na(defaultVal) | defaultVal == "")
  isreqd <- (col$get_isRequired() == 1)
  is_val_nothing <- (isnull | isna)

  if (is_val_nothing && isreqd) {
    if (!is_default_available) {
      stop(paste0("No value is available for ", col$get_name(), " of ", col$get_nameTable()))
    }
  }

  invisible(NULL)
}

## formatting with data type already checked at the time of data entry into add_ValToDB


