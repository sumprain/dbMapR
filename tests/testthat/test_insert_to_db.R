context("Test INSERT into database")

# check utility functions related to insert ----------------

test_that("nextNumber output is correct", {
  expect_error(nextNumber(NA))
  expect_error(nextNumber("suman"))
  expect_error(nextNumber(c(0,1,2)))
  expect_equal(nextNumber(c(2,3,4)), 1)
  expect_equal(nextNumber(c(1,2,3)), 4)
  expect_equal(nextNumber(c(1,2,4)), 3)
})

test_that("uid produces correct output", {
  expect_is(uid(), "character")
  expect_equal(nchar(uid()), 16L)
  expect_equal(nchar(uid(digits = 8L)), 8L)
})

db_to_r <- function(x) {
  return(change_data_type(match_text(x)))
}

test_that("db --> R data type matching is done correctly", {
  expect_equal(db_to_r("varchar"), "character")
  expect_equal(db_to_r("text"), "character")
  expect_equal(db_to_r("int4"), "integer")
  expect_equal(db_to_r("integer"), "integer")
  expect_equal(db_to_r("date"), "date")
  expect_equal(db_to_r("timestamp"), "character")
  expect_equal(db_to_r("real"), "numeric")
  expect_equal(db_to_r("boolean"), "logical")
  expect_error(db_to_r("suman"))
})

test_that("data parsed correctly before inserting into db", {
  expect_equivalent(parse_val("suman", "character"), "suman")
  expect_equivalent(parse_val(1.09, "numeric"), 1.09)
  expect_equivalent(parse_val(1L, "integer"), 1L)
  expect_equivalent(parse_val("2013/09/08", "date", date_format = "ymd"), as.Date("2013-09-08"))
  expect_equivalent(parse_val(TRUE, "logical"), 1L)
  expect_equivalent(parse_val(FALSE, "logical"), 0L)
})

col1 <- dbColumnClass$new(name = "col1",
                         nameTable = "table1",
                         isPK = 0,
                         isFK = 0,
                         isRequired = 1,
                         defaultVal = 1,
                         typeData = "integer")

col2 <- dbColumnClass$new(name = "col2",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 0,
                          isRequired = 1,
                          typeData = "integer")

col3 <- dbColumnClass$new(name = "col3",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 0,
                          isRequired = 0,
                          typeData = "integer")

col4 <- dbColumnClass$new(name = "col4",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 0,
                          isRequired = 1,
                          typeData = "integer")

col4$add_valToDB(56L)

col5 <- dbColumnClass$new(name = "col5",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 1,
                          isRequired = 0,
                          typeData = "integer")

test_that("required constraint is valid", {
  expect_true(is_nothing_allowed(col1))
  expect_false(is_nothing_allowed(col2))
  expect_true(is_nothing_allowed(col3))
  expect_true(is_nothing_allowed(col4))
  expect_false(is_nothing_allowed(col5))
})

rm(col1, col2, col3, col4, col5)

# validation is correctly done ----------

col1 <- dbColumnClass$new(name = "col1",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 0,
                          isRequired = 1,
                          defaultVal = 1,
                          typeData = "integer")

col1$set_validationStatements(.. > 10, .. <= 20)

f_val1 <- function() {
  col1$add_valToDB(15)
  return(col1$get_valToDB())
}

f_val2 <- function() {
  col1$add_valToDB(25)
  return(col1$get_valToDB())
}

test_that("User defined validation is correctly done.", {
  expect_equivalent(f_val1(), 15L)
  expect_error(f_val2(), "VALIDATION FAILURE FOR RULE")
})

rm(col1)

col1 <- dbColumnClass$new(name = "col1",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 0,
                          isRequired = 1,
                          typeData = "date",
                          date_input = "ymd")

col1$set_validationStatements(.. < lubridate::today())

f_val3 <- function() {
  col1$add_valToDB("2015/09/09")
  return(as.character(col1$get_valToDB()))
}

f_val4 <- function() {
  val <- lubridate::today() + 30
  col1$add_valToDB(val)
  return(col1$get_valToDB())
}

test_that("User defined validation is correctly done (date).", {
  expect_equivalent(f_val3(), "2015-09-09")
  expect_error(f_val4(), "VALIDATION FAILURE FOR RULE")
})

# insert into database -------

src_sq <- dplyr::src_sqlite(path = tempfile(), create = TRUE)

DBI::dbSendQuery(src_sq$con, "CREATE TABLE table1 (
                 id integer PRIMARY KEY,
                 colchar varchar NOT NULL,
                 colint integer NOT NULL DEFAULT 100,
                 colreal real DEFAULT 20.01,
                 coldate date,
                 colbool boolean NOT NULL,
                 coltimestamp char NOT NULL)
                 ")

DBI::dbSendQuery(src_sq$con, "CREATE TABLE table2 (
                 id character PRIMARY KEY,
                 fk integer REFERENCES table1 (id),
                 colchar varchar,
                 coltimestamp char NOT NULL)
                 ")

DBI::dbSendQuery(src_sq$con, "CREATE TABLE table3 (
                 fk_2 character REFERENCES table2 (id),
                 colreal real DEFAULT 1.008,
                 coltimestamp char NOT NULL
)")


DBI::dbClearResult(src_sq$con)

db1 <- dbDatabaseClass$new(src_sq, date_input = "ymd")

tb1 <- db1$get_tables()$table1
cols <- tb1$get_columns()


# scenario1: col is null, required true, no default_val: expect_error

f_scen1 <- function() {

  # colchar is required but has no default and no val: expect_error
  cols$colint$add_valToDB(1L)
  cols$colreal$add_valToDB(1.89)
  cols$coldate$add_valToDB("2015/09/09")
  cols$colbool$add_valToDB(TRUE)
  #cols$coltimestamp$add_valToDB(cur_timestamp())

  tb1$insertIntoDB(token_col_name = "coltimestamp")

}

# scenario2: col is null, required true, default val present: expect_no_error, col_val is default_val

f_scen2 <- function() {

  # colint is null and has default val and is required: expect_no_error and col_val is default_val

  cols$colchar$add_valToDB("suman")
  cols$colint$revert_valToDB_null()
  cols$colreal$add_valToDB(1.89)
  cols$coldate$add_valToDB("2015/09/09")
  cols$colbool$add_valToDB(TRUE)
  #cols$coltimestamp$add_valToDB(cur_timestamp())

  tb1$insertIntoDB(token_col_name = "coltimestamp")

  return(DBI::dbGetQuery(src_sq$con, "select * from table1")[, -7])
}

# scenario3: col is null, required false, default val present: expect_no_error, col val is default val

f_scen3 <- function() {

  # colreal real DEFAULT 20.01
  # colreal is null and is not required but has a default val: expect_no_error, col_val is default val
  cols$colchar$add_valToDB("suman")
  cols$coldate$add_valToDB("2015/09/09")
  cols$colbool$add_valToDB(TRUE)
  #cols$coltimestamp$add_valToDB(cur_timestamp())

  tb1$insertIntoDB(token_col_name = "coltimestamp")

  return(DBI::dbGetQuery(src_sq$con, "select * from table1")[, -7])

}

# scenario4: col is null, required false, default val absent: expect_no_error, col val NA

f_scen4 <- function() {

  # coldate is null and is not required but has no default val: expect_no_error, col_val is NA
  cols$colchar$add_valToDB("suman")
  cols$colint$add_valToDB(12L)
  cols$colreal$add_valToDB(10.09)
  cols$colbool$add_valToDB(TRUE)
  #cols$coltimestamp$add_valToDB(cur_timestamp())

  tb1$insertIntoDB(token_col_name = "coltimestamp")

  return(DBI::dbGetQuery(src_sq$con, "select * from table1")[, -7])

}

# scenario5: foreign key is null: expect_error

tb2 <- db1$get_tables()$table2
cols2 <- tb2$get_columns()

f_scen_fk_null <- function() {
  cols2$colchar$add_valToDB("adhrit")
  #cols2$coltimestamp$add_valToDB(cur_timestamp())

  tb2$insertIntoDB(token_col_name = "coltimestamp")
}

f_scen_fk_not_in_pk <- function() {
  cols2$fk$add_valToDB(10)
  cols2$colchar$add_valToDB("adhrit")
  #cols2$coltimestamp$add_valToDB(cur_timestamp())

  tb2$insertIntoDB(token_col_name = "coltimestamp")
}

f_scen_fk_correct <- function() {
  cols2$fk$add_valToDB(2)
  cols2$colchar$add_valToDB("adhrit")
  #cols2$coltimestamp$add_valToDB(cur_timestamp())

  tb2$insertIntoDB(token_col_name = "coltimestamp")

  return(DBI::dbGetQuery(src_sq$con, "select * from table2")[, -c(1, 4)])
}

tb3 <- db1$get_tables()$table3
cols3 <- tb3$get_columns()

f_scen_nopk <- function() {

  cols3$fk_2$add_valToDB(1)
  cols3$colreal$add_valToDB(2.36)
  tb3$insertIntoDB(token_col_name = "coltimestamp")

}

test_that("Database insert is appropriately done", {
  expect_error(f_scen1())
  expect_equal(f_scen2(), data.frame(id = 1, colchar = "suman", colint = 100, colreal = 1.89, coldate = "2015-09-09", colbool = 1, stringsAsFactors = FALSE))
  expect_equivalent(cols$colchar$get_queue_valToDB()$`1`$val_to_db, "suman")
  expect_equivalent(cols$colchar$get_queue_valToDB()$`1`$pk_id, 1)
  expect_equal(f_scen3(), data.frame(id = c(1, 2), colchar = c("suman", "suman"), colint = c(100, 100), colreal = c(1.89, 20.01), coldate = c("2015-09-09", "2015-09-09"), colbool = c(1, 1), stringsAsFactors = FALSE))
  expect_equivalent(cols$colchar$get_queue_valToDB()$`2`$val_to_db, "suman")
  expect_equivalent(cols$colchar$get_queue_valToDB()$`2`$pk_id, 2)
  expect_equal(f_scen4(), data.frame(id = c(1, 2, 3), colchar = c("suman", "suman", "suman"), colint = c(100, 100, 12), colreal = c(1.89, 20.01, 10.09), coldate = c("2015-09-09", "2015-09-09", NA), colbool = c(1, 1, 1), stringsAsFactors = FALSE))
  expect_error(f_scen_fk_null())
  expect_error(f_scen_fk_not_in_pk())
  expect_equal(f_scen_fk_correct(), data.frame(fk = 2, colchar = "adhrit", stringsAsFactors = FALSE))
  expect_error(f_scen_nopk(), "Table: table3 does not have PK.")
})


db1$disconnect()
