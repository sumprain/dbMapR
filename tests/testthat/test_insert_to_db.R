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
                         typeData = "integer",
                         method = "extract_from_db")

col2 <- dbColumnClass$new(name = "col2",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 0,
                          isRequired = 1,
                          typeData = "integer",
                          method = "extract_from_db")

col3 <- dbColumnClass$new(name = "col3",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 0,
                          isRequired = 0,
                          typeData = "integer",
                          method = "extract_from_db")

col4 <- dbColumnClass$new(name = "col4",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 0,
                          isRequired = 1,
                          typeData = "integer",
                          method = "extract_from_db")

col4$add_valToDB(56L)

col5 <- dbColumnClass$new(name = "col5",
                          nameTable = "table1",
                          isPK = 0,
                          isFK = 1,
                          isRequired = 0,
                          typeData = "integer",
                          method = "extract_from_db")

test_that("required constraint is valid", {
  expect_true(is_nothing_allowed(col1))
  expect_false(is_nothing_allowed(col2))
  expect_true(is_nothing_allowed(col3))
  expect_true(is_nothing_allowed(col4))
  expect_false(is_nothing_allowed(col5))
})

rm(col1, col2, col3, col4, col5)

# insert into database -------

src_sq <- dplyr::src_sqlite(path = tempfile(), create = TRUE)

DBI::dbSendQuery(src_sq$con, "CREATE TABLE table1 (
                 id integer PRIMARY KEY,
                 colchar varchar NOT NULL,
                 colint integer DEFAULT 1,
                 colreal real,
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

db1 <- dbDatabaseClass$new(src_sq, method = "extract_from_db", date_input = "ymd")

tb1 <- db1$get_tables()$table1
cols <- tb1$get_columns()

cols$colchar$add_valToDB("suman")
cols$colint$add_valToDB(1L)
cols$colreal$add_valToDB(1.89)
cols$coldate$add_valToDB("2015/09/09")
cols$colbool$add_valToDB(TRUE)
cols$coltimestamp$add_valToDB(cur_timestamp())

tb1$insertIntoDB()

cols$colchar$add_valToDB("mom")
cols$colreal$add_valToDB(1.89)
cols$coldate$add_valToDB("2015/09/09")
cols$colbool$add_valToDB(TRUE)
cols$coltimestamp$add_valToDB(cur_timestamp())

tb1$insertIntoDB()

tb2 <- db1$get_tables()$table2
cols2 <- tb2$get_columns()

cols2$fk$add_valToDB(2)
cols2$colchar$add_valToDB("adhrit")
cols2$coltimestamp$add_valToDB(cur_timestamp())

tb2$insertIntoDB()

cols2$fk$add_valToDB(2)
cols2$colchar$add_valToDB("aarotrika")
cols2$coltimestamp$add_valToDB(cur_timestamp())
tb2$insertIntoDB()
cols2$fk$add_valToDB(2)
cols2$coltimestamp$add_valToDB(cur_timestamp())
tb2$insertIntoDB()

db1$disconnect()
