context("Test database connection and structure")

# create new db on fly----------------

src_sq <- dplyr::src_sqlite(path = tempfile(), create = TRUE)

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

DBI::dbClearResult(src_sq$con)

test_that("Database is of SQLite", {
  expect_is(src_sq, "src_sqlite")
})

# create new database class --------------

db1 <- dbMapR::dbDatabaseClass$new(src_sq, date_input = "ymd",
                                        method = "extract_from_db")

# check contents of database object

test_that("Database structure is correct", {
  expect_equal(db1$get_nameTables(), c("table1", "table2", "table3"))
  expect_is(db1$get_tables()[[1]], "R6")

})

# check contents of table1 object
tb1 <- db1$get_tables()$table1

test_that("table1 structure is correct", {
  expect_equal(tb1$get_name(), "table1")
  expect_equal(tb1$get_nameColumns(), c("id", "colchar", "colint", "colreal", "coldate", "colbool", "coltimestamp"))
  expect_equal(tb1$get_PKColumn(), "id")
  expect_equal(tb1$get_dfForeignKey()$column_name, NA_character_)
})

# check contents of table2 object

tb2 <- db1$get_tables()$table2

test_that("table2 contains foreign key", {
  expect_equal(tb2$get_name(), "table2")
  expect_equal(tb2$get_PKColumn(), "id")
  expect_is(tb2$get_dfForeignKey(), "data.frame")
  expect_equal(tb2$get_dfForeignKey()$column_name, "fk")
})

# check correctness of master class

tb2$set_masterTable("table1")
test_that("correct master table is assigned", {
  expect_equal(tb2$get_masterTable(), "table1")
  expect_error(tb2$set_masterTable("table3"), "Suggested table name is not amongst the list of linked tables.")
})

tb2$set_masterTable()
test_that("NULL master table is assigned", {
  expect_null(tb2$get_masterTable())
})

# check contents of table3

tb3 <- db1$get_tables()$table3

test_that("table3 does not have any primary key", {
  expect_equal(tb3$get_name(), "table3")
  expect_equal(tb3$get_PKColumn(), character(0L))
})

# check datatypes columns of table1

data_tb1 <- vapply(tb1$get_columns(), function(x) x$get_typeData(), FUN.VALUE = character(1L))

test_that("table1 data types are correct", {
  expect_equal(unname(data_tb1), c("integer", "character", "integer", "numeric", "date", "logical", "character"))
})

# check table1 default values

dflt_val_tb1 <- sapply(tb1$get_columns(), function(x) x$get_defaultVal())

test_that("table1 has correct default values", {
  expect_equal(unname(dflt_val_tb1), c(NA_character_, NA_character_, "1", NA_character_, NA_character_, NA_character_, NA_character_))
})

# check table1 is required values

isreqd_tb1 <- vapply(tb1$get_columns(), function(x) x$get_isRequired(), integer(1L))

test_that("table1 has correct is required fields", {
  expect_equal(unname(isreqd_tb1), c(0, 1, 0, 0, 0, 1, 1))
})

# check whether column has appropriate data type for valCol (select widget)

col <- tb1$get_columns()$colint
col$set_selectValCol(c(1,2,3))

test_that("table1 colint has appropriate datatype for selectValCol", {
  expect_true(inherits(col$get_selectValCol(), "integer"))
  expect_error(col$set_selectValCol(letters[1:4]))
})

# check whether table subsetting is done correctly in cloned database

db2 <- db1$clone()
tbl_to_keep <- c("table1", "table2")
db2$tbls_tobe_kept(tbl_to_keep)

test_that("db2 is properly cloned from db1", {
  expect_equal(length(db2$get_tables()), 2L)
  expect_equal(length(db1$get_tables()), 3L)
  expect_equal(db2$get_nameTables(), c("table1", "table2"))
  expect_equal(db1$get_nameTables(), c("table1", "table2", "table3"))
})

# check whether database connection is removed successfully

test_that("Database is successfully disconnected", {
  expect_equal(db1$disconnect(), "Successfully disconnected")
})

