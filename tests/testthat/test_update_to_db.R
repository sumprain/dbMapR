context("Test UPDATE into database")

# create a database table

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

DBI::dbClearResult(src_sq$con)

# create database tree based on dbDatabaseClass

db1 <- dbDatabaseClass$new(src_sq, date_input = "ymd")

tb1 <- db1$get_tables()$table1
cols <- tb1$get_columns()

cols$colint$add_valToDB(1L)
cols$colreal$add_valToDB(1.89)
cols$colchar$add_valToDB("suman")
cols$coldate$add_valToDB("2015/09/09")
cols$colbool$add_valToDB(TRUE)
#cols$coltimestamp$add_valToDB(cur_timestamp())

tb1$insertIntoDB(token_col_name = "coltimestamp")

cols$colint$add_valToDB(123L)
cols$colreal$add_valToDB(1.98)
cols$coldate$add_valToDB("2015/09/30")
cols$colbool$add_valToDB(TRUE)
cols$colchar$add_valToDB("mom")
#cols$coltimestamp$add_valToDB(cur_timestamp())

tb1$insertIntoDB(token_col_name = "coltimestamp")

f_scen1 <- function() {
  tb1$retrieveRowForUpdate(pk_id = 2, token_col_name = "coltimestamp")
  assign("modified_val", 10L, envir = cols$colint$get_updateContainer())
  assign("modified_val", "aarotrika", envir = cols$colchar$get_updateContainer())
  tb1$updateToDB(token_col_name = "coltimestamp")
  return(DBI::dbGetQuery(src_sq$con, "select * from table1")[, -7])
}

df_scen1 <- data.frame(id = c(1, 2), colchar = c("suman", "aarotrika"), colint = c(1, 10), colreal = c(1.89, 1.98), coldate = c("2015-09-09", "2015-09-30"), colbool = c(1, 1), stringsAsFactors = FALSE)

test_that("Record updating is done correctly.", {
  expect_equal(f_scen1(), df_scen1)
  expect_equivalent(cols$colchar$get_queue_valToBeUpdated()$`1`$modified_val, "aarotrika")
  expect_equivalent(cols$colchar$get_queue_valToBeUpdated()$`1`$orig_val, "mom")
  expect_equivalent(cols$colchar$get_queue_valToBeUpdated()$`1`$pk_id, 2)
  expect_null(cols$colint$get_updateContainer()$orig_val)
  expect_null(cols$colint$get_updateContainer()$modified_val)
  expect_null(cols$colint$get_updateContainer()$pk_id)
})

db1$disconnect()
