context("Test connection")

# make a new sqlite database on the fly

src <- dplyr::src_sqlite(path = tempfile(), create = TRUE)
parTbl <- dbfrontendR::createTable(src, table = "parent", types = c(id = "integer", name = "character"), typePK = "id")
childTbl <- dbfrontendR::createTable(src, table = "children", types = c(id = "integer", par_id = "integer", name = "character"), typePK = "id", typeRequired = "par_id", typeFK = list(par_id = "parent"), onUpdate = "cascade", onDelete = "cascade")

test_that("Tables are created successfully", {

  expect_that(as.character(parTbl), equals('CREATE TABLE "parent" (id integer PRIMARY KEY, name character)'))
  expect_that(as.character(childTbl), equals('CREATE TABLE "children" (id integer PRIMARY KEY, par_id integer NOT NULL REFERENCES parent ON UPDATE CASCADE ON DELETE CASCADE, name character)'))
})

# fill up the tables with hypothetical data

dataPar <- data.frame(id = c(1,2,3,5,6), name = letters[1:5], stringsAsFactors = FALSE)
dataChild <- data.frame(id = c(1:7), par_id = c(1,1,2,3,6,6,6), name = LETTERS[1:7], stringsAsFactors = FALSE)

isSuccessWritePar <- RSQLite::dbWriteTable(src$con, "parent", dataPar, append = TRUE)
isSuccessWriteChild <- RSQLite::dbWriteTable(src$con, "children", dataChild, append = TRUE)


test_that("Tables are filled successfully with dbWriteTable", {

  expect_true(isSuccessWritePar)
  expect_true(isSuccessWriteChild)
})

# extract next primary key

test_that("PK details are appropriately extracted", {
  PKPar <- dbfrontendR::getPK(src, "parent")
  PKChild <- dbfrontendR::getPK(src, "children")
  expect_equal(PKPar$column_name, "id")
  expect_equal(PKChild$column_name, "id")
  expect_equal(PKPar$data_type, "integer")
  expect_equal(PKChild$data_type, "integer")
  expect_equal(PKPar$nextVal, 4)
  expect_equal(PKChild$nextVal, 8)
})

RSQLite::dbDisconnect(src$con)

