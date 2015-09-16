#' create table in SQLite and postgresql databases
#'
#' @param src It is the connection as done by \code{dplyr} \code{src_*} group of
#'   methods.
#' @param table Name if table. The function will raise error if table name
#'   already exists.
#' @param types Named vector of column names and type. For \code{character} data
#'   type, we can add \code{"character(30)"} to denote length of column. Common
#'   types are \code{"integer", "numeric", "character", "date"}. We can also add
#'   \code{"TIMESTAMP"} and \code{"SERIAL"} data type in postgresql for
#'   \code{timestamp, with default CURRENT_TIMESTAMP} and \code{primary key with
#'   autonumbering}.
#' @param temporary (default \code{FALSE}). It denotes whether the table is a
#'   temporary one.
#' @param typeUnique character vector denoting the columns with only unique
#'   values allowed.
#' @param typePK character vector (usually, single string) denoting the Primary
#'   Key of the table.
#' @param typeRequired character vector denoting the columns which cannot be
#'   left empty.
#' @param typeDefault named list with list(column1 = "defaultVal1", column2 =
#'   "defaultval2", ...) format.
#' @param typeFK named list of the format list(fk_column1 = "tableName1withPK",
#'   fk_column2 = "tableName2withPK", ...)
#' @param onUpdate to be used only if foreign key is defined. One of the
#'   following options: \code{"cascade", "noAction", "setNull"}.
#' @param onDelete to be used only if foreign key is defined. One of the
#'   following options: \code{"cascade", "noAction", "setNull"}.
#' @return Resulting SQL statement.
#'
#' @examples
#' \dontrun{
#' createTable(src, table = "tbl1", types = c(id = "numeric", name =
#' "character(50)", address = "character(50)", age = "numeric"), temporary = F,
#' typeUnique = c("name", "address"), typePK = "id", typeRequired = "name",
#' typeDefault = list(age = 20, name = "mee"))

#'
#' createTable(src, table = "tbl2", types = c(id = "numeric", fk_id  =
#' "numeric", name = "character(50)", address = "character(50)", age =
#' "numeric"), temporary = F, typeUnique = "name", typePK = "id", typeRequired =
#' c("fk_id", "name"), typeDefault = list(age = 20), typeFK = list(fk_id =
#' "tbl1"), onUpdate = "setNull", onDelete = "cascade")
#'}
#'
#' @export
createTable <- function(src, table, types, temporary = FALSE, typeUnique = NULL, typePK = NULL, typeRequired = NULL, typeDefault = NULL, typeFK = NULL, ...) {

  assertthat::assert_that(assertthat::is.string(table), is.character(types))
  con <- src$con
  name_vec <- names(types)

  assertthat::assert_that(anyDuplicated(name_vec) == 0) # asserts that field names are unique.

  field_names <- dplyr::escape(dplyr::sql(name_vec), collapse = NULL, parens = FALSE, con = con)

  #browser()
  field_unique <- if (!is.null(typeUnique)) {
    assertthat::assert_that(is.character(typeUnique))
    create_field_vec(con, name_vec, to.change = typeUnique, iden.type = "unique")
  } else NULL
  #browser()
  field_pk <- if (!is.null(typePK)) {
    assertthat::assert_that(is.character(typePK))
    create_field_vec(con, name_vec, to.change = typePK, iden.type = "pk")
  } else NULL

  field_req <- if (!is.null(typeRequired)) {
    assertthat::assert_that(is.character(typeRequired))
    create_field_vec(con, name_vec, to.change = typeRequired, iden.type = "required")
  } else NULL

  field_default <- if (!is.null(typeDefault)) {
    assertthat::assert_that(is.list(typeDefault))
    create_field_vec(con, name_vec, to.change = typeDefault, iden.type = "default")
  } else NULL
  #browser()
  field_fk <- if(!is.null(typeFK)) {
    assertthat::assert_that(is.list(typeFK))
    create_field_vec(con, name_vec, to.change = typeFK, iden.type = "fk", ...)
  }

  #browser()

  field_int <- paste(field_names, types, field_pk, field_req, field_unique, field_default, field_fk, sep = " ")
  field_int <- gsub(pattern = "^\\s+|\\s+$", replacement = "", field_int)
  field_int <- gsub(pattern = "\\s+", replacement = " ", x = field_int)

  fields <- sql_vector(field_int, parens = TRUE, collapse = ", ", con = con)

  sql <- dplyr::build_sql("CREATE ", if (temporary) dplyr::sql("TEMPORARY "),
                   "TABLE ", dplyr::ident(table), " ", fields, con = con)

  #browser()
  if (inherits(src, "src_sqlite")) {
    RSQLite::dbGetQuery(con, sql)
  } else if (inherits(src, "src_postgres")) {
    RPostgreSQL::dbGetQuery(con, sql)
  }

  return(c(sql_statement = sql))

}

#--------------------------------------------

create_field_vec <- function(con, field.names, to.change, iden.type = c("required", "unique", "pk", "default", "fk"), onUpdate = if (type == "fk") c("cascade", "noAction", "setNull") else NULL, onDelete = if (type == "fk") c("cascade", "noAction", "setNull") else NULL) {

  #browser()
  type <- match.arg(iden.type)

  if (type %in% c("default", "fk")) {
    assertthat::assert_that(is.list(to.change))
    assertthat::assert_that(setequal(union(field.names, names(to.change)), field.names))
  }  else {
    assertthat::assert_that(setequal(union(field.names, to.change), field.names))  # names to be changed are present in field.names.
  }

  alternatives <- switch(type,
                         required = c("NOT NULL", ""),
                         unique = c("UNIQUE", ""),
                         pk = c("PRIMARY KEY", ""))

  opt_fk <- function(x) {
    r <- switch(x,
                cascade = "CASCADE",
                noAction = "NO ACTION",
                setNull = "SET NULL")
    r
  }

  onUpd <- if (!is.null(onUpdate)) {
    upd <- onUpdate
    paste0("ON UPDATE ", opt_fk(upd))
  } else NULL

  onDel <- if (!is.null(onDelete)) {
    del <- onDelete
    paste0("ON DELETE ", opt_fk(del))
  } else NULL

  res <- vector("character", length(field.names))
  names(res) <- field.names

  if (type %in% c("required", "unique", "pk")) {
    res[field.names %in% to.change] <- alternatives[1L]
    res[!(field.names %in% to.change)] <- alternatives[2L]
  } else if (type == "default") {
    res[names(to.change)] <- vapply(to.change, function(x) paste0("DEFAULT ", dplyr::escape(x)), FUN.VALUE = character(1L))
  } else if (type == "fk") {
    #browser()
    res[names(to.change)] <- vapply(to.change, function(x) paste0("REFERENCES ", dplyr::sql(x), " ", dplyr::sql(onUpd), " ", dplyr::sql(onDel)), FUN.VALUE = character(1L))
  }
  #browser()
  return(dplyr::sql(res))
}
