null_df_to_na <- function(df, col_names) {

  if (is_nothing_df(df)) {
    return(dplyr::as_data_frame(setNames(as.list(rep(NA_character_, length(col_names))), col_names)))
  } else {
    return(df)
  }

}

#========================================================
# sql statement for column information
sql_col_info.src_postgres <- function(src, tbl_name) {
  return(dplyr::build_sql(dplyr::sql("SELECT column_name, is_nullable, udt_name, column_default, character_maximum_length, numeric_precision FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = "), dplyr::escape(tbl_name)))
}

#----------------------------------------------------
sql_col_info.src_sqlite <- function(src, tbl_name) {
  return(dplyr::build_sql(dplyr::sql("PRAGMA table_info("), dplyr::escape(tbl_name), dplyr::sql(")")))
}

#----------------------------------------------------
sql_col_info <- function(src, tbl_name) {
  UseMethod("sql_col_info")
}

#----------------------------------------------------

getColumnInfo <- function(src, tbl_name) {
  UseMethod("getColumnInfo")
}

#----------------------------------------------------

getColumnInfo.src_postgres <- function(src, tbl_name) {

  dfCol <- DBI::dbGetQuery(src$con, sql_col_info(src, tbl_name))

  return(getColumnInfoFinal(dfCol))
}

#----------------------------------------------------

getColumnInfo.src_sqlite <- function(src, tbl_name) {

  dfCol <- DBI::dbGetQuery(src$con, sql_col_info(src, tbl_name))

  dfCol$character_maximum_length <- rep(NA, nrow(dfCol))
  dfCol$numeric_precision <- rep(NA, nrow(dfCol))
  dfCol <- dfCol %>% dplyr::select_("name", "notnull", "type", "dflt_value", "character_maximum_length", "numeric_precision") %>% dplyr::rename_(column_name = ~name, is_nullable = ~notnull, udt_name = ~type, column_default = ~dflt_value) %>% dplyr::mutate_(is_nullable = ~(1*! is_nullable))

  return(getColumnInfoFinal(dfCol))
}

#--------------------------------------------------------
getColumnInfoFinal <- function(dfCol) {

  dfCol <- dfCol %>% dplyr::mutate(var_size = ifelse(is.na(character_maximum_length), numeric_precision, character_maximum_length)) %>% dplyr::select(-character_maximum_length, -numeric_precision)

  return(dfCol)
}

#=========================================================
# sql statements for primary and foreign key information

sql_key_info_pk <- function(src, tbl_name) {
  UseMethod("sql_key_info_pk")
}

sql_key_info_fk <- function(src, tbl_name) {
  UseMethod("sql_key_info_fk")
}

getKeyInfo_pk <- function(src, tbl_name) {
  UseMethod("getKeyInfo_pk")
}

getKeyInfo_fk <- function(src, tbl_name) {
  UseMethod("getKeyInfo_fk")
}

#-------------------------------------------------------

sql_key_info_pk.src_postgres <- function(src, tbl_name) {

  sSQL1 <- "SELECT tc.table_name, kcu.column_name FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = 'PRIMARY KEY' AND tc.table_name = "
  return(dplyr::build_sql(dplyr::sql(sSQL1), dplyr::escape(tbl_name)))
}

sql_key_info_fk.src_postgres <- function(src, tbl_name) {

  sSQL1 <- "SELECT tc.constraint_name, tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name, rc.update_rule As update_rule, rc.delete_rule AS delete_rule FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name JOIN information_schema.referential_constraints AS rc ON rc.constraint_name = tc.constraint_name WHERE constraint_type = 'FOREIGN KEY' AND tc.table_name = "

  return(dplyr::build_sql(dplyr::sql(sSQL1), dplyr::escape(tbl_name)))

}

#------------------------------------------------------

sql_key_info_pk.src_sqlite <- function(src, tbl_name) {

  return(dplyr::build_sql(dplyr::sql("PRAGMA table_info("), dplyr::escape(tbl_name), dplyr::sql(")")))

}

#------------------------------------------------------

sql_key_info_fk.src_sqlite <- function(src, tbl_name) {

  return(dplyr::build_sql(dplyr::sql("PRAGMA foreign_key_list("), dplyr::escape(tbl_name), dplyr::sql(")")))

}

#-----------------------------------------------------------------

getKeyInfo_pk.src_postgres <- function(src, tbl_name) {

  df <- DBI::dbGetQuery(src$con, sql_key_info_pk(src, tbl_name))

  if (is_nothing_df(df)) {
    df <- null_df_to_na(df, c("column_name", "isPK"))
  } else {
    df <- df %>% dplyr::select_("column_name") %>% unique %>% dplyr::mutate_(isPK = ~ rep(1, nrow(.)))
  }

  return(df)

}

getKeyInfo_pk.src_sqlite <- function(src, tbl_name) {

  df <- DBI::dbGetQuery(src$con, sql_key_info_pk(src, tbl_name))

  if (is_nothing_df(df)) {
    df <- null_df_to_na(df, c("column_names", "isPK"))
  } else {
    df <- df %>% dplyr::select_("name", "pk") %>% dplyr::filter_(~pk == 1) %>% dplyr::rename_(column_name = "name", isPK = "pk")
  }
  return(df)
}

#-----------------------------------------------------------------

getKeyInfo_fk.src_postgres <- function(src, tbl_name) {

  df <- DBI::dbGetQuery(src$con, sql_key_info_fk(src, tbl_name))

  if (is_nothing_df((df))) {
    df <- null_df_to_na(df, c("column_name", "foreign_table_name", "foreign_column_name", "update_rule", "delete_rule", "isFK"))
  } else {
    df <- df %>% dplyr::select_("column_name", "foreign_table_name", "foreign_column_name", "update_rule", "delete_rule") %>% unique %>% dplyr::mutate_(isFK = ~ rep(1, nrow(.)))
  }

  return(df)

}

getKeyInfo_fk.src_sqlite <- function(src, tbl_name) {

  df <- DBI::dbGetQuery(src$con, sql_key_info_fk(src, tbl_name))

  if (is_nothing_df((df))) {
    df <- null_df_to_na(df, c("column_name", "foreign_table_name", "foreign_column_name", "update_rule", "delete_rule", "isFK"))
  } else {
    df <- df %>% dplyr::select_("from", "table", "to", "on_update", "on_delete") %>% dplyr::rename_(column_name = "from", foreign_table_name = "table", foreign_column_name = "to", update_rule = "on_update", delete_rule = "on_delete") %>% dplyr::mutate_(isFK = ~ rep(1, nrow(.)))
  }

  return(df)
}
