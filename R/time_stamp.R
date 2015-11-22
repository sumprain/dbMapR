#' @export
t_stamp <- function(x) {
  UseMethod("t_stamp")
}

#' @export
cur_timestamp <- function(digits = 3L, tz = "Asia/Kolkata") {

  if (!(tz %in% OlsonNames())) {
    stop("Supplied time zone not in recognised list. Check OlsonNames() for the same.", call. = FALSE)
  }

  options(digits.secs = digits)
  res <- format(Sys.time(), "%Y%m%d%H%M%OS")
  options(digits.secs = NULL)

return(structure(paste0(c(res, tz), collapse = "@@"), class = "t_stamp"))

}

#' @export
as.POSIXct.t_stamp <- function(x, tz = NULL, ...) {

  ts_split <- strsplit(x, "@@", fixed = TRUE)[[1]]
  res <- as.POSIXct(ts_split[1], tz = ts_split[2], format = "%Y%m%d%H%M%OS")


  if (!is.null(tz)) {
    res <- lubridate::with_tz(res, tzone = tz)
  }

  return(res)

}

#' @export
t_stamp.character <- function(x) {

  if (check_format_tstamp(x)) {
    class(x) <- "t_stamp"
  } else {
    stop("Format of string not conforming to timestamp. Get timestamp from cur_timestamp() function.")
  }
  x

  }

#' @export
as.character.t_stamp <- function(x, ...) {
  class(x) <- "character"
  x
}

change_to_t_stamp <- function(ts) {

  if (inherits(ts, "character")) {
    ts <- t_stamp(ts)
  }
  ts

}

#' @export
`%earlier%` <- function(ts1, ts2) {

  ts1 <- change_to_t_stamp(ts1)
  ts2 <- change_to_t_stamp(ts2)

  mod_ts1 <- lubridate::with_tz(as.POSIXct(ts1),tzone = "UTC")
  mod_ts2 <- lubridate::with_tz(as.POSIXct(ts2),tzone = "UTC")

  return(mod_ts1 < mod_ts2)

}

#' @export
`%later%` <- function(ts1, ts2) {

  ts1 <- change_to_t_stamp(ts1)
  ts2 <- change_to_t_stamp(ts2)

  mod_ts1 <- lubridate::with_tz(as.POSIXct(ts1),tzone = "UTC")
  mod_ts2 <- lubridate::with_tz(as.POSIXct(ts2),tzone = "UTC")

  return(mod_ts1 > mod_ts2)

}

#' @export
`%same_time%` <- function(ts1, ts2) {

  ts1 <- change_to_t_stamp(ts1)
  ts2 <- change_to_t_stamp(ts2)

  mod_ts1 <- lubridate::with_tz(as.POSIXct(ts1),tzone = "UTC")
  mod_ts2 <- lubridate::with_tz(as.POSIXct(ts2),tzone = "UTC")

  return(mod_ts1 == mod_ts2)

}

#' @export
check_format_tstamp <- function(x) {

  split_pattern <- "@@"
  splitted_str <- unlist(strsplit(x, split_pattern))
  pattern_time <- "(^\\d{14}$)|(^\\d{14}\\.\\d+$)"
  chk_time <- grepl(pattern_time, splitted_str[1])
  chk_tz <- splitted_str[2] %in% OlsonNames()

  chk_time & chk_tz

}
