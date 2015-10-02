cur_timestamp <- function(digits = 3L, tz = "Asia/Kolkata") {

  if (!(tz %in% OlsonNames())) {
    stop("Supplied time zone not in recognised list. Check OlsonNames() for the same.", call. = FALSE)
  }

  options(digits.secs = digits)
  res <- format(Sys.time(), "%Y%m%d%H%M%OS")
  options(digits.secs = NULL)

return(structure(paste0(c(res, tz), collapse = "@@"), class = "t_stamp"))

}

as.POSIXct.t_stamp <- function(ts, tz_to_be_set = NULL) {

  ts_split <- strsplit(ts, "@@", fixed = TRUE)[[1]]
  res <- as.POSIXct(ts_split[1], tz = ts_split[2], format = "%Y%m%d%H%M%OS")


  if (!is.null(tz_to_be_set)) {
    res <- lubridate::with_tz(res, tzone = tz_to_be_set)
  }

  return(res)

}

`%earlier%` <- function(ts1, ts2) {

  mod_ts1 <- lubridate::with_tz(as.POSIXct(ts1),tzone = "UTC")
  mod_ts2 <- lubridate::with_tz(as.POSIXct(ts2),tzone = "UTC")

  return(mod_ts1 < mod_ts2)

}

`%later%` <- function(ts1, ts2) {

  mod_ts1 <- lubridate::with_tz(as.POSIXct(ts1),tzone = "UTC")
  mod_ts2 <- lubridate::with_tz(as.POSIXct(ts2),tzone = "UTC")

  return(mod_ts1 > mod_ts2)

}

`%same_time%` <- function(ts1, ts2) {

  mod_ts1 <- lubridate::with_tz(as.POSIXct(ts1),tzone = "UTC")
  mod_ts2 <- lubridate::with_tz(as.POSIXct(ts2),tzone = "UTC")

  return(mod_ts1 == mod_ts2)

}
