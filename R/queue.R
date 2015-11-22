#' @export
queue <- function(max_length = 5L, parent = emptyenv()) {
  e <- new.env(hash = TRUE, parent = parent)
  attr(e, "max_length") <- max_length
  return(structure(e, class = "queue"))
}

#' @export
push <- function(queue, value) {
  UseMethod("push")
}

#' @export
empty <- function(queue) {
  UseMethod("empty")
}

#' @export
sorted_index <- function(queue) {
  UseMethod("sorted_index")
}

#' @export
min_index <- function(queue) {
  UseMethod("min_index")
}

#' @export
max_index <- function(queue) {
  UseMethod("max_index")
}

#' @export
remove <- function(queue, index) {
  UseMethod("remove")
}

#' @export
counter <- function(queue) {
  UseMethod("counter")
}

#' @export
push.queue <- function(queue, value) {

  max_length <- attr(queue, "max_length")

  if (length(queue) >= max_length) {
    remove(queue, min_index(queue))
  }

  queue[[counter(queue)]] <- value

  invisible(queue)
}

#' @export
empty.queue <- function(queue) {
  rm(list = ls(envir = queue), envir = queue)
  invisible(queue)
}

#' @export
length.queue <- function(x) {
  return(length(ls(envir = x)))
}

#' @export
sorted_index.queue <- function(queue) {

  ind <- ls(envir = queue)
  #ind_POSIX <- as.POSIXct.t_stamp(ind)

  return(as.character(sort(as.integer(ind))))
}

#' @export
min_index.queue <- function(queue) {
  return(sorted_index(queue)[1])
}

#' @export
max_index.queue <- function(queue) {
  return(sorted_index(queue)[length(queue)])
}

#' @export
remove.queue <- function(queue, index) {
  base::remove(list = index, envir = queue)
  invisible(queue)
}

#' @export
counter.queue <- function(queue) {

  if (!length(queue)) {
    return(as.character(1L))
  } else {
    return(as.character(as.integer(max_index(queue)) + 1L))
  }
}
