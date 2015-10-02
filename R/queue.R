queue <- function(max_length = 5L, parent = emptyenv()) {
  e <- new.env(hash = TRUE, parent = parent)
  attr(e, "max_length") <- max_length
  return(structure(e, class = "queue"))
}

push <- function(queue, value) {
  UseMethod("push")
}

empty <- function(queue) {
  UseMethod("empty")
}

sorted_index <- function(queue) {
  UseMethod("sorted_index")
}

min_index <- function(queue) {
  UseMethod("min_index")
}

max_index <- function(queue) {
  UseMethod("max_index")
}

remove <- function(queue, index) {
  UseMethod("remove")
}

counter <- function(queue) {
  UseMethod("counter")
}

push.queue <- function(queue, value) {

  max_length <- attr(queue, "max_length")

  if (length(queue) >= max_length) {
    remove(queue, min_index(queue))
  }

  queue[[counter(queue)]] <- value

  invisible(queue)
}

empty.queue <- function(queue) {
  rm(list = ls(envir = queue), envir = queue)
  invisible(queue)
}

length.queue <- function(queue) {
  return(length(ls(envir = queue)))
}

sorted_index.queue <- function(queue) {

  ind <- ls(envir = queue)
  #ind_POSIX <- as.POSIXct.t_stamp(ind)

  return(as.character(sort(as.integer(ind))))
}

min_index.queue <- function(queue) {
  return(sorted_index(queue)[1])
}

max_index.queue <- function(queue) {
  return(sorted_index(queue)[length(queue)])
}

remove.queue <- function(queue, index) {
  base::remove(list = index, envir = queue)
  invisible(queue)
}

counter.queue <- function(queue) {

  if (!length(queue)) {
    return(as.character(1L))
  } else {
    return(as.character(as.integer(max_index(queue)) + 1L))
  }
}
