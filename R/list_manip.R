# LIST MANIPULATION FUNCTIONS START --------------------------------------------------

remove_from_list = function(list) {
  return(list[-1])
}

#------------------------------------------------------

add_val_to_list = function(list, val, id, lim) {

  if (length(list) >= lim) {
    list <- remove_from_list(list)
  }

  list[[length(list) + 1]] <- val
  names(list)[length(list)] <- id

  return(list)
}

#----------------------------------------------------

empty_list = function(list) {
  list <- NULL
  return(vector("list"))
}

# LIST MANIPULATION FUNCTIONS END -------------------
