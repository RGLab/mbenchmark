#' Convert a list to a data.table
#'
#' data.table version of ldply
#' @param .data a list
#' @import data.table
#' @return a data.table
.ldply <- function (.data,  ..., .id = NA)
{
  index <- names(.data)
  if(is.null(index)){
    index <- seq_along(.data)
    .id <- NULL
  }


  res <- .do_loop(index = index, .data = .data, ..., .id = .id)
  res <- rbindlist(res)
  # setkeyv(res, .id)
  res
}

#' @param index the index of the list, can be either character or numeric
#' @param  .fun the function to apply to each element of .data
#' @param ... other arguments passed to .fun
#' @param .id see help(ldply)
.do_loop <- function(index, .data, .fun = NULL, ..., .id = NA){

  lapply(index, function(i){

    dt <- .fun(.data[[i]], ...)
    dt <- as.data.table(dt)
    #append id
    if(!is.null(.id)){
      if (is.na(.id)) {
        .id <- ".id"
      }
      set(dt, j = .id, value = i)
    }
    dt
  })
}