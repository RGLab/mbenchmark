#' @param x list of objects representing matrix containers that support subsetting method `[` and `dim`. (such as `DelayedArray`)`
#' @param access_pattern the type of operations to be measured
#' @param num_threads parallel number
#' @param clear_page_cache whether to flush page cache between iterations
#' @param ... passed
#' @export
mbenchmark <- function(x, access_pattern, num_threads, clear_page_cache, ...)UseMethod("mbenchmark")

mbenchmark.list <- function(x, access_pattern = c("region_selection", "random_slice", "row_traverse", "col_traverse"), ...) {
  lapply(access_pattern, function(op){
    op <- paste0("mbenchmark_", op)
    do.call(op, list(...))
  })
}

mbenchmark_region_selection <- function(x, shape = c(0.5, 1, 2), nsize = 3, ubound = 0.1,  num_threads = 1, times = 5L, clear_page_cache = FALSE){
  mat <- x[[1]]
  dims <- dim(mat)
  nrow <- dims[1]
  ncol <- dims[2]

  lapply(shape, function(ratio){
    lapply(list, function)
    {
      res <- lapply(1:times, function(i)
      {
        if(clear_page_cache)
        {
          stop("clear_page_cache not supported yet!")
        }
        sapply(x, function(mat){
          system.time(mat)[["elapsed"]]
        })
      })
    })
  })
}
