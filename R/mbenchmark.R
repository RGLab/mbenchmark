#' @param x list of objects representing matrix containers that support subsetting method `[` and `dim`. (such as `DelayedArray`)`
#' @param tasks the type of operations to be measured
#' @param num_threads parallel number
#' @param clear_page_cache whether to flush page cache between iterations
#' @param ... passed
#' @export
mbenchmark <- function(x, tasks, num_threads, clear_page_cache, ...)UseMethod("mbenchmark")

mbenchmark.list <- function(x, tasks = c("region_selection", "random_slicing", "row_traversal", "col_traversal"), ...) {
  lapply(tasks, function(op){
    op <- paste0("mbenchmark_", op)
    do.call(op, list(...))
  })
}

#' @examples
#' mat <- matrix(seq_len(2e6), nrow = 1e3, ncol =2e3)
#' res <- mbenchmark_region_selection(mat, ubound = 1)
#' autoplot(res)
#' smat <- Matrix(mat, sparse = TRUE)
#' res <- mbenchmark_random_slicing(list(dense = mat, sparse = smat), ubound = 0.5)
#' autoplot(res)
#' @export
mbenchmark_region_selection <- function(x, ...){
  mbenchmark_index(x, type = "region_selection", ...)
}

#' @export
mbenchmark_random_slicing <- function(x, ...){
  mbenchmark_index(x, type = "random_slicing", ...)
}
autoplot.mbenchmark <- function(object, ...){
  object <- object[, time := mean(time)
                   , by = c("dataset", "nrow/ncol", "nrow")
                   ]
 p <-  ggplot(object, aes(x = nrow, y = time, color = dataset)) + geom_point() + geom_line()
 p <- p + facet_wrap(~`nrow/ncol`)
 p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
}

