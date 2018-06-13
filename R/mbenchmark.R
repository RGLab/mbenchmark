#' @param x list of objects representing matrix containers that support subsetting method `[` and `dim`. (such as `DelayedArray`)`
#' @param type the type of operations to be measured
#' @param num_threads parallel number
#' @param clear_page_cache whether to flush page cache between iterations
#' @param ... passed
#' @export
#' @examples
#' \dontrun{
#'
#' mat <- matrix(seq_len(2e6), nrow = 1e3, ncol =2e3)
#' smat <- Matrix(mat, sparse = TRUE)
#' mat.list <- list(dense = mat, sparse = smat)
#' mbenchmark(mat.list)
#' autoplot(res)
#' }
mbenchmark <- function(x, type, num_threads, clear_page_cache, ...)UseMethod("mbenchmark")

mbenchmark.list <- function(x, type = c("subsetting", "traversing"), ...) {
  type <- match.arg(type)
  op <- paste0("mbenchmark_", type)
  eval(as.symbol(op))(x, ...)


}

#' @export
autoplot.mbenchmark_traversal<- function(object, ...){
  object <- copy(object)
  object <- object[, time := mean(time)
                   , by = c("task", "dataset")
                   ]
  ggplot(object, aes(x = task, y = time, fill = dataset)) + geom_col(position = "dodge")
  # p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p
}

#' @export
autoplot.mbenchmark_subsetting <- function(object, ...){
  object <- copy(object)
  object <- object[, time := mean(time)
                   , by = c("task", "dataset", "nrow/ncol", "nrow")
                   ]
  object <- object[, `nrow/ncol` := paste0("nrow/ncol = ", `nrow/ncol`)]
  p <-  ggplot(object, aes(x = nrow, y = time, color = dataset)) + geom_point() + geom_line()
  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p <- p + facet_grid(task~`nrow/ncol`)
  p
}

