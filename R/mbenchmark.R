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
#' matlist <- list(dense = mat, sparse = smat)
#' res <- mbenchmark(matlist)
#' autoplot(res)
#'
#' }
mbenchmark <- function(x, type, num_threads, clear_page_cache, ...)UseMethod("mbenchmark")

#' @export
mbenchmark.list <- function(x, type = c("subsetting", "traversing"), ...) {
  type <- match.arg(type)
  op <- paste0("mbenchmark_", type)
  eval(as.symbol(op))(x, ...)


}


#' @export
plot_mem <- function(object, units = "Kb"){
  object <- object[timeid == 1, ]
  object[, mem_change := utils:::format.object_size(mem_change, units = units)]
  object[, mem_change := as.numeric(sub(paste0(" ", units,"$"), "", mem_change))]
  object <- object[, `nrow/ncol` := paste0("nrow/ncol = ", `nrow/ncol`)]
  p <-  ggplot(object, aes(x = nrow, y = mem_change, color = dataset)) + geom_point() + geom_line()
  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p <- p + facet_grid(task~`nrow/ncol`, scales="free")
  p <- p + ylab(paste("Mem", units))
  p
}

drop_page_cahce <- function(){
  cmd <- paste0("sudo bash ", system.file("script/drop_cache.sh", package = "mbenchmark"))
  system(cmd, intern = TRUE)

}
