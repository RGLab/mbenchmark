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

#' @param shape vectors of colsize-to-rowsize ratio, which defines the shape of the selected region
#' @param nstride the number of regions to test. with the dimension size evenly increase up to the ubound
#' @param ubound the upper bound of size of region to select. It is the percentage of the maximum rows
#' @examples
#' mat <- matrix(seq_len(2e6), nrow = 1e3, ncol =2e3)
#' res <- mbenchmark_region_selection(mat, ubound = 1)
#' autoplot(res)
mbenchmark_region_selection <- function(x, shape = c(0.5, 1, 2), nstride = 3, ubound = 0.1,  num_threads = 1, times = 5L, clear_page_cache = FALSE){
  if(!is.list(x))
    x <- list(a = x)

  mat <- x[[1]]
  dims <- dim(mat)
  nrow <- dims[1]
  ncol <- dims[2]
  row_start <- col_start <- 1
  row_stride <- floor(nrow * ubound/nstride)
  res <- ldply(shape, function(ratio){
    col_stride <- floor(row_stride * ratio)
    res <- ldply(seq_len(nstride), function(i)
    {
      # browser()
      row_size <- row_stride * i
      row_end <- row_start + row_size
      ridx <- row_start:row_end
      col_size <- col_stride * i
      col_end <- col_start + col_size
      if(col_end>ncol)
      {
        warning("col idx out of bound!", col_end)
        return(NULL)
      }
      cidx <- col_start:col_end
      # message(row_end, ":", col_end)
      res <- ldply(seq_len(times), function(j)
      {
        if(clear_page_cache)
        {
          stop("clear_page_cache not supported yet!")
        }
        res <- ldply(x, function(mat){
          system.time(v <- mat[ridx, cidx])[["elapsed"]]
        })
        res[["timeid"]] <- j
        res
      })
      res[["row_size"]] <- row_size
      # res[["size"]] <- paste0(row_size, "x", col_size)
      res
    })
    res[["nrow/ncol"]] <- ratio
    res
  })
  attr(res, "class") <- c("region", "mbenchmark",attr(res, "class"))
  res
}

autoplot.mbenchmark <- function(object, ...){
  ggplot(object, aes(x = row_size, y = V1)) + geom_point() + geom_line() + facet_grid(timeid~`nrow/ncol`)
}
