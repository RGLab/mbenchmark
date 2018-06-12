#' @param shape vectors of colsize-to-rowsize ratio, which defines the shape of the selected region
#' @param nstride the number of regions to test. with the dimension size evenly increase up to the ubound
#' @param ubound the upper bound of size of region to select. It is the percentage of the maximum rows
#' @importFrom plyr ldply
#' @examples
#' mat <- matrix(seq_len(2e6), nrow = 1e3, ncol =2e3)
#' res <- mbenchmark_region_selection(mat, ubound = 1)
#' autoplot(res)
mbenchmark_index <- function(x, type = c("random_slicing", "region_selection"), shape = c(0.5, 1, 2), nsubset = 5, ubound = 0.1,  num_threads = 1, times = 5L, clear_page_cache = FALSE, verbose = TRUE){
  type <- match.arg(type)
  if(!is.list(x))
    x <- list(a = x)

  mat <- x[[1]]
  dims <- dim(mat)
  nrow <- dims[1]
  ncol <- dims[2]
  row_start <- col_start <- 1
  row_stride <- floor(nrow * ubound/nsubset)
  #interate over different subset shapes
  res <- .ldply(shape, function(ratio){
    message("subset shape (nrow / ncol):", ratio)
    col_stride <- floor(row_stride * ratio)

    #iterate over subsets of different sizes
    res <- .ldply(seq_len(nsubset), function(i)
    {
      # browser()
      row_size <- row_stride * i
      if(type == "region_selection")
      {
        row_end <- row_start + row_size - 1
        ridx <- row_start:row_end

      }else
        ridx <- sample(nrow, row_size)

      col_size <- col_stride * i

      if(type == "region_selection")
      {
        col_end <- col_start + col_size - 1
        if(col_end>ncol)
        {
          warning("col idx out of bound!", col_end)
          return(NULL)
        }
        cidx <- col_start:col_end
      }else
        cidx <- sample(ncol, col_size)

      if(type == "region_selection")
        if(verbose)
          message("row = ", row_start, ":", row_end, " col = ", col_start, ":", col_end)

      #iterate multiple times
      res <- .ldply(seq_len(times), function(j)
      {
        if(clear_page_cache)
        {
          stop("clear_page_cache not supported yet!")
        }
        res <- .ldply(x, function(mat){

          system.time(v <- mat[ridx, cidx])[["elapsed"]]
        }, .id = "dataset")
        setnames(res, "dt", "time")
        set(res, j = "timeid", value = j)

      })
      # set(res, j = "size", value = paste0(row_size, "x", col_size))
      set(res, j = "nrow", value = row_size)
      res
    })
    set(res, j = "nrow/ncol", value = ratio)
  })
  attr(res, "class") <- c("index", "mbenchmark",attr(res, "class"))
  res
}
