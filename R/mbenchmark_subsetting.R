#' @param shape vectors of colsize-to-rowsize ratio, which defines the shape of the selected region
#' @param nstride the number of regions to test. with the dimension size evenly increase up to the ubound
#' @param ubound the upper bound of size of region to select. It is the percentage of the maximum rows
#' @examples
#' \dontrun{
#' library(Matrix)
#' mat <- matrix(seq_len(2e6), nrow = 1e3, ncol =2e3)
#' smat <- Matrix(mat, sparse = TRUE)
#' mat.list <- list(dense = mat, sparse = smat)
#' res <- mbenchmark_subsetting(mat.list, times = 2, nsubset = 3)
#' autoplot(res)
#' }
mbenchmark_subsetting <- function(x, type = c("random_slicing", "region_selection"), shape = c(0.5, 1, 2), nsubset = 5, ubound = 0.1,  num_threads = 1, times = 5L, clear_page_cache = FALSE, verbose = TRUE){
  if(!is.list(x))
    x <- list(a = x)

  mat <- x[[1]]
  dims <- dim(mat)
  nrow <- dims[1]
  ncol <- dims[2]
  row_start <- col_start <- 1
  row_stride <- floor(nrow * ubound/nsubset)
  res <- .ldply(type, function(thistype){
    message(thistype)
    #interate over different subset shapes
    res <- .ldply(shape, function(ratio){
      message("subset shape (nrow / ncol):", ratio)
      col_stride <- floor(row_stride * ratio)

      #iterate over subsets of different sizes
      res <- .ldply(seq_len(nsubset), function(i)
      {
        # browser()
        row_size <- row_stride * i
        if(thistype == "region_selection")
        {
          row_end <- row_start + row_size - 1
          ridx <- row_start:row_end

        }else
          ridx <- sample(nrow, row_size)

        col_size <- col_stride * i

        if(thistype == "region_selection")
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

        if(thistype == "region_selection")
          if(verbose)
            message("row = ", row_start, ":", row_end, " col = ", col_start, ":", col_end)

        #iterate multiple times
        res <- .ldply(seq_len(times), function(j)
        {
          if(clear_page_cache)
          {
            stop("clear_page_cache not supported yet!")
          }

          records <- new.env(parent = emptyenv())

          res <- .ldply(names(x), function(dsname){

            t <- system.time(v <- as.matrix(x[[dsname]][ridx, cidx]))[["elapsed"]]

            #validity check on the results
            if(j==1)#only check at first timeiteration
              if(is.null(records[["first"]]))
                records[["first"]] <- v#assign the value from the first ds
              else
                if(!isTRUE(all.equal(records[["first"]], v, check.attributes = FALSE)))
                  stop("results from ", dsname, " are not the same as the other dataset!")

              data.table(time = t, dataset = dsname)
          })


          set(res, j = "timeid", value = j)

        })
        # set(res, j = "size", value = paste0(row_size, "x", col_size))
        set(res, j = "nrow", value = row_size)
        res
      })
      set(res, j = "nrow/ncol", value = ratio)
    })
    res[, task := thistype]
  })
  attr(res, "class") <- c("mbenchmark_subsetting", "mbenchmark",attr(res, "class"))
  res
}
