#' @examples
#' \dontrun{
#' mat <- matrix(seq_len(2e6), nrow = 1e3, ncol =2e3)
#' smat <- Matrix(mat, sparse = TRUE)
#' mat.list <- list(dense = mat, sparse = smat)
#' res <- mbenchmark_mbenchmark_traversal(mat.list, times = 2)
#' autoplot(res)
#' }
mbenchmark_mbenchmark_traversal <- function(x, type = c("rowSums", "colSums"),  num_threads = 1, times = 5L, clear_page_cache = FALSE, verbose = TRUE){
  if(!is.list(x))
    x <- list(a = x)

  res <- .ldply(type, function(thistype){
    message(thistype)
    #iterate multiple times
    res <- .ldply(seq_len(times), function(j)
    {
      if(clear_page_cache)
      {
        stop("clear_page_cache not supported yet!")
      }

      records <- new.env(parent = emptyenv())

      res <- .ldply(names(x), function(dsname){
        f <- as.symbol(thistype)
        f  <- eval(f, envir = globalenv())
        t <- system.time(v <- f(x[[dsname]]))[["elapsed"]]
        #validity check on the results
        if(j==1)#only check at first timeiteration
          if(is.null(records[["first"]]))
            records[["first"]] <- v#assign the value from the first ds
          else
            if(!all.equal(records[["first"]], v))
              stop("results from ", dsname, " are not the same as the other dataset!")
        data.table(time = t, dataset = dsname)
      })


      set(res, j = "timeid", value = j)

      })
    res[, task := thistype]
  })
  attr(res, "class") <- c("mbenchmark_traversal", "mbenchmark",attr(res, "class"))
  res
}