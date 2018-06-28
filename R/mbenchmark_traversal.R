#' @examples
#' \dontrun{
#' library(Matrix)
#' mat <- matrix(seq_len(2e6), nrow = 1e3, ncol =2e3)
#' smat <- Matrix(mat, sparse = TRUE)
#' mat.list <- list(dense = mat, sparse = smat)
#' res <- mbenchmark_traversing(mat.list, times = 2, ubound = 0.1)
#' autoplot(res)
#' }
mbenchmark_traversing <- function(x, type = c("rowSums", "colSums")
                                            , ubound = 0.1
                                            ,  num_threads = 1, times = 5L, clear_page_cache = FALSE, verbose = TRUE){
  if(!is.list(x))
    x <- list(a = x)
  mat <- x[[1]]
  dims <- dim(mat)
  nrow <- dims[1]
  ncol <- dims[2]
  nrow <- nrow * ubound
  ncol <- ncol * ubound
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
        thisCall <- substitute(f(x[[dsname]][1:nrow, 1:ncol]), list(f = as.symbol(thistype)))
        t <- system.time(v <- eval(thisCall))[["elapsed"]]
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
  attr(res, "class") <- c("mbenchmark_traversing", "mbenchmark",attr(res, "class"))
  res
}

#' @export
autoplot.mbenchmark_traversing<- function(object, ...){
  object <- copy(object)
  object <- object[, time := mean(time)
                   , by = c("task", "dataset")
                   ]
  ggplot(object, aes(x = task, y = time, fill = dataset)) + geom_col(position = "dodge")
  # p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # p
}

