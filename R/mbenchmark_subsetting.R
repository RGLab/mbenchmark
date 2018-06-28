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
mbenchmark_subsetting <- function(x, type = c("random_slicing", "region_selection")
                                  , shape = c(0.5, 1, 2)
                                  , nsubset = 5
                                  , ubound = 0.1
                                  , num_threads = 1
                                  , times = 5L
                                  , clear_page_cache = FALSE
                                  , trace_mem = FALSE
                                  , cache.file = tempfile()
                                  , verbose = FALSE){
  if(!is.list(x))
    x <- list(a = x)

  pid <- Sys.getpid()
  cmd <- paste0("ps -p ", pid, " -o rss --no-headers")

  mat <- x[[1]]
  dims <- dim(mat)
  nrow <- dims[1]
  ncol <- dims[2]
  row_start <- col_start <- 1
  row_stride <- floor(nrow * ubound/nsubset)
  lapply(type, function(thistype){
    message(thistype)
    #interate over different subset shapes
    lapply(shape, function(ratio){
      # message("subset shape (nrow / ncol):", ratio)
      col_stride <- floor(row_stride / ratio)

      #iterate over subsets of different sizes
      lapply(seq_len(nsubset), function(i)
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
        {
          if(col_size>ncol)
          {
            warning("col_size out of bound!", col_size)
            return(NULL)
          }
          cidx <- sample(ncol, col_size)
        }


        # if(thistype == "region_selection")
          # if(verbose)
            # message("row = ", row_start, ":", row_end, " col = ", col_start, ":", col_end)

        #iterate multiple times
        lapply(seq_len(times), function(j)
        {
          if(file.exists(cache.file))
          {
            cache.res <- fread(cache.file)
            cached.sub <- cache.res[timeid == j & nrow == row_size & `nrow/ncol` == ratio & task == thistype,]
            is.cached <- nrow(cached.sub) >0
          }else
          {
            is.cached <- FALSE
            cache.res <- NULL
          }


          if(!is.cached)
          {
            if(clear_page_cache)
            {
              # stop("clear_page_cache not supported yet!")
              drop_page_cahce()
            }

            records <- new.env(parent = emptyenv())

            res <- .ldply(names(x), function(dsname){
              #track mem
              if(trace_mem && num_threads == 1 && j == 1)
              {
                gc()#return mem to os
                gc()
                mem <- as.integer(system(cmd, intern = TRUE))
              }


              t <- system.time(v <- as.matrix(x[[dsname]][ridx, cidx]))[["elapsed"]]

              #track mem change
              if(trace_mem && num_threads == 1 && j == 1)
              {
                mem <- as.integer(system(cmd, intern = TRUE)) - mem
                gc()#return mem to os
                gc()#somehow need it twice to be sussessful
              }else
                mem <- 0


              #validity check on the results
              if(j==1)#only check at first timeiteration
                if(is.null(records[["first"]]))
                  records[["first"]] <- v#assign the value from the first ds
                else
                  if(!isTRUE(all.equal(records[["first"]], v, check.attributes = FALSE)))
                    stop("results from ", dsname, " are not the same as the other dataset!")

                dt <- data.table(time = t, mem_change = mem, dataset = dsname, timeid = j, nrow = row_size, `nrow/ncol` = ratio, task = thistype)
                if(verbose)
                  print(dt)
                dt

            })#iter over datasets

            #cache it to disk to avoid recompute next time when resume
            if(is.null(cache.res))
              cache.res <- res
            else
              cache.res <- rbind(cache.res, res)
            fwrite(cache.res, file = cache.file)
          }else
          {
            if(verbose)
            {
              message("skip cached items: ")
              print(cached.sub[1, list(timeid, nrow, `nrow/ncol`, task)])
            }

          }

        })#iter over times
      })#iter over shapes
    })
  })
  res <- fread(cache.file)
  attr(res, "class") <- c("mbenchmark_subsetting", "mbenchmark",attr(res, "class"))
  res
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
  p <- p + facet_grid(task~`nrow/ncol`, scales="free")
  p
}
