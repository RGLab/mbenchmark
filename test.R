library(HDF5Array)#must load it first to avoid namespace conflicting
library(DelayedArray)
library(mbenchmark)
path <- "/loc/no-backup/mike/shared/1M_neurons"
#h5
library(rhdf5)
h5.file <- file.path(path, "gz_chunk_by_gene_sub.h5")
hm = HDF5Array(h5.file, "data")
dims <- dim(hm)
nblocks <- 100

#bigmemory
library(bigmemory)

# bm.file <- file.path(path, "bm")
bm.desc <- file.path(path, "bm.desc")
# bm <- BigMatrix(nrow = dims[1], ncol = dims[2], backingfile = file.path(path, "bm"))
#
# i <- 1
# while(i < dims[2])
# {
#   j <- i + nblocks - 1
#   if(j > dims[2])
#     j <- dims[2]
#   bm[, i:j] <- as.matrix(hm[, i:j])
#   i <- j + 1
#   message(i)
# }
# all.equal(as.matrix(hm[,1:3]), as.matrix(bm[,1:3]))
# saveRDS(bm, file = file.path(path, "bmext"))
# bm <- readRDS(bm.file)
bm <- attach.big.matrix(bm.desc)
bmseed <- BMArraySeed(bm)
bm <- DelayedArray(bmseed)



library(ff)
ff.file <- file.path(path, "fm")
fm <- ff(vmode="double", dim=dims, filename = ff.file)
# i <- 1
# while(i < dims[2])
# {
#   j <- i + nblocks - 1
#   if(j > dims[2])
#     j <- dims[2]
#   fm[, i:j] <- as.matrix(hm[, i:j])
#   i <- j + 1
#   message(i)
# }
# all.equal(as.matrix(hm[,1:3]), as.matrix(fm[,1:3]))
fm <- DelayedArray(fm)

library(matter)
mm.file <- file.path(path, "mm")
mm <- matter_mat(nrow = dims[1], ncol = dims[2], paths = mm.file)
# i <- 1
# while(i < dims[2])
# {
#   j <- i + nblocks - 1
#   if(j > dims[2])
#     j <- dims[2]
#   mm[, i:j] <- as.matrix(hm[, i:j])
#   i <- j + 1
#   message(i)
# }
# all.equal(as.matrix(hm[,1:3]), as.matrix(mm[,1:3]))

mm <- DelayedArray(mm)


## Compare disk usage
mat.list <- list(bigmemory = bm, ff = fm, h5 = hm, matter = mm)

utils:::format.object_size(file.size(bm@seed@obj$backingfile), units = "Mb")
utils:::format.object_size(file.size(h5.file), units = "Mb")
utils:::format.object_size(file.size(ff.file), units = "Mb")
utils:::format.object_size(file.size(mm.file), units = "Mb")

# ridx <- 1:1e3
# microbenchmark::microbenchmark(as.matrix(bm[ridx, ridx])
#                                ,as.matrix(fm[ridx, ridx])
#                                ,as.matrix(mm[ridx, ridx])
#                                , times = 3
#                                )
res <- mbenchmark(mat.list, type = "subsetting", times = 5, ubound = 0.1, trace_mem = TRUE, verbose = T)
write.csv(res, file = file.path(path, "mbenchres.csv"))
res <- data.table::fread(file = file.path(path, "mbenchres.csv"))
class(res) <- c("mbenchmark_subsetting", class(res))
autoplot(res) + scale_y_log10()

plot_mem(res, units = "Kb") + scale_y_log10()
