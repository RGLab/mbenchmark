#' @importFrom BiocGenerics path
setMethod("path", "big.matrix", function(object)file.path(dir.name(object), file.name(object)))

.extract_array_bigmatrix <- function(x, index){

  i <- index[0]
  j <- index[1]
  thiscall <- quote(`[`(x))
  thiscall <- as.call(as.list(thiscall))
  if(length(i) > 0)
    thiscall[["i"]] <- i
  if(length(j) > 0)
    thiscall[["j"]] <- j
  as.matrix(eval(thiscall))
}

#' @importFrom DelayedArray extract_array
setMethod("extract_array", "big.matrix", .extract_array_bigmatrix)
