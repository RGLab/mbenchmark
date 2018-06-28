#can't skip this class because big.matrix class already has dim method defined but return numberic instead of integer
#thus incompatible with DelayedArray. So we will need this class for proper method dispatch
#also [] method for bm doesn't support NULL idx
#' @exportClass BMArraySeed
#' @import DelayedArray
#' @importClassesFrom  bigmemoryExtras BigMatrix
setClass("BMArraySeed",
         contains="Array",
         representation(
           obj = "big.matrix",
           filepath="character",       # Absolute path to the HDF5 file so the
           # object doesn't break when the user
           # changes the working directory (e.g. with
           # setwd()).
           name="character",           # Name of the dataset in the HDF5 file.
           dim="integer",
           first_val="ANY",            # First value in the dataset.
           chunkdim="integer_OR_NULL"
         )
)

#' @export
BMArraySeed <- function(obj, type=NA)
{
  stopifnot(is(obj, "big.matrix"))
  # filepath <- dir.name(obj)
  # filepath <- .normarg_path(filepath, "'filepath'")
  # if (!isSingleString(name))
  #   stop(wmsg("'name' must be a single string specifying the name ",
  #             "of the dataset in the file"))
  # if (name == "")
  #   stop(wmsg("'name' cannot be the empty string"))
  # if (!isSingleStringOrNA(type))
  #   stop("'type' must be a single string or NA")
  # dim <- h5dim(filepath, name)
  # if (any(dim == 0L)) {
  #   if (is.na(type))
  #     stop(wmsg("This dataset is empty! Don't know how to ",
  #               "determine the type of an empty dataset at the ",
  #               "moment. Please use the 'type' argument to help me ",
  #               "(see '?HDF5Array' for more information)."))
  #   first_val <- match.fun(type)(1)  # fake value
  #   if (!is.atomic(first_val))
  #     stop(wmsg("invalid type: ", type))
  # } else {
  #   first_val <- .read_h5dataset_first_val(filepath, name, length(dim))
  #   detected_type <- typeof(first_val)
  #   if (!(is.na(type) || type == detected_type))
  #     warning(wmsg("The type specified via the 'type' argument (",
  #                  type, ") doesn't match the type of this  ",
  #                  "dataset (", detected_type, "). Ignoring the ",
  #                  "former."))
  # }
  # chunkdim <- h5chunkdim(filepath, name)
  new("BMArraySeed"
       , obj = obj
       # , filepath=filepath,
       # name=name,
       # dim=dim,
       , first_val=obj[1,1]
       # , chunkdim=chunkdim
       )
}


#' @importFrom BiocGenerics path
#' @export
setMethod("path", "BMArraySeed", function(object)file.path(dir.name(object@obj), file.name(object@obj)))

#' @export
setMethod("dim", "BMArraySeed", function(x)as.integer(dim(x@obj)))

#' @importMethodsFrom  bigmemory [
.extract_array_bigmatrix <- function(x, index){
  .extract_array(x@obj, index)
}
.extract_array <- function(x, index){

  i <- index[[1]]
  j <- index[[2]]
  if(!is.null(i) > 0&&!is.null(j) > 0)
    thiscall <- quote(x[i,j, drop = FALSE])
  else if(!is.null(i) > 0)
    thiscall <- quote(x[i,, drop = FALSE])
  else if(!is.null(j) > 0)
    thiscall <- quote(x[,j, drop = FALSE])
  else
    thiscall <- quote(x[,, drop = FALSE])

  as.matrix(eval(thiscall))
}

#' @importFrom DelayedArray extract_array
#' @export
setMethod("extract_array", "BMArraySeed", .extract_array_bigmatrix)

# setMethod("extract_array", "big.matrix", .extract_array)
