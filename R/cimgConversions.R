#' Converts from cimg to raster array
#'
#' What it says it does.
#'
#' @param x A `cimg` object.
#'
#' @return A 3D array.
cimg.to.array <- function(x) {
  img <- as.numeric(x)
  dim(img) <- dim(x)[1:2]
  return(img)
}

#' Converts from a raster array to a cimg object
#'
#' What it says it does.
#'
#' @param x An image array, i.e. as read in by readPNG.
#'
#' @return A `cimg` object.
array.to.cimg <- function(x) {

  dim(x) <- c(dim(x)[1:2], 1, dim(x)[3])
  class(x) <- "cimg"
  x <- imager::rm.alpha(x)
  return(x)

}
