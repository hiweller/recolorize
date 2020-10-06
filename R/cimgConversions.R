#' Converts from cimg to raster array
#'
#' What it says it does.
#'
#' @param x A `cimg` object.
#'
#' @return A 3D array.
cimg.to.array <- function(x) {
  img <- as.numeric(x)
  dim(img) <- dim(x)[c(1, 2, 4)]
  if (dim(img)[3] == 1) {
    dim(img) <- dim(img)[1:2]
  }
  return(img)
}

#' Converts from a raster array to a cimg object
#'
#' What it says it does.
#'
#' @param x An image array, i.e. as read in by readPNG.
#' @param flatten.alpha Logical. Flatten the alpha channel?
#' @param bg Passed to \code{\link[imager]{flatten.alpha}}. Pixel color for
#'   previously transparent pixels.
#' @param rm.alpha Logical. Remove the alpha channel?
#'   Note this will "reveal" whatever is hidden behind
#'   the transparent pixels, rather than turn them white.
#'
#' @return A `cimg` object.
array.to.cimg <- function(x,
                          flatten.alpha = TRUE,
                          bg = "white",
                          rm.alpha = TRUE) {

  dim(x) <- c(dim(x)[1:2], 1, dim(x)[3])
  class(x) <- "cimg"

  if (flatten.alpha) {
    x <- imager::flatten.alpha(x, "white")
  }

  if (rm.alpha) {
    x <- imager::rm.alpha(x)
  }

  return(x)

}
