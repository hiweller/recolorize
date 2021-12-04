#' Get recolored image from a recolorize object
#'
#' `recolorize` objects use a numeric color map and a matrix of
#' color centers to make recolored images, since this is a lighter weight
#' and more flexible format. This function generates a colored image
#' from those values for plotting.
#'
#' @param recolorize_obj An object of class `recolorize`. Must include
#'   a pixel assignment matrix and matrix of color centers.
#' @param type Type of image to return. One of either "array" or "raster".
#'   Arrays are numeric RGB arrays (larger, but easier to do operations on),
#'  rasters are matrices of hex codes (smaller, only really good for plotting).
#'
#' @return A numeric image array (if `type = array`) or a matrix of hex codes (
#' if `type = raster`).
#'
#'
#' @export
recoloredImage <- function(recolorize_obj,
                           type = "array") {

  type <- match.arg(type, c("array", "raster"))
  img <- constructImage(recolorize_obj$pixel_assignments,
                        recolorize_obj$centers)
  if(type == "raster") {
    img <- grDevices::as.raster(img)
  }

  return(img)

}
