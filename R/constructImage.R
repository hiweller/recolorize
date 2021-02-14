#' Generate an image from pixel assignments and color matrix
#'
#' Combines a matrix of pixel assignments and a corresponding
#' matrix of colors to make a recolored RGB image.
#'
#' @param pixel_assignments A matrix of index values for each pixel which
#'   corresponds to `centers` (e.g. a `1` indicates that pixel is the
#'   color of the first row of `centers`). Pixels with an index value of 0
#'   are considered background.
#' @param centers An n x 3 matrix of color centers where rows are colors
#'   and columns are R, G, and B channels.
#' @param background_color A numeric RGB triplet, a hex code, or a named
#'   R color for the background. Will be masked by alpha channel (and appear
#'   white in the plot window), but will be revealed if the alpha
#'   channel is removed. If the alpha channel is a background mask,
#'   this is the 'baked in' background color.
#'
#' @return An image (raster) array of the recolored image,
#' with four channels (R, G, B, and alpha).
#'
#' @export
constructImage <- function(pixel_assignments,
                           centers,
                           background_color = "white") {

  # make two copies of matrix as a cimg object:
  index_cimg <- imager::as.cimg(pixel_assignments)
  final_cimg <- index_cimg

  # color the background in
  # you won't see this unless you remove the alpha layer:
  final_cimg <- imager::colorise(final_cimg,
                                 index_cimg == 0,
                                 background_color)

  # color in every color center:
  for (i in 1:nrow(centers)) {
    final_cimg <- imager::colorise(final_cimg,
                                   index_cimg == i,
                                   centers[i, ])
  }

  # convert to a regular array:
  as_array <- cimg_to_array(final_cimg)

  # and add an alpha channel:
  alpha_layer <- pixel_assignments
  alpha_layer[which(alpha_layer > 0)] <- 1
  as_array <- abind::abind(as_array,
                           alpha_layer,
                           along = 3)

  # beep boop:
  return(as_array)

}

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
