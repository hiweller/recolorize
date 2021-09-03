#' Convert from a RasterBrick to an array
#'
#' Converts from a RasterBrick to a numeric array. Useful
#' in going from patternize to recolorize.
#'
#' @param raster_brick An object of RasterBrick class.
#'
#' @details
#' This function is provided to convert from the RasterBrick objects provided
#' by the alignment functions in the patternize package, e.g. [patternize::alignLan].
#'
#' @export
brick_to_array <- function(raster_brick) {

  # shorter object name
  r <- raster_brick

  # get non-background pixels (> 0) and set them to have alpha = 1
  r_alpha <- raster::merge(r > 0)

  # adjust negative values
  r[r < 0] <- 0

  # add alpha layer
  # divide r by 255 so it's in a 0-1 range
  r2 <- raster::addLayer(r / 255, r_alpha)

  # convert to an array
  r3 <- raster::as.array(r2)

  # and return
  return(r3)

}
