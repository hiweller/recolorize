#' Convert from a (small-r) raster object to an RGB array
#'
#' Recreates the original numeric array from a `raster` object created
#' by [grDevices::as.raster]. Not to be confused with the `Raster*` classes
#' used by the `raster` package.
#'
#' @param raster_obj A matrix of hex codes as output by [grDevices::as.raster].
#' @param alpha Logical. If there is an alpha channel, retain it in the array?
#'
#' @return A numeric RGB array (0-1 range).
#'
#' @export
raster_to_array <- function(raster_obj, alpha = TRUE) {

  # convert to matrix and to RGB triplets in 0-1 range:
  o1 <- as.matrix(raster_obj)
  o2 <- grDevices::col2rgb(o1, alpha = alpha) / 255

  # make a blank array of correct dimensions:
  im_array <- array(NA, dim = c(dim(raster_obj), nrow(o2)))

  # fill it out:
  for (i in 1:nrow(o2)) {
    im_array[ , , i] <- o2[i, ]
  }

  # and return
  return(im_array)
}

#' Convert from an array to a raster stack
#'
#' Convert from an image array to a raster stack, optionally using the alpha
#' channel as a mask.
#'
#' @param img_array An RGB array.
#' @param type Type of Raster* object to return. One of either "stack"
#'   ([raster::stack]) or "brick" ([raster::brick]).
#' @param alpha_mask Logical. Use the alpha channel as a background mask?
#' @param return_alpha Logical. Return the alpha channel as a layer?
#'
#' @return A Raster* object, either `RasterStack` or `RasterBrick` depending
#' on the `type` argument.
array_to_RasterStack <- function(img_array,
                                 type = c("stack", "brick"),
                                 alpha_mask = TRUE,
                                 return_alpha = FALSE) {

  requireNamespace("raster")

  type <- match.arg(type)

  r <- apply(img_array * 255, 3, raster::raster)

  if (type == "stack") {
    r2 <- raster::stack(r)
  } else {
    r2 <- raster::brick(r)
  }

  output <- r2

  if (alpha_mask) {

    if (dim(r2)[3] != 4) {
      warning("No alpha channel included; not masking output")
    } else {
      r3 <- raster::mask(raster::subset(r2, 1:3),
                         raster::subset(r2, 4),
                         maskvalue = 0)
      output <- r3
    }
  }

  raster::crs(output) <- "+proj=longlat"

  return(output)

}
