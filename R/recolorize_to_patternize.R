#' Convert a recolorize object to a raster object
#'
#' Convert from a `recolorize` object to a list of RasterLayer objects, the
#' format required by the `patternize` package. Note that most of the downstream
#' `patternize` functions that require lists of RasterLayer objects mostly
#' require lists of these lists, so you will probably need to use this function
#' on a list of `recolorize` objects.
#'
#' @param recolorize_obj A `recolorize` object.
#' @param return_background Logical.
#'
#' @details Note that this function does not retain the colors of the layers --
#'   you won't be able to convert back to a recolorize object from this object.
#'
#' @return A list of RasterLayer objects, one per color class.
#'
#' @examples
#'
#' \donttest{
#' # fit recolorize object:
#' img <- system.file("extdata/ephippigera.png", package = "recolorize")
#' rc <- recolorize2(img)
#'
#' # takes ~10 sec to run:
#' # convert to a raster list:
#' as_raster_list <- recolorize_to_patternize(rc)
#' }
#'
#' @export
recolorize_to_patternize <- function(recolorize_obj,
                                     return_background = FALSE) {

  # convert to a raster first
  r <- raster::raster(recolorize_obj$pixel_assignments)

  # iterate through unique components
  l <- 1:nrow(recolorize_obj$centers)

  if (return_background) {
    l <- c(0, l)
  }

  # make a list to store the layers
  layer_list <- vector("list", length = length(l))

  # for every layer...
  for (i in 1:length(l)) {

    # store all the coordinates equal to that layer
    layer_list[[i]] <- r == l[i]
  }

  # voila
  return(layer_list)

}
