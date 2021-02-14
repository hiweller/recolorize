#' Expand aspects of a recolorize object for other functions
#'
#' @param recolorize_obj A `recolorize` object.
#' @param original_img Logical. Return original image as numeric array?
#' @param recolored_img Logical. Return recolored image as numeric array?
#' @param sizes Logical. Return cluster sizes (as number of pixels)?
#'
#' @return A `recolorize` object with the indicated additional elements,
#' as well as the original elements.
expand_recolorize <- function(recolorize_obj,
                              original_img = FALSE,
                              recolored_img = FALSE,
                              sizes = FALSE) {

  rc <- recolorize_obj

  if (original_img) {
    rc$original_img <- raster_to_array(recolorize_obj$original_img)
  }

  if (recolored_img) {
    rc$recolored_img <- constructImage(recolorize_obj$pixel_assignments,
                                       recolorize_obj$centers)
  }

  if (sizes) {
    sizes <- table(recolorize_obj$pixel_assignments)
    sizes <- sizes[-which(names(sizes) == 0)]
    rc$sizes <- sizes[order(as.numeric(names(sizes)))]
  }

  return(rc)

}
