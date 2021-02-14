#' Recolor an image after color clustering
#'
#' Legacy function. Probably don't use this. Use [recoloredImage].
#'
#' @param bg_indexed An object of class `bg_index`, the output of
#'   \code{\link[recolorize]{backgroundIndex}}.
#' @param color_clusters An object of class `color_clusters`, the output of
#'   \code{\link[recolorize]{colorClusters}}.
#' @param plotting Logical. Plot recolored image results?
#' @param main Plot title.
#' @param remove_empty_clusters Logical. If no pixels are assigned to a given
#'   color cluster, should that cluster be returned by the function, or dropped?
#' @param bg_color A color (either an RGB triplet, hex code, or R color name)
#'   for the background color. Will not be visible unless the alpha channel is
#'   removed when plotting; see details.
#'
#' @return
#' A list with the following attributes:
#' \enumerate{
#'     \item `recolored_img`: The recolored image, as an RGB array.
#'     \item `centers`: The colors to which the recolored image were mapped.
#' }
#'
#' @details
#' This function works by taking in:
#' \enumerate{
#'     \item A vector of pixel assignments, where each number represents the
#'     color cluster to which the pixel at that index was assigned;
#'     \item A matrix of colors centers whose order is assumed to match that of
#'     the pixel assignments
#'     \item An RGB array whose pixel locations are assumed to match those of
#'     the pixel assignments.
#' }
#'
#' This is all kept organized by keeping these data together in the `bg_index` and `color_clusters`
#' classes, and because this function is mostly called internally by \code{\link[recolorize]{imposeColors}}
#' and \code{\link[recolorize]{recolorize}}. However, it does present the user
#' with opportunities to remap colors at random by reshuffling or swapping the color centers
#' in the `color_clusters` object. This could be useful for a number of things,
#' such as testing analytical or color clustering robustness, or otherwise
#' ruining a nice image with math.
#'
#' Note that regardless of input background type, the returned image
#' uses the alpha channel to indicate the background, effectively
#' storing a background mask in the alpha channel. Users can also
#' change the background color by specifying it in the function, but
#' it will appear as white unless only the first three channels
#' are plotted.
#'
recolorImage <- function(bg_indexed, color_clusters,
                         plotting = FALSE, main = "",
                         remove_empty_clusters = FALSE,
                         bg_color = "white") {

  # just in case...
  if (class(bg_indexed) != "bg_index" |
      class(color_clusters) != "color_clusters") {
    warning("bg_index and color_clusters should be
            the output of backgroundIndex() and colorClusters(),
            respectively")
  }

  # make an image from the above information:
  recolored_img <- constructImage(color_clusters$pixel_assignments,
                                  color_clusters$centers,
                                  background_color = bg_color)

  # plot if plotting:
  if (plotting) {
    plotImageArray(recolored_img, main = main)
  }

  # just in case:
  empty_centers <- which(color_clusters$sizes == 0)

  # make returnables
  if (length(empty_centers) > 0 & isTRUE(remove_empty_clusters)) {
    centers <- color_clusters$centers[-empty_centers, ]
  } else {
    centers <- color_clusters$centers
  }

  # return it!
  return(list(recolored_img = recolored_img,
              centers = centers))

}
