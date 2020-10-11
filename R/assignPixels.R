#' Assign a 2D matrix of pixels to specified colors
#'
#' @param color_centers Matrix of color centers (rows = colors, columns = channels).
#' @param pixel_matrix Matrix of pixel colors (rows = pixels, columns = channels).
#' @param color_space Color space in which to minimize distances, passed to
#'   \code{\link{grDevices}{convertColor}}. One of "sRGB", "Lab", "Luv", or
#'   "XYZ". Default is "Lab", a perceptually uniform (for humans) color space.
#' @param ref_white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight.
#' @param adjust_centers Logical. Should the returned color clusters be the
#'   average value of the pixels assigned to that cluster? See details.
#'
#' @return A list of class `color_clusters`, containing:
#' \enumerate{
#'         \item `pixel_assignments`: The color center assignment for each pixel.
#'         \item `centers`: A matrix of color centers. If `adjust_centers =
#'         FALSE`, this will be identical to the input of `color_centers`.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details
#' This is a largely internal function called by \code{\link[recolorize]{imposeColors}}
#' for recoloring an image based on extrinsic colors. If `adjust_centers = TRUE`,
#' then after assigning pixels to given color centers, the location of each color center
#' is replaced by the average color of all the pixels assigned to that center.
#'
#' @examples
#'
#' # RGB extremes (white, black, red, green, blue, yellow, magenta, cyan)
#' ctrs <- matrix(c(1, 1, 1,
#'                  0, 0, 0,
#'                  1, 0, 0,
#'                  0, 1, 0,
#'                  0, 0, 1,
#'                  1, 1, 0,
#'                  1, 0, 1,
#'                  0, 1, 1), byrow = TRUE, ncol = 3)
#'
#' # plot it
#' recolorize::plotColorPalette(ctrs)
#'
#' # create a pixel matrix of random colors
#' pixel_matrix <- matrix(runif(3000), ncol = 3)
#'
#' # assign pixels
#' reassigned <- recolorize::assignPixels(ctrs, pixel_matrix, adjust_centers = TRUE)
#' recolorize::plotColorPalette(reassigned$centers)
#'
#' # if we turn off adjust_centers, the colors remain the same as the inputs:
#' keep.centers <- recolorize::assignPixels(ctrs, pixel_matrix, adjust_centers = FALSE)
#' recolorize::plotColorPalette(keep.centers$centers)
#'
#' @export
assignPixels <- function(color_centers,
                         pixel_matrix,
                         color_space = "Lab",
                         ref_white = "D65",
                         adjust_centers = TRUE) {

  if (color_space != "sRGB") {
    pm <- grDevices::convertColor(pixel_matrix,
                                  from = "sRGB",
                                  to = color_space,
                                  to.ref.white = ref_white)
    ctrs <- grDevices::convertColor(color_centers,
                                   from = "sRGB",
                                   to = color_space,
                                   to.ref.white = ref_white)
  } else {
    pm <- pixel_matrix
    ctrs <- color_centers
  }

  # I'm not sure this is really as fast as it could be
  tmp <- sapply(1:nrow(pm),
                  function(i) apply(ctrs, 1,
                                    function(v) sum((pm[i, ]-v)^2)))

  # make returnables
  pixel_assignments <- max.col(-t(tmp))  # find index of min distance
  assignments <- table(pixel_assignments) # make a table of assigned pixels
  sizes <- rep(0, nrow(color_centers))
  sizes[as.numeric(names(assignments))] <- assignments # empty clusters are 0

  # if specified: make new color centers based on average of assigned pixels
  if (adjust_centers) {

    for (i in 1:nrow(ctrs)) {

      pixel_idx <- which(pixel_assignments == i)

      if (length(pixel_idx) == 0) {
        next
      } else if (length(pixel_idx) == 1) {

        ctrs[i, ] <- pm[pixel_idx, ]

      } else {

        ctrs[i, ] <- colMeans(pm[pixel_idx, ])

      }

    }
  }

  # and convert back to sRGB
  if (color_space != "sRGB") {
    color_centers <- grDevices::convertColor(ctrs,
                                   from = color_space,
                                   to = "sRGB",
                                   from.ref.white = ref_white)
  }

  color_clusters <- list(pixel_assignments = pixel_assignments,
                         centers = color_centers,
                         sizes = sizes)
  class(color_clusters) <- "color_clusters"
  return(color_clusters)

}

