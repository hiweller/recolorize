#' Assign a 2D matrix of pixels to specified colors
#'
#' @param color.centers Matrix of color centers (rows = colors, columns = channels).
#' @param pixel.matrix Matrix of pixel colors (rows = pixels, columns = channels).
#' @param color.space Color space in which to minimize distances, passed to
#'   \code{\link{grDevices}{convertColor}}. One of "sRGB", "Lab", "Luv", or
#'   "XYZ". Default is "Lab", a perceptually uniform (for humans) color space.
#' @param ref.white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight.
#' @param adjust.centers Logical. Should the returned color clusters be the
#'   average value of the pixels assigned to that cluster? See details.
#'
#' @return A list of class `color.clusters`, containing:
#' \enumerate{
#'         \item `pixel.assignments`: The color center assignment for each pixel.
#'         \item `centers`: A matrix of color centers. If `adjust.centers =
#'         FALSE`, this will be identical to the input of `color.centers`.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details
#' This is a largely internal function called by \code{\link[recolorize]{imposeColors}}
#' for recoloring an image based on extrinsic colors. If `adjust.centers = TRUE`,
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
#' pixel.matrix <- matrix(runif(3000), ncol = 3)
#'
#' # assign pixels
#' reassigned <- recolorize::assignPixels(ctrs, pixel.matrix, adjust.centers = TRUE)
#' recolorize::plotColorPalette(reassigned$centers)
#'
#' # if we turn off adjust.centers, the colors remain the same as the inputs:
#' keep.centers <- recolorize::assignPixels(ctrs, pixel.matrix, adjust.centers = FALSE)
#' recolorize::plotColorPalette(keep.centers$centers)
#'
#' @export
assignPixels <- function(color.centers,
                         pixel.matrix,
                         color.space = "Lab",
                         ref.white = "D65",
                         adjust.centers = TRUE) {

  if (color.space != "sRGB") {
    pm <- grDevices::convertColor(pixel.matrix,
                                  from = "sRGB",
                                  to = color.space,
                                  to.ref.white = ref.white)
    ctrs <- grDevices::convertColor(color.centers,
                                   from = "sRGB",
                                   to = color.space,
                                   to.ref.white = ref.white)
  } else {
    pm <- pixel.matrix
    ctrs <- color.centers
  }

  # I'm not sure this is really as fast as it could be
  tmp <- sapply(1:nrow(pm),
                  function(i) apply(ctrs, 1,
                                    function(v) sum((pm[i, ]-v)^2)))

  # make returnables
  pixel.assignments <- max.col(-t(tmp))  # find index of min distance
  assignments <- table(pixel.assignments) # make a table of assigned pixels
  sizes <- rep(0, nrow(color.centers))
  sizes[as.numeric(names(assignments))] <- assignments # empty clusters are 0

  # if specified: make new color centers based on average of assigned pixels
  if (adjust.centers) {

    for (i in 1:nrow(ctrs)) {

      pixel.idx <- which(pixel.assignments == i)

      if (length(pixel.idx) == 0) {
        next
      } else if (length(pixel.idx) == 1) {

        ctrs[i, ] <- pm[pixel.idx, ]

      } else {

        ctrs[i, ] <- colMeans(pm[pixel.idx, ])

      }

    }
  }

  # and convert back to sRGB
  if (color.space != "sRGB") {
    color.centers <- grDevices::convertColor(ctrs,
                                   from = color.space,
                                   to = "sRGB",
                                   from.ref.white = ref.white)
  }

  color.clusters <- list(pixel.assignments = pixel.assignments,
                         centers = color.centers,
                         sizes = sizes)
  class(color.clusters) <- "color.clusters"
  return(color.clusters)

}

