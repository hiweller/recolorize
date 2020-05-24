#' Assign a 2D matrix of pixels to specified colors
#'
#' @param color.centers Matrix of color centers (rows = colors, columns = channels).
#' @param pixel.matrix Matrix of pixel colors (rows = pixels, columns = channels).
#' @param adjust.centers Logical. Should the returned color clusters be the
#'   average value of the pixels assigned to that cluster? See details.
#'
#' @return A list of class `color.clusters`, containing:
#' \enumerate{
#'         \item `pixel.assignments`: A vector of color center assignments for each pixel.
#'         \item `centers`: A matrix of color centers. If `adjust.centers =
#'         FALSE`, this will be identical to the input `color.centers`.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details
#' This is a largely internal function called by \code{\link[recolorize]{imposeClusters}}
#' for recoloring an image based on extrinsic colors. If `adjust.centers = TRUE`,
#' then after assigning pixels to given colors,
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
assignPixels <- function(color.centers, pixel.matrix, adjust.centers = TRUE) {

  # I'm not sure this is really as fast as it could be
  tmp <- sapply(1:nrow(pixel.matrix),
                  function(i) apply(color.centers, 1,
                                    function(v) sum((pixel.matrix[i, ]-v)^2)))

  # make returnables
  pixel.assignments <- max.col(-t(tmp))  # find index of min distance
  sizes <- table(pixel.assignments) # empty clusters?

  # if specified: make new color centers based on average of assigned pixels
  if (adjust.centers) {

    for (i in 1:nrow(color.centers)) {

      pixel.idx <- which(pixel.assignments == i)

      if (length(pixel.idx) == 0) { next } else {
        color.centers[i, ] <- colMeans(pixel.matrix[pixel.idx, ])
      }

    }

  }

  color.clusters <- list(pixel.assignments = pixel.assignments,
                         centers = color.centers,
                         sizes = sizes)
  class(color.clusters) <- "color.clusters"
  return(color.clusters)

}
