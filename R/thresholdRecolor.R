#' Drop minor colors from a recolorize object
#'
#' Drops color patches whose cumulative sum (as a proportion of total pixels
#' assigned) is equal to or less than `pct`, so that only the dominant
#' color patches remain, and refits the object with the reduced set of
#' color centers Useful for dropping spurious detail colors.
#'
#' @param recolorize_obj An object of class `recolorize`.
#' @param pct The proportion cutoff (0-1) for dropping color patches. The
#' higher this value is, the more/larger color centers will be dropped.
#' @param plotting Logical. Plot the results?
#' @param ... Further arguments passed to [recolorize::imposeColors], which is
#'   called for refitting a new recolorize object for the reduced set of
#'   clusters.
#'
#' @details This function is fairly simple in execution: the color centers are
#'   arranged by their sizes, largest to smallest, and their cumulative sum is
#'   calculated. The minimum number of color centers to reach a cumulative sum
#'   equal to or greater than the cutoff (`1 - pct`) is retained, and these
#'   dominant colors are used to re-fit the image. Despite being
#'   straightforward, this can be a surprisingly useful function.
#'
#' @return A `recolorize` object.
#'
#' @examples
#' img <- system.file("extdata/fulgidissima.png", package = "recolorize")
#' init_fit <- recolorize(img, bins = 3)
#' thresh_fit <- thresholdRecolor(init_fit, pct = 0.1)
#'
#' # if you take it too far, you just get one color back:
#' thresh_fit_oops <- thresholdRecolor(init_fit, pct = 1)
#' @export
thresholdRecolor <- function(recolorize_obj,
                              pct = 0.05,
                              plotting = TRUE,
                              ...) {
  # threshold cutoff:
  pct <- 1 - pct

  # get sizes:
  sizes <- recolorize_obj$sizes

  # get size order:
  size_order <- order(sizes, decreasing = TRUE)

  # normalize so we get proportions:
  size_norm <- sizes[size_order] / sum(sizes)

  # take cumulative sum, and only take those clusters whose cumulative
  # sum is enough to reach the cutoff:
  keep_idx <- size_order[1:which(cumsum(size_norm) >= pct)[1]]

  # refit, using those colors
  img <- raster_to_array(recolorize_obj$original_img)
  refit <- imposeColors(img = img,
                        centers = recolorize_obj$centers[keep_idx, ],
                        plotting = FALSE,
                        ...)

  # plot comparison
  if (plotting) {
    graphics::par(mar = rep(1, 4))
    graphics::layout(matrix(1:4, nrow = 1),
                     widths = c(0.4, 0.1, 0.1, 0.4))
    initial_fit <- recoloredImage(recolorize_obj, type = "raster")
    thresholded_fit <- recoloredImage(refit, type = "raster")

    # plot initial fit
    plot(initial_fit); graphics::title("initial fit")
    plotColorPalette(recolorize_obj$centers,
                     recolorize_obj$sizes, horiz = F)
    plotColorPalette(refit$centers, refit$sizes, horiz = F)
    plot(thresholded_fit); graphics::title("thresholded fit")
  }
  return(refit)

}
