#' Change color centers to median color of all pixels assigned to it
#'
#' By default, recolorize sets the centers of each color patch to the average
#' (mean) color of all pixels assigned to it. This can sometimes result in colors
#' that look washed out, especially in cases where a region is very shiny (e.g.
#' black with white reflective highlights will average to grey). In these cases,
#' switching to median colors may be either more accurate or more visually
#' pleasing.
#'
#' @param recolorize_obj A `recolorize` class object.
#' @param plotting Logical. Plot results?
#'
#' @return A `recolorize` object, with median colors instead of average colors
#' in the `centers` attribute.
#'
#' @export
medianColors <- function(recolorize_obj, plotting = TRUE) {

  # make a new matrix for colors
  median_ctrs <- recolorize_obj$centers

  # calculate median colors
  for (i in 1:nrow(recolorize_obj$centers)) {
    idx <- which(recolorize_obj$pixel_assignments == i)
    px <- recolorize_obj$original_img[idx]
    as_rgb <- grDevices::col2rgb(px) / 255
    median_color <- apply(as_rgb, 1, stats::median)
    median_ctrs[i, ] <- median_color
  }

  if (plotting) {
    # reset graphical parameters when function exits:
    current_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(current_par))

    graphics::layout(matrix(1:4, 1, 4),
                     widths = c(0.4, 0.1, 0.1, 0.4))

    # original
    # plotting image
    graphics::par(mar = c(0, 0, 2, 0))
    original <- constructImage(recolorize_obj$pixel_assignments,
                               recolorize_obj$centers)
    plotImageArray(original, main = "average colors")

    # plotting palette
    graphics::par(mar = rep(0.5, 4))
    plotColorPalette(recolorize_obj$centers, horiz = FALSE)

    # median
    # plotting palette
    graphics::par(mar = rep(0.5, 4))
    plotColorPalette(median_ctrs, horiz = FALSE)

    # plotting image
    graphics::par(mar = c(0, 0, 2, 0))
    original <- constructImage(recolorize_obj$pixel_assignments,
                               median_ctrs)
    plotImageArray(original, main = "median colors")
  }

  # swap out centers and return object
  recolorize_obj$centers <- median_ctrs
  return(recolorize_obj)
}
