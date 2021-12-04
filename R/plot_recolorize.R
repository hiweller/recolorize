#' Plot recolorized image results
#'
#' S3 plotting method for objects of class `recolorize`. Plots a side-by-side
#' comparison of an original image and its recolorized version, plus the color
#' palette used for recoloring.
#'
#' @param x An object of class `recolorize`, such as
#' returned by \code{\link{recolorize}}, \code{\link{recluster}},
#' \code{\link{imposeColors}}, etc.
#' @param plot_original Logical. Plot the original image for comparison?
#' @param horiz Logical. Should plots be stacked vertically or horizontally?
#' @param sizes Logical. If `TRUE`, color palette is plotted proportional
#' to the size of each color. If `FALSE`, all colors take up an equal
#' amount of space, and their indices are printed for reference.
#' @param cex_text Text size for printing color indices. Plotting parameters
#'   passed to \code{\link{recolorize}{plotColorPalette}}.
#' @param ... further arguments passed to `plot`.
#'
#' @return No return value; plots the original image, recolored image, and
#' color palette.
#'
#' @examples
#' corbetti <- system.file("extdata/corbetti.png",
#'                          package = "recolorize")
#'
#' corbetti_recolor <- recolorize(corbetti, method = "hist",
#'                                          bins = 2, plotting = FALSE)
#'
#' # unscaled color palette
#' plot(corbetti_recolor)
#'
#' # scaled color palette
#' plot(corbetti_recolor, sizes = TRUE)
#'
#' @rdname plot.recolorize
#' @export
plot.recolorize <- function(x, ...,
                            plot_original = TRUE,
                            horiz = TRUE,
                            cex_text = 2, sizes = FALSE) {

  # reset graphical parameters when function exits:
  current_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(current_par))

  # layout
  if (plot_original) {

    if(horiz) {
      graphics::layout(matrix(c(1, 2, 3), 1, 3),
                       widths = c(0.42, 0.42, 0.16))
      h <- FALSE
    } else {
      graphics::layout(matrix(c(1, 2, 3), 3, 1),
                       heights = c(0.42, 0.42, 0.16))
      h <- TRUE
    }

  } else {

    if (horiz) {
      graphics::layout(matrix(c(1, 2), 1, 2),
                       widths = c(0.8, 0.2))
      h <- FALSE
    } else {
      graphics::layout(matrix(c(1, 2), 2, 1),
                       heights = c(0.8, 0.2))
      h <- TRUE
    }

  }

  # plot original if specified
  if (plot_original) {
    graphics::par(mar = c(0, 0, 2, 0))
    plotImageArray(x$original_img,
                   main = "original")
  }

  # plotting image
  graphics::par(mar = c(0, 0, 2, 0))
  recolored_img <- constructImage(x$pixel_assignments,
                                  x$centers)
  plotImageArray(recolored_img, main = "recolored")

  # plotting palette
  graphics::par(mar = rep(0.5, 4))

  if (sizes) {
    sizes <- x$sizes
  } else {
    sizes <- NULL
  }

  plotColorPalette(x$centers, horiz = h,
                   cex_text = cex_text, sizes = sizes)

}
