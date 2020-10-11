# plots a recolored image + its color palette
#' Plot recolorized image results
#'
#' Plots a side-by-side comparison of an original image and its recolorized
#' version, plus the color palette used for recoloring. Called by
#' \code{\link[recolorize]{imposeColors}} and
#' \code{\link[recolorize]{recolorize}}.
#'
#' @param recolored_img The recolored image, as a 3D RGB array.
#' @param original_img The original image, as a 3D RGB array.
#' @param plot_original Logical. Plot the original image for comparison?
#' @param color_centers The colors to plot in the color palette, as a numeric
#'   matrix (rows = colors, columns = channels).
#' @param horiz Logical. Should plots be stacked vertically or horizontally?
#' @param cex_text,sizes Plotting parameters passed to
#'   \code{\link{recolorize}{plotColorPalette}}.
#'
#' @examples
#' corbetti <- system.file("extdata/corbetti.png",
#'                          package = "recolorize")
#'
#' corbetti_recolor <- recolorize(corbetti, method = "hist",
#'                                          bins = 2, plotting = FALSE)
#'
#' # full color palette
#' plotRecolorized(recolored_img = corbetti_recolor$recolored_img,
#'                 original_img = corbetti_recolor$original_img,
#'                 color_centers = corbetti_recolor$centers)
#'
#' # scaled color palette
#' plotRecolorized(recolored_img = corbetti_recolor$recolored_img,
#'                 original_img = corbetti_recolor$original_img,
#'                 color_centers = corbetti_recolor$centers,
#'                 sizes = corbetti_recolor$sizes)
#'
#' @export
plotRecolorized <- function(recolored_img, original_img,
                            plot_original = TRUE,
                            color_centers,
                            horiz = FALSE,
                            cex_text = 2, sizes = NULL) {

  # for resetting
  user_par <- graphics::par(no.readonly = TRUE)

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
    graphics::par(mar = rep(0.5, 4))
    plotImageArray(original_img, main = "original")
  }

  # plotting image
  graphics::par(mar = rep(0.5, 4))
  plotImageArray(recolored_img, main = "recolored")

  # plotting palette
  graphics::par(mar = rep(0.5, 4))
  plotColorPalette(color_centers, horiz = h,
                   cex_text = cex_text, sizes = sizes)

  # reset parameters
  graphics::par(user_par)

}
