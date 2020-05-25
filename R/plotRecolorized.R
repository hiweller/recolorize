# plots a recolored image + its color palette
#' Plot recolorized image results
#'
#' Plots a side-by-side comparison of an original image and its recolorized
#' version, plus the color palette used for recoloring. Called by
#' \code{\link[recolorize]{imposeClusters}} and
#' \code{\link[recolorize]{recolorize}}.
#'
#' @param recolored.img The recolored image, as a 3D RGB array.
#' @param original.img The original image, as a 3D RGB array.
#' @param plot.original Logical. Plot the original image for comparison?
#' @param color.centers The colors to plot in the color palette, as a numeric
#'   matrix (rows = colors, columns = channels).
#' @param horiz Logical. Should plots be stacked vertically or horizontally?
#' @param cex.text,sizes Plotting parameters passed to
#'   \code{\link{recolorize}{plotColorPalette}}.
#'
#' @examples
#' corbetti <- system.file("extdata/corbetti.png",
#'                          package = "recolorize")
#'
#' corbetti.recolor <- recolorize::recolorize(corbetti,
#'                                           method = "hist",
#'                                           bins = 2, plotting = F)
#'
#' # full color palette
#' plotRecolorized(recolored.img = corbetti.recolor$recolored.img,
#'                 original.img = corbetti.recolor$original.img,
#'                 color.centers = corbetti.recolor$centers)
#'
#' # scaled color palette
#' plotRecolorized(recolored.img = corbetti.recolor$recolored.img,
#'                 original.img = corbetti.recolor$original.img,
#'                 color.centers = corbetti.recolor$centers,
#'                 sizes = corbetti.recolor$sizes)
#'
#' @export
plotRecolorized <- function(recolored.img, original.img,
                            plot.original = TRUE,
                            color.centers,
                            horiz = FALSE,
                            cex.text = 2, sizes = NULL) {

  # for resetting
  user.par <- graphics::par(no.readonly = TRUE)

  # layout
  if (plot.original) {

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
  if (plot.original) {
    graphics::par(mar = rep(0.5, 4))
    plotImageArray(original.img)
  }

  # plotting image
  graphics::par(mar = rep(0.5, 4))
  plotImageArray(recolored.img)

  # plotting palette
  graphics::par(mar = rep(0.5, 4))
  plotColorPalette(color.centers, horiz = h,
                   cex.text = cex.text, sizes = sizes)

  # reset parameters
  graphics::par(user.par)

}
