#' Plot color clusters in a color space
#'
#' Plots color clusters in a 3D color space.
#'
#' @param centers A matrix of color centers, with rows for centers and columns
#'   as channels. These are interpreted as coordinates.
#' @param sizes A vector of color sizes. Can be relative or absolute; it's going
#'   to be scaled for plotting.
#' @param color_space The color space of the centers. Important for setting the
#'   axis ranges and for converting the colors into hex codes for plotting. The
#'   function assumes that the `centers` argument is already in this color space.
#' @param scaling Factor for scaling the cluster sizes. If your clusters are
#'   way too big or small on the plot, tinker with this.
#' @param plus Value to add to each scaled cluster size; can be helpful for
#'   seeing small or empty bins when they are swamped by larger clusters.
#' @param ... Further parameters passed to [scatterplot3d::scatterplot3d].
#'
#' @details This function does very little on your behalf (e.g. labeling the
#'   axes, setting the axis ranges, trying to find nice scaling parameters,
#'   etc). You can pass those parameters using the `...` function to
#'   [scatterplot3d::scatterplot3d], which is probably a good idea.
#'
#' @examples
#' corbetti <- system.file("extdata/corbetti.png", package = "recolorize")
#' init_fit <- recolorize(corbetti,
#'                        color_space = "Lab",
#'                        method = "k",
#'                        n = 30)
#'
#' # we still have to convert to Lab color space first, since the centers are always RGB:
#' centers <- grDevices::convertColor(init_fit$centers, "sRGB", "Lab")
#' plotColorClusters(centers, init_fit$sizes,
#'                   scaling = 25,
#'                   color_space = "Lab",
#'                   xlab = "Luminance",
#'                   ylab = "a (red-green)",
#'                   zlab = "b (blue-yellow)")
#' @export
plotColorClusters <- function(centers, sizes,
                              scaling = 10,
                              plus = 0,
                              color_space = "sRGB",
                              ...) {

  # get color vector in RGB
  centers <- col2col(centers, from = color_space, to = "sRGB")
  hex_col <- grDevices::rgb(centers)

  # and plot it
  scatterplot3d::scatterplot3d(centers, pch = 20,
                               cex.symbols = (sizes / max(sizes)) * scaling,
                               color = hex_col, ...)

  # BARE BONES, gang...bare bones
}
