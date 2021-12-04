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
#' @param phi,theta Viewing angles (in degrees).
#' @param alpha Transparency (0-1 range).
#' @param ... Further parameters passed to [plot3D::scatter3D].
#'
#' @return Nothing; plots a 3D scatterplot of color clusters, with corresponding
#' colors and sizes.
#'
#' @details This function does very little on your behalf (e.g. labeling the
#'   axes, setting the axis ranges, trying to find nice scaling parameters,
#'   etc). You can pass those parameters using the `...` function to
#'   [plot3D::scatter3D], which is probably a good idea.
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
#'                   zlab = "b (blue-yellow)",
#'                   cex.lab = 0.5)
#' @export
plotColorClusters <- function(centers, sizes,
                              scaling = 10,
                              plus = 0,
                              color_space = "sRGB",
                              phi = 35, theta = 60,
                              alpha = 0.5,
                              ...) {

  # get color vector in RGB
  centers_rgb <- col2col(centers, from = color_space, to = "sRGB")
  hex_col <- grDevices::rgb(centers_rgb, alpha = alpha)

  # normalize sizes
  sizes <- sizes / sum(sizes)

  # make blank plot
  plot3D::scatter3D(x = centers[ , 1],
                    y = centers[ , 2],
                    z = centers[ , 3],
                    cex = 0, colkey = FALSE,
                    phi = phi, theta = theta,
                    ...)

  # add one point at a time, setting size with the cex argument
  for (i in 1:nrow(centers)) {
    plot3D::scatter3D(x = centers[i , 1],
                      y = centers[i , 2],
                      z = centers[i , 3],
                      cex = sizes[i] * scaling + plus,
                      pch = 19, alpha = alpha,
                      col = hex_col[i], add = TRUE)
  }

  # BARE BONES, gang...bare bones
}
