#' Plot a 3D array as an RGB image
#'
#' Does what it says on the tin. An extremely simple wrapper for
#' \code{\link[graphics]{rasterImage}}, but maintains aspect ratio, removes
#' axes, and reduces margins for cleaner plotting.
#'
#' @param rgb.array A 3D array of RGB values. Preferably output from
#'   \code{\link[png]{readPNG}}, \code{\link[jpeg]{readJPEG}}, or
#'   \code{\link[recolorize]{recolorImage}}.
#' @param main Optional title for plot.
#'
#' @examples
#' # make a 100x100 image of random colors
#' random.colors <- array(runif(100 * 100 * 3),
#'                        dim = c(100, 100, 3))
#' recolorize::plotImageArray(random.colors)
#'
#' # we can also plot...a real image
#' corbetti <- system.file("extdata/corbetti.png",
#'                        package = "recolorize")
#' img <- png::readPNG(corbetti)
#' plotImageArray(img)
#' @export
plotImageArray <- function(rgb.array, main = "") {

  # Make sure the array is 3-dimensional
  if (length(dim(rgb.array)) != 3) {
    stop("RGB_array must be an array of three dimensions (pixel rows,
             pixel columns, and color channels)")
  }

  # Change graphical parameters for image display
  op <- graphics::par(mar = c(0, 0, 2, 0))
  asp <- dim(rgb.array)[1] / dim(rgb.array)[2]

  # Initialize empty plot window
  graphics::plot(0:1, 0:1, type = "n", ann = F, axes = F, asp = asp)

  # Use rasterImage to actually plot the image
  graphics::rasterImage(rgb.array, 0, 0, 1, 1)
  graphics::title(main, line = 0)

  # Return to original graph window settings
  graphics::par(op)
}
