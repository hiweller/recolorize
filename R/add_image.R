#' Add a raster image to a plot
#'
#' Adds a raster image (a 3D array) to an existing plot as an image.
#' A silly, generic function, but nice for visualization. Sort of like
#' [graphics::points], but for images.
#'
#' @param obj An array of the dimensions height x width x channels,
#' such as read in by [png::readPNG] or [readImage], or the `original_img`
#' and `recolored_img` elements of a `recolorize` object.
#' @param x,y The x and y coordinates on which the image should be centered.
#' @param width Image width, in x-axis units.
#' @param interpolate Passed to [graphics::rasterImage]. Use linear
#' interpolation when scaling the image?
#' @param angle Passed to [graphics::rasterImage]. The angle (in degrees)
#' for rotating the image.
#'
#' @examples
#' images <- dir(system.file("extdata", package = "recolorize"),
#'               ".png", full.names = TRUE)
#' x <- runif(5)
#' y <- runif(5)
#' plot(x, y,
#'      xlim = range(x) + c(-0.2, 0.2),
#'      ylim = range(y) + c(-0.2, 0.2))
#' for (i in 1:length(images)) {
#'   img <- readImage(images[i])
#'   add_image(img, x[i], y[i], width = 0.1)
#' }
#'
#' @export
add_image <- function(obj, x = NULL,
                      y = NULL,
                      width = NULL,
                      interpolate = TRUE,
                      angle = 0){

  usr <- graphics::par()$usr
  pin <- graphics::par()$pin
  imdim <- dim(obj)
  sf <- imdim[1] / imdim[2]

  w <- width / (usr[2] - usr[1]) * pin[1]
  h <- w * sf
  hu <- h / pin[2] * (usr[4] - usr[3])

  graphics::rasterImage(image = obj,
                        xleft = x - (width / 2), xright = x + (width / 2),
                        ybottom = y - (hu / 2), ytop = y + (hu/2),
                        interpolate = interpolate,
                        angle = angle)
}
