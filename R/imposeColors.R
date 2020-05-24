# similar to recolorize, but you specify color centers instead of getting them
# from the image
#' Recolor an image to a provided set of colors
#'
#' Takes an image and a set of color centers, and assigns each pixel to the most
#' similar provided color. Useful for producing a set of images with identical colors.
#'
#' @param img.path Path to the image. Must be a character vector.
#' @param color.centers Colors to map to, as an n x 3 matrix (rows = colors,
#'   columns = channels).
#' @param adjust.centers Logical. After pixel assignment, should the returned
#'   colors be the average color of the pixels assigned to that cluster, or the
#'   original colors?
#' @param lower,upper RGB triplet ranges for setting a bounding box of pixels to
#'   mask. See details.
#' @param transparent Logical. Treat transparent pixels as background? Requires
#'   an alpha channel (PNG).
#' @param resize A value between 0 and 1 for resizing the image (ex. `resize =
#'   0.5` will reduce image size by 50%). Recommended for large images as it can
#'   speed up analysis considerably. See details.
#' @param rotate Degrees to rotate the image clockwise.
#' @param plotting Logical. Plot recolored image & color palette?
#' @param horiz Logical for plotting. Plot output image and color palette side
#'   by side (`TRUE`) or stacked vertically (`FALSE`)?
#' @param scale.palette Logical. If plotting, plot colors in the color palette
#'   proportional to the size of each cluster?
#' @param cex.text If `plotting = TRUE` and `scale.palette = FALSE`, size of
#'   text to display on the color palette numbers.
#'
#' @return A list with the following attributes:
#' \enumerate{
#'     \item `original.img`: The original image, as a 3D array.
#'     \item `recolored.img`: The recolored image, as a 3D array.
#'     \item `color.space`: The associated color space. Currently only RGB.
#'     \item `centers`: A matrix of color centers. If `adjust.centers =
#'         FALSE`, this will be identical to the input `color.centers`.
#'     \item `sizes`: The number of pixels assigned to each color cluster.
#'     \item `pixel.assignments`: A vector of color center assignments for each pixel.
#' }
#'
#' @details
#' Background masking: `lower`, `upper`, and `transparent` are all background
#' masking conditions. Transparency is unambiguous and so tends to produce
#' cleaner results, but the `lower` and `upper` bounds can be used instead to
#' treat pixels in a specific color range as the background. For example, to
#' ignore white pixels (RGB = 1, 1, 1), you might want to mask all pixels whose
#' R, G, and B values exceed 0.9. In that case, `lower = c(0.9, 0.9, 0.9)` and
#' `upper = c(1, 1, 1)`. Regardless of input background, recolored images are
#' returned with transparent backgrounds by adding an alpha channel if one does
#' not already exist.
#'
#' Resizing: The speed benefits of downsizing images are fairly obvious (fewer pixels = fewer operations).
#' Because recoloring the images simplifies their detail anyways, downsizing
#' prior to recoloring doesn't run a very high risk of losing important information.
#' A general guidelines for resizing is that any distinguishable features of interest
#' should still take up at least 2 pixels (preferably with a margin of error) in the
#' resized image.
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
#' #' # plot it
#' recolorize::plotColorPalette(ctrs)
#'
#' # get image paths
#' ocellata <- system.file("extdata/ocellata.png", package = "recolorize)
#'
#' # map to rgb extremes
#' recolorize::imposeColors(ocellata, ctrs, adjust.centers = FALSE)
#'
#' # looks much better if we recalculate the centers from the image
#' recolorize::imposeColors(ocellata, ctrs, adjust.centers = TRUE)
#'
#' # we can map one image to extracted colors from another image
#' ocellata.colors <- recolorize(ocellata)
imposeColors <- function(img.path, color.centers,
                           adjust.centers = TRUE,
                           lower = NULL, upper = NULL,
                           transparent = TRUE,
                           resize = NULL, rotate = NULL,
                           plotting = TRUE, horiz = TRUE,
                           cex.text = 1.5, scale.palette = TRUE) {

  # read in image
  img <- readImage(img.path, resize = resize, rotate = rotate)

  # make background condition
  alpha.channel <- dim(img)[3] == 4 # is there a transparency channel?
  bg.condition <- backgroundCondition(lower = lower, upper = upper,
                                      center = NULL, radius = NULL,
                                      transparent = transparent,
                                      alpha.channel = alpha.channel)

  # index background
  bg.indexed <- backgroundIndex(img, bg.condition)

  # color clusters & assign pixels
  color.clusters <- assignPixels(color.centers, bg.indexed$non.bg,
                                 adjust.centers = adjust.centers)

  # recolor based on assignments/centers
  recolored <- recolorImage(bg.indexed, color.clusters,
                            plotting = FALSE,
                            remove.empty.clusters = FALSE)

  # get sizes vector
  sizes <- color.clusters$sizes
  if (scale.palette) { s <- sizes } else { s <- NULL }

  # plot result
  if (plotting) {
    plotRecolorized(recolored$recolored.img, img,
                    plot.original = TRUE,
                    recolored$centers, horiz = horiz,
                    cex.text = cex.text,
                    sizes = s)
  }

  # returnables:
  original.img <- img
  recolored.img <- recolored$recolored.img

  # only rgb for now...would others be useful?
  color.space <- "RGB"
  centers <- color.clusters$centers
  pixel.assignments <- color.clusters$pixel.assignments

  # return em
  return.list <- list(original.img = original.img,
                      recolored.img = recolored.img,
                      color.space = color.space,
                      centers = centers,
                      sizes = sizes,
                      pixel.assignments = pixel.assignments)

}
