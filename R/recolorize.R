#' Simplify the colors of an image
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
recolorize <- function(img.path, method = "kmeans",
                       bins = 3, n = 10,
                       lower = NULL, upper = NULL,
                       transparent = TRUE,
                       resize = NULL, rotate = NULL,
                       plotting = TRUE, horiz = TRUE,
                       cex.text = 1.5, scale.palette = TRUE) {

  # get method
  method <- match.arg(tolower(method), c("kmeans", "histogram"))

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
  color.clusters <- colorClusters(bg.indexed$non.bg, method = method,
                                  n = n, bins = bins)

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

  # return binning scheme
  method <- if( method == "kmeans" ) {
    list(method = "kmeans", n = n)
  } else {
    list(method = "histogram", bins = bins)
  }

  # only rgb for now...would others be useful?
  color.space <- "RGB"
  centers <- color.clusters$centers
  pixel.assignments <- color.clusters$pixel.assignments

  # return em
  return.list <- list(original.img = original.img,
                      recolored.img = recolored.img,
                      method = method,
                      color.space = color.space,
                      centers = centers,
                      sizes = sizes,
                      pixel.assignments = pixel.assignments)

}
