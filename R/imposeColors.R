#' Recolor an image to a provided set of colors
#'
#' Takes an image and a set of color centers, and assigns each pixel to the most
#' similar provided color. Useful for producing a set of images with identical colors.
#'
#' @param img Path to the image (a character vector) or a 3D image array as read
#'   in by [png::readPNG()] \code{{readImage}}.
#' @param centers Colors to map to, as an n x 3 matrix (rows = colors,
#'   columns = channels).
#' @param adjust_centers Logical. After pixel assignment, should the returned
#'   colors be the average color of the pixels assigned to that cluster, or the
#'   original colors?
#' @param color_space Color space in which to minimize distances. One of "sRGB",
#'   "Lab", "Luv", "HSV", or "XYZ". Default is "Lab", a perceptually uniform
#'   (for humans) color space.
#' @param ref_white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight.
#' @param lower,upper RGB triplet ranges for setting a bounding box of pixels to
#'   mask. See details.
#' @param transparent Logical. Treat transparent pixels as background? Requires
#'   an alpha channel (PNG).
#' @param resid Logical. Return a list of different residual metrics to
#'   describe the goodness of fit?
#' @param resize A value between 0 and 1 for resizing the image (ex. `resize =
#'   0.5` will reduce image size by 50%). Recommended for large images as it can
#'   speed up analysis considerably. See details.
#' @param rotate Degrees to rotate the image clockwise.
#' @param plotting Logical. Plot recolored image & color palette?
#' @param horiz Logical for plotting. Plot output image and color palette side
#'   by side (`TRUE`) or stacked vertically (`FALSE`)?
#' @param scale_palette Logical. If plotting, plot colors in the color palette
#'   proportional to the size of each cluster?
#' @param cex_text If `plotting = TRUE` and `scale_palette = FALSE`, size of
#'   text to display on the color palette numbers.
#'
#' @return A list with the following attributes:
#' \enumerate{
#'     \item `original_img`: The original image, as a raster.
#'     \item `centers`: A matrix of color centers. If `adjust_centers =
#'         FALSE`, this will be identical to the input `centers`.
#'     \item `sizes`: The number of pixels assigned to each color cluster.
#'     \item `pixel_assignments`: A vector of color center assignments for each pixel.
#'     \item `call`: The call(s) used to generate the `recolorize` object.
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
#' Resizing: The speed benefits of downsizing images are fairly obvious (fewer
#' pixels = fewer operations). Because recoloring the images simplifies their
#' detail anyways, downsizing prior to recoloring doesn't run a very high risk
#' of losing important information. A general guideline for resizing is that any
#' distinguishable features of interest should still take up at least 2 pixels
#' (preferably with a margin of error) in the resized image.
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
#' # plot it
#' recolorize::plotColorPalette(ctrs)
#'
#' # get image paths
#' ocellata <- system.file("extdata/ocellata.png", package = "recolorize")
#'
#' # map to rgb extremes
#' ocellata_fixed <- recolorize::imposeColors(ocellata, ctrs,
#'                                             adjust_centers = FALSE)
#'
#' # looks much better if we recalculate the centers from the image
#' ocellata_adjusted <- recolorize::imposeColors(ocellata, ctrs,
#'                                            adjust_centers = TRUE)
#'
#' # we can map one image to extracted colors from another image
#' # extract ocellata colors
#' ocellata_colors <- recolorize(ocellata)
#'
#' # map fulgidissima to ocellata colors
#' fulgidissima <- system.file("extdata/fulgidissima.png",
#'                              package = "recolorize")
#'
#' fulgidissma_ocellata <- recolorize::imposeColors(fulgidissima,
#'                        ocellata_colors$centers,
#'                        adjust_centers = FALSE)
#'
#' @export
imposeColors <- function(img, centers,
                           adjust_centers = TRUE,
                           color_space = "sRGB",
                           ref_white = "D65",
                           lower = NULL, upper = NULL,
                           transparent = TRUE,
                           resid = FALSE,
                           resize = NULL, rotate = NULL,
                           plotting = TRUE, horiz = TRUE,
                           cex_text = 1.5, scale_palette = TRUE) {

  # if 'img' is a filepath, read in image
  if (is.character(img)) {
    if (file.exists(img)) {

      # read image
      img <- readImage(img, resize = resize, rotate = rotate)

    } else {

      # oops
      stop("Invalid file path.")

    }
  } else if (!is.array(img) | length(dim(img)) != 3) {

    # otherwise, make sure it's an image array
    stop("'img' must be a path or an image or an image array.")

  }

  # make background condition
  alpha_channel <- dim(img)[3] == 4 # is there a transparency channel?
  bg_condition <- backgroundCondition(lower = lower, upper = upper,
                                      center = NULL, radius = NULL,
                                      transparent = transparent,
                                      alpha_channel = alpha_channel)

  # index background
  bg_indexed <- backgroundIndex(img, bg_condition)

  if (is.null(dim(centers))) {
    centers <- matrix(centers, ncol = 3, byrow = TRUE)
  }

  # color clusters & assign pixels
  color_clusters <- assignPixels(centers, bg_indexed$non_bg,
                                 adjust_centers = adjust_centers)
  sizes <- color_clusters$sizes
  color_clusters <- pixelAssignMatrix(bg_indexed, color_clusters)
  color_clusters$sizes <- sizes
  class(color_clusters) <- "color_clusters"

  # get sizes vector
  if (scale_palette) { s <- sizes } else { s <- NULL }

  # returnables:
  original_img <- img

  # only rgb for now...would others be useful?
  color_space <- color_space
  centers <- color_clusters$centers
  pixel_assignments <- color_clusters$pixel_assignments

  # make returnable
  return_list <- list(original_img = grDevices::as.raster(original_img),
                      centers = centers,
                      sizes = sizes,
                      pixel_assignments = pixel_assignments,
                      call = match.call())
  class(return_list) <- "recolorize"

  # plot result
  if (plotting) {
    plot.recolorize(return_list, horiz = horiz,
                    cex_text = cex_text,
                    sizes = TRUE)
  }

  # residuals if TRUE
  if (resid) {
    return_list$resids <- colorResiduals(bg_indexed$non_bg,
                                         color_clusters$pixel_assignments,
                                         centers)
  }

  # return it
  return(return_list)

}
