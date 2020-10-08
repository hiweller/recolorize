#' Simplify the colors of an image
#'
#' Clusters the colors in an RGB image according to a specified method,
#' then recolors that image to the simplified color scheme.
#'
#' @param img Path to the image (a character vector) or a 3D image array as read
#'   in by \code{\link[png]{readPNG}} \code{{readImage}}.
#' @param method Method for clustering image colors. One of either `histogram`
#'   or `kmeans`. See details.
#' @param n If `method = "kmeans"`, the number of color clusters to fit.
#' @param bins If `method = "histogram"`, either the number of bins per color
#'   channel (if a single number is provided) OR a vector of length 3 with the
#'   number of bins for each channel.
#' @param color.space Color space in which to minimize distances, passed to
#'   \code{\link{grDevices}{convertColor}}. One of "sRGB", "Lab", or "Luv".
#'   Default is "Lab", a perceptually uniform (for humans) color space.
#' @param ref.white Reference white for converting to different color spaces.
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
#' @param scale.palette Logical. If plotting, plot colors in the color palette
#'   proportional to the size of each cluster?
#' @param cex.text If `plotting = TRUE` and `scale.palette = FALSE`, size of
#'   text to display on the color palette numbers.
#'
#' @return A list with the following attributes:
#' \enumerate{
#'     \item `original.img`: The original image, as a 3D array.
#'     \item `recolored.img`: The recolored image, as a 3D array.
#'     \item `centers`: A matrix of color centers. If `adjust.centers =
#'         FALSE`, this will be identical to the input `color.centers`.
#'     \item `sizes`: The number of pixels assigned to each color cluster.
#'     \item `pixel.assignments`: A vector of color center assignments for each pixel.
#' }
#'
#' @details
#' Method for color clustering: \code{\link[stats]{kmeans}} clustering tries to
#' find the set of `n` clusters that minimize overall distances. Histogram
#' binning divides up color space according to set breaks; for example, bins = 2
#' would divide the red, green, and blue channels into 2 bins each (> 0.5 and <
#' 0 .5), resulting in 8 possible ranges. A white pixel (RGB = 1, 1, 1) would
#' fall into the R > 0.5, G > 0.5, B > 0.5 bin. The resulting centers represent
#' the average color of all the pixels assigned to that bin.
#'
#' K-means clustering can produce more intuitive results, but because it is
#' iterative, it will find slightly different clusters each time it is run, and
#' their order will be arbitrary. It also tends to divide up similar colors that
#' make up the majority of the image. Histogram binning will produce the same
#' results every time, in the same order, and because it forces the bins to be
#' dispersed throughout color space, tends to better pick up small color
#' details. Bins are also comparable across images. However, this sometimes
#' means returning empty bins (i.e. the white bin will be empty if clustering a
#' very dark image).
#'
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
#' of losing important information. A general guideline for resizing is that
#' any distinguishable features of interest should still take up at least 2
#' pixels (preferably with a margin of error) in the resized image.
#'
#' @examples
#'
#' # filepath to image
#' img <- system.file("extdata/chongi.png", package = "recolorize")
#'
#' # default: histogram, 2 bins/channel
#' recolorize(img)
#'
#' # we can also have different numbers of bins per channel
#' recolorize(img, bins = c(4, 1, 1)) # mostly red
#' recolorize(img, bins = c(1, 4, 1)) # mostly green
#' recolorize(img, bins = c(1, 1, 4)) # mostly blue
#'
#' # kmeans can produce a better fit with fewer colors
#' recolorize(img, method = "kmeans", n = 8)
#'
#' # increasing numbers of kmean colors
#' recolored.images <- setNames(vector("list", length = 10), c(1:10))
#' for (i in 1:10) {
#'   kmeans.recolor <- recolorize(img, method = "kmeans",
#'                                n = i)
#' }
#'
#' # kmeans, 10 colors
#' kmeans.recolor <- recolorize(img, method = "kmeans",
#'                              n = 8, plotting = FALSE)
#' hist.recolor <- recolorize(img, method = "hist",
#'                            bins = 2, plotting = FALSE)
#'
#' # compare binning vs. kmeans clustering
#' layout(matrix(c(1, 2, 3), ncol = 3))
#' plotImageArray(kmeans.recolor$original.img, main = "original")
#' plotImageArray(kmeans.recolor$recolored.img, main = "kmeans")
#' plotImageArray(hist.recolor$recolored.img, main = "binning")
#' @export
recolorize <- function(img, method = "histogram",
                       bins = 2, n = 5,
                       color.space = "sRGB", ref.white = "D65",
                       lower = NULL, upper = NULL,
                       transparent = TRUE,
                       resid = FALSE,
                       resize = NULL, rotate = NULL,
                       plotting = TRUE, horiz = TRUE,
                       cex.text = 1.5, scale.palette = TRUE) {

  # get method
  method <- match.arg(tolower(method), c("kmeans", "histogram"))

  # if 'img' is a filepath, read in image
  if (is.character(img)) {
    if (file.exists(img)) {
      img <- readImage(img, resize = resize, rotate = rotate)
    } else {
      stop(paste("Could not find", img))
    }

  } else if (!is.array(img) | length(dim(img)) != 3) {

    # otherwise, make sure it's an image array
    stop("'img' must be a path to an image or an image array.")

  }

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
                                  n = n, bins = bins,
                                  color.space = color.space,
                                  ref.white = ref.white)

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

  # get residuals if TRUE
  if (resid) {
    return.list$resids <- colorResiduals(bg.indexed$non.bg,
                                         color.clusters$pixel.assignments,
                                         centers)
  }


  # set class
  class(return.list) <- "recolorize"

  # and...you know
  return(return.list)

}
