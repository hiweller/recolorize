#' Recolorize with automatic thresholding
#'
#' Calls [recolorize] and [recluster] in sequence, since these are often
#' very effective in combination.
#'
#' @param img Path to the image (a character vector) or a 3D image array as read
#'   in by \code{\link[png]{readPNG}} \code{{readImage}}.
#' @param method Method for clustering image colors. One of either `histogram`
#'   or `kmeans`. See details.
#' @param n If `method = "kmeans"`, the number of color clusters to fit.
#' @param bins If `method = "histogram"`, either the number of bins per color
#'   channel (if a single number is provided) OR a vector of length 3 with the
#'   number of bins for each channel.
#' @param color_space Color space in which to minimize distances, passed to
#'   \code{\link{grDevices}{convertColor}}. One of "sRGB", "Lab", or "Luv".
#'   Default is "sRGB".
#' @param recluster_color_space Color space in which to group colors for
#'   reclustering. Default is CIE Lab.
#' @param cutoff Numeric similarity cutoff for grouping color centers
#'   together. The range is in absolute Euclidean distance. In CIE Lab space,
#'   it is greater than 0-100, but cutoff values between 20 and 80
#'   will usually work best. In RGB space, range is 0-sqrt(3).
#'   See [recluster] details.
#' @param n_final Final number of desired colors; alternative to specifying
#'  a similarity cutoff. Overrides `similarity_cutoff` if provided.
#' @param refit_method Method for refitting the image with the new color
#'   centers. One of either "impose" or "merge". \code{\link{imposeColors}}
#'   refits the original image using the new colors (slow but often better
#'   results). \code{\link{mergeLayers}} merges the layers of the existing
#'   recolored image. This is faster since it doesn't require a new fit, but can
#'   produce messier results.
#' @param ref_white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight.
#' @param lower,upper RGB triplet ranges for setting a bounding box of pixels to
#'   mask. See details.
#' @param transparent Logical. Treat transparent pixels as background? Requires
#'   an alpha channel (PNG).
#' @param resize A value between 0 and 1 for resizing the image (ex. `resize =
#'   0.5` will reduce image size by 50%). Recommended for large images as it can
#'   speed up analysis considerably. See details.
#' @param rotate Degrees to rotate the image clockwise.
#' @param plotting Logical. Plot final results?
#'
#' @return An object of S3 class `recolorize` with the following attributes:
#' \enumerate{
#'     \item `original_img`: The original image, as a raster array.
#'     \item `centers`: A matrix of color centers in RGB (0-1 range).
#'     \item `sizes`: The number of pixels assigned to each color cluster.
#'     \item `pixel_assignments`: A matrix of color center assignments for each
#'     pixel.
#' }
#'
#' @seealso [recolorize], [recluster]
#'
#' @examples
#' # get image path
#' img <- system.file("extdata/corbetti.png", package = "recolorize")
#'
#' # fit recolorize:
#' rc <- recolorize2(img, bins = 2, cutoff = 45)
#'
#' @export
recolorize2 <- function(img, method = "histogram",
                        bins = 2, n = 5,
                        cutoff = 20, n_final = NULL,
                        color_space = "sRGB",
                        recluster_color_space = "Lab",
                        refit_method = "impose",
                        ref_white = "D65",
                        lower = NULL, upper = NULL,
                        transparent = TRUE,
                        resize = NULL, rotate = NULL,
                        plotting = TRUE) {

  # initial fit - don't plot yet
  fit1 <- recolorize(img, method = method,
                     bins = bins, n = n,
                     color_space = color_space,
                     ref_white = ref_white,
                     lower = lower, upper = upper,
                     transparent = transparent,
                     resize = resize, rotate = rotate,
                     plotting = FALSE)

  # recluster
  fit2 <- recluster(fit1, color_space = recluster_color_space,
                    ref_white = ref_white,
                    similarity_cutoff = cutoff,
                    n_final = n_final, refit_method = refit_method,
                    plot_hclust = plotting, plot_final = plotting)

  return(fit2)
}
