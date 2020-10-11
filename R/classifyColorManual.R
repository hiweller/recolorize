#' Simplify the colors of an image with user-specified parameters
#'
#' Clusters the colors in an RGB image according to a specified method,
#' then recolors that image to the simplified color scheme.
#'
#' @param img_path Path to the image. Must be a character vector.
#' @param method Method for clustering image colors. One of either `histogram`
#'   or `kmeans`. See details.
#' @param n If `method = "kmeans"`, the number of color clusters to fit.
#' @param bins If `method = "histogram"`, either the number of bins per color
#'   channel (if a single number is provided) OR a vector of length 3 with the
#'   number of bins for each channel.
#' @param lower,upper RGB triplet ranges for setting a bounding box of pixels to
#'   mask. See details.
#' @param transparent Logical. Treat transparent pixels as background? Requires
#'   an alpha channel (PNG). If `TRUE`, overrides `upper` and `lower` arguments.
#' @param resize A value between 0 and 1 for resizing the image (ex. `resize =
#'   0.5` will reduce image size by 50%). Recommended for large images as it can
#'   speed up analysis considerably. See details.
#' @param rotate Degrees to rotate the image clockwise.
#' @param plotting Logical. Plot recolored image & color palette?
#' @param horiz Logical for plotting. Plot output images and color palette side
#'   by side (`TRUE`) or stacked vertically (`FALSE`)?
#' @param scale_palette Logical. If plotting, plot colors in the color palette
#'   proportional to the size of each cluster?
#' @param cex_text If `plotting = TRUE` and `scale_palette = FALSE`, size of
#'   text to display on the color palette numbers.
#'
#' @return A list with the following attributes:
#' \enumerate{
#'     \item `original_img`: The original image, as a 3D array.
#'     \item `recolored_img`: The recolored image, as a 3D array.
#'     \item `method`: The method (kmeans or histogram) used to bin the colors.
#'     \item `color_residuals`: Squared residuals, both per pixel and
#'     summarized (overall and per cluster), calculated by
#'      \code{\link{colorResiduals}}.
#'     \item `color_space`: The associated color space. Currently only RGB.
#'     \item `centers`: A matrix of color centers. If `adjust.centers =
#'         FALSE`, this will be identical to the input `color_centers`.
#'     \item `sizes`: The number of pixels assigned to each color cluster.
#'     \item `pixel_assignments`: A vector of color center assignments for each pixel.
#' }
#'
#' @details
#' Method for color clustering: \code{\link[stats]{kmeans}} clustering tries to
#' find the set of `n` clusters that minimize overall distances between pixels
#' and assigned centers. Histogram binning divides up color space according to
#' set breaks; for example, bins = 2 would divide the red, green, and blue
#' channels into 2 bins each (> 0.5 and < 0 .5), resulting in 8 possible ranges.
#' A white pixel (RGB = 1, 1, 1) would fall into the R > 0.5, G > 0.5, B > 0.5
#' bin. The resulting centers represent the average color of all the pixels
#' assigned to that bin.
#'
#' K-means clustering can produce more intuitive results, but because it is
#' iterative, it will find slightly different clusters each time it is run, and
#' their order will be arbitrary. It also tends to divide up similar colors that
#' make up the majority of the image. Histogram binning will produce the same
#' results every time, in the same order, and because it forces the bins to be
#' dispersed throughout color space, tends to better pick up small color
#' details. Bins are also comparable across images. However, this sometimes
#' means returning empty bins (i.e. the white bin will be empty if clustering a
#' very dark image), and a single color can be split across two bins.
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
#' classifyColorManual(img)
#'
#' # we can also have different numbers of bins per channel
#' classifyColorManual(img, bins = c(4, 1, 1)) # mostly red
#' classifyColorManual(img, bins = c(1, 4, 1)) # mostly green
#' classifyColorManual(img, bins = c(1, 1, 4)) # mostly blue
#'
#' # kmeans can produce a better fit with fewer colors
#' classifyColorManual(img, method = "kmeans", n = 8)
#'
#' # increasing numbers of kmean colors
#' recolored_images <- setNames(vector("list", length = 10), c(1:10))
#' for (i in 1:10) {
#'   recolored_images[[i]] <- classifyColorManual(img, method = "kmeans",
#'                                n = i)
#' }
#' layout(matrix(1:10, nrow = 2, byrow = TRUE))
#' sapply(1:length(recolored_images),
#'       function(i) hist(recolored_images[[i]]$color_residuals$sq_residuals,
#'                        border = NA, main = paste(i, "cluster(s)"),
#'                        freq = FALSE, xlab = "Squared residual",
#'                        xlim = c(0, 1)))
#'
#' residual_vec <- sapply(1:length(recolored_images),
#'                       function(i) recolored_images[[i]]$color_residuals$tot_residuals)
#' plot(residual_vec, type = 'b',
#'     ylab = "Sum of squared residuals",
#'     xlab = "Cluster number")
#'
#' # kmeans, 10 colors
#' kmeans_recolor <- classifyColorManual(img, method = "kmeans",
#'                              n = 8, plotting = FALSE)
#' hist_recolor <- classifyColorManual(img, method = "hist",
#'                            bins = 2, plotting = FALSE)
#'
#' # compare binning vs. kmeans clustering
#' layout(matrix(c(1, 2, 3), ncol = 3))
#' plotImageArray(kmeans_recolor$original_img, main = "original")
#' plotImageArray(kmeans_recolor$recolored_img, main = "kmeans")
#' plotImageArray(hist_recolor$recolored_img, main = "binning")
#' @export
classifyColorManual <- function(img_path, method = "histogram",
                       bins = 2, n = 5,
                       lower = NULL, upper = NULL,
                       transparent = TRUE,
                       resize = NULL, rotate = NULL,
                       plotting = TRUE, horiz = TRUE,
                       cex_text = 1.5, scale_palette = TRUE) {

  # get method
  method <- match.arg(tolower(method), c("kmeans", "histogram"))

  # read in image
  img <- readImage(img_path, resize = resize, rotate = rotate)

  # make background condition
  alpha_channel <- dim(img)[3] == 4 # is there a transparency channel?
  bg_condition <- backgroundCondition(lower = lower, upper = upper,
                                      center = NULL, radius = NULL,
                                      transparent = transparent,
                                      alpha_channel = alpha_channel)

  # index background
  bg_indexed <- backgroundIndex(img, bg_condition)

  # color clusters & assign pixels
  color_clusters <- colorClusters(bg_indexed$non_bg, method = method,
                                  n = n, bins = bins)

  # recolor based on assignments/centers
  recolored <- recolorImage(bg_indexed, color_clusters,
                            plotting = FALSE,
                            remove_empty_clusters = FALSE)

  # get sizes vector
  sizes <- color_clusters$sizes
  if (scale_palette) { s <- sizes } else { s <- NULL }

  # plot result
  if (plotting) {
    plotRecolorized(recolored$recolored_img, img,
                    plot_original = TRUE,
                    recolored$centers, horiz = horiz,
                    cex_text = cex_text,
                    sizes = s)
  }

  # returnables:
  original_img <- img
  recolored_img <- recolored$recolored_img

  # return binning scheme
  method <- if( method == "kmeans" ) {
    list(method = "kmeans", n = n)
  } else {
    list(method = "histogram", bins = bins)
  }

  # only rgb for now...would others be useful?
  color_space <- "RGB"
  centers <- color_clusters$centers
  pixel_assignments <- color_clusters$pixel_assignments

  # calculate residuals
  color_residuals <- colorResiduals(bg_indexed$non_bg,
                                    color_clusters$pixel_assignments,
                                    centers)

  # return em
  return_list <- list(original_img = original_img,
                      recolored_img = recolored_img,
                      method = method,
                      color_residuals = color_residuals,
                      color_space = color_space,
                      centers = centers,
                      sizes = sizes,
                      pixel_assignments = pixel_assignments)

  return(return_list)

}
