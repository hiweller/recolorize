#' Recluster color centers based on color similarity
#'
#' Color mapping (as with k-means or binning) often requires over-clustering in
#' order to recover details in an image. This can result in larger areas of
#' relatively uniform color being split into multiple colors, or in regions with
#' greater variation (due to lighting, shape, reflection, etc) being split into
#' multiple colors. This function clusters the color centers by visual
#' similarity (in CIE Lab space), then returns the re-clustered object. Users
#' can either set a similarity cutoff or a final number of colors. See examples.
#'
#' @param recolorize_obj A recolorize object from \code{\link{recolorize}},
#'   \code{\link{recluster}}, or \code{\link{imposeColors}}.
#' @param similarity_cutoff Numeric similarity cutoff for grouping color centers
#'   together. The range is in absolute Euclidean distance in CIE Lab space,
#'   which means it is greater than 0-100, but cutoff values between 20 and 80
#'   will usually work best. See details.
#' @param n_final Final number of desired colors; alternative to specifying
#'  a similarity cutoff. Overrides `similarity_cutoff` if provided.
#' @param refit_method Method for refitting the image with the new color
#'   centers. One of either "impose" or "merge". \code{\link{imposeColors}}
#'   refits the original image using the new colors (slow but often better
#'   results). \code{\link{mergeLayers}} merges the layers of the existing
#'   recolored image. This is faster since it doesn't require a new fit, but can
#'   produce messier results.
#' @param color_space Color space in which to cluster centers, passed to
#'   \code{\link{grDevices}{convertColor}}. One of "sRGB", "Lab", or "Luv".
#'   Default is "Lab", a perceptually uniform (for humans) color space.
#' @param ref_white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight.
#' @param plot_hclust Logical. Plot the hierarchical clustering tree for
#'  color similarity? Helpful for troubleshooting a cutoff.
#' @param resid Logical. Get final color fit residuals with
#'   \code{\link{colorResiduals}}?
#' @param color_space_fit Passed to \code{\link{imposeColors}}. What
#'   color space should the image be reclustered in?
#' @param plot_final Logical. Plot the final color fit?
#'
#' @return
#' An \code{\link{imposeColors}} object with the re-fit color centers.
#'
#' @details
#' This function is fairly straightforward: the RGB color centers of the
#' recolorize object are converted to CIE Lab color space (which is
#' approximately perceptually uniform for human vision), clustered using
#' \code{\link[stats]{hclust}}, then grouped using \code{\link[stats]{cutree}}.
#' The resulting groups are then passed as the assigned color centers to
#' \code{\link{imposeColors}}, which re-fits the *original* image using the new
#' centers.
#'
#' The similarity cutoff does not require the user to specify the final number
#' of colors, unlike k-means or `n_final`, meaning that the same cutoff could be
#' used for multiple images (with different numbers of colors) and produce
#' relatively good fits. Because the cutoff is in absolute Euclidean distance in
#' CIE Lab space for sRGB colors, the possible range of distances (and therefore
#' cutoffs) is from 0 to >200. The higher the cutoff, the more dissimilar colors
#' will be grouped together. There is no universally recommended cutoff; the
#' same degree of color variation due to lighting in one image might be
#' biologically relevant in another.
#'
#' @examples
#' # get an image
#' corbetti <- system.file("extdata/corbetti.png", package = "recolorize")
#'
#' # too many color centers
#' recolored_corbetti <- recolorize(corbetti, bins = 3)
#'
#' # just enough!
#' # check previous plot for clustering cutoff
#' recluster_obj <- recluster(recolored_corbetti,
#'                            similarity_cutoff = 60,
#'                            plot_hclust = TRUE,
#'                            refit_method = "impose")
#'
#' # compare to merging layers - quite different results:
#' recluster_merge <- recluster(recolored_corbetti,
#'                            similarity_cutoff = 60,
#'                            plot_hclust = TRUE,
#'                            refit_method = "merge")
#'
#' # compare to the result using k-means clustering and the same n:
#' kmeans_fit <- recolorize(corbetti, "k", n = 6)
#' # (you also get different clusters every time you run this)
#'
#' # a cutoff that's too severe will usually just produce "light",
#' # "dark", and "other" colors:
#' recluster_obj <- recluster(recolored_corbetti,
#'                            similarity_cutoff = 100,
#'                            plot_hclust = TRUE)
#'
#' # we get the same result by specifying n_final = 6
#' recluster_obj <- recluster(recolored_corbetti,
#'                            n_final = 6,
#'                            plot_hclust = TRUE)
#'
#' @export
#'
recluster <- function(recolorize_obj,
                      color_space = "Lab",
                      ref_white = "D65",
                       similarity_cutoff = 60,
                       n_final = NULL,
                       plot_hclust = TRUE,
                      refit_method = "impose",
                      resid = FALSE,
                      plot_final = TRUE,
                      color_space_fit = "sRGB") {

  # courtesy:
  current_par <- graphics::par()

  # rename, to keep things clear
  init_fit <- recolorize_obj
  init_fit <- expand_recolorize(init_fit,
                                original_img = TRUE)

  # first, ignore empty clusters -- they're not informative
  sizes <- init_fit$sizes
  centers <- init_fit$centers

  # if any are empty, remove them
  if (any(sizes == 0)) {
    zero_idx <- which(sizes == 0)
    sizes <- sizes[-zero_idx]
    centers <- init_fit$centers[-zero_idx, ]
  }

  # convert to Lab space for better clustering
  lab_init <- col2col(centers,
                      from = "sRGB",
                      to = color_space,
                      ref_white = ref_white)

  # get distance matrix
  d <- stats::dist(lab_init)

  # perform clustering
  # hc <- hclust(d / max(d))
  hc <- stats::hclust(d)

  # plot clustering:
  if (plot_hclust) {

    graphics::par(mfrow = c(1, 1), mar = c(1, 3, 3, 1))
    hex_cols <- grDevices::rgb(init_fit$centers)
    sizes <- init_fit$sizes
    hcd <- stats::as.dendrogram(hc)
    hcd <- stats::dendrapply(hcd, function(x) labelCol(x, hex_cols, cex = 3))

    graphics::par(mar = c(3, 2, 0, 0))
    plot(hcd, xlab = "", ylab = paste(color_space, "color distance"))

    # plot cutoff value if provided:
    if (is.null(n_final)) {
      graphics::abline(h = similarity_cutoff, lty = 2, col = "red", lwd = 2)
    }

  }

  # form groups
  clust_groups <- stats::cutree(hc, k = n_final,
                         h = similarity_cutoff)
  merge_list <- lapply(unique(clust_groups),
                       function(i) which(clust_groups == i))

  refit_method <- match.arg(refit_method, c("imposeColors", "mergeLayers"))

  if (refit_method == "imposeColors") {
    # get weighted avg new colors:
    for (i in 1:length(merge_list)) {
      temp_colors <- centers[merge_list[[i]], ]
      if (is.null(nrow(temp_colors))) {
        new_color <- temp_colors
      } else {
        new_color <- apply(temp_colors, 2, function(j)
          stats::weighted.mean(j, w = sizes[merge_list[[i]]]))
      }

      # make new dataframe/add new colors:
      if (i == 1) {
        new_centers <- data.frame(R = new_color[1],
                                  G = new_color[2],
                                  B = new_color[3])
      } else {
        new_centers <- rbind(new_centers, new_color)
      }
    }

    # and refit:
    final_fit <- imposeColors(init_fit$original_img,
                              centers = new_centers,
                              plotting = FALSE)
  } else if (refit_method == "mergeLayers") {
    # the hiccup here is that we removed some empty clusters (above)
    # so the indexing no longer matches
    init_fit$centers <- centers
    init_fit$sizes <- sizes
    final_fit <- mergeLayers(init_fit,
                             merge_list = merge_list,
                             plotting = FALSE)

  }

  # if plotting...
  if (plot_final) {

    # first, set nice margins and layout
    graphics::par(mar = rep(0, 4))
    graphics::layout(matrix(1:4, nrow = 1), widths = c(0.3, 0.3, 0.3, 0.1))

    # plot original image
    plotImageArray(init_fit$original_img, main = "original")

    # plot initial fit
    plotImageArray(constructImage(init_fit$pixel_assignments,
                                  init_fit$centers), main = "initial fit")

    # plot reclustered fit
    plotImageArray(constructImage(final_fit$pixel_assignments,
                                  final_fit$centers), main = "reclustered fit")

    # and the new color palette
    plotColorPalette(final_fit$centers, sizes = final_fit$sizes, horiz = FALSE)
  }

  # be nice!
  graphics::par(mfrow = current_par$mfrow,
      mar = current_par$mar)

  final_fit <- list(original_img = grDevices::as.raster(final_fit$original_img),
                    pixel_assignments = final_fit$pixel_assignments,
                    sizes = final_fit$sizes,
                    centers = final_fit$centers)

  class(final_fit) <- "recolorize"
  return(final_fit)

}

#' Change colors of dendrogram tips
#'
#' Internal function for [recolorize::recluster] plotting.
#'
#' @param x Leaf of a dendrogram.
#' @param hex_cols Hex color codes for colors to change to.
#' @param pch The type of point to draw.
#' @param cex The size of the point.
labelCol <- function(x, hex_cols, pch = 20, cex = 2) {

  if (length(cex) == 1) {
    cex <- rep(cex, length(hex_cols))
  }

  if (stats::is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label")
    ## set label color
    attr(x, "nodePar") <- list(lab.col = hex_cols[label],
                               col = hex_cols[label],
                               pch = pch, cex = cex[label])
  }
  return(x)
}

