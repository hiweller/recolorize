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
#' @param recolorize.obj A recolorize object from \code{\link{recolorize}},
#'   \code{\link{recluster}}, or \code{\link{imposeColors}}.
#' @param similarity_cutoff Numeric similarity cutoff for grouping color centers
#'   together. The range is in absolute Euclidean distance in CIE Lab space,
#'   which means it is greater than 0-100, but cutoff values between 20 and 80
#'   will usually work best. See details.
#' @param n_final Final number of desired colors; alternative to specifying
#'  a similarity cutoff. Overrides `similarity_cutoff` if provided.
#' @param plot_hclust Logical. Plot the hierarchical clustering tree for
#'  color similarity? Helpful for troubleshooting a cutoff.
#' @param resid Logical. Get final color fit residuals with
#'   \code{\link{colorResiduals}}?
#' @param plot_final Logical. Plot the final color fit?
#' @param adjust_centers Logical. Once new color centers are determined and
#'   fit to the image, adjust their centers to the average value of all
#'   the pixels assigned to it? Unlike in \code{\link{imposeColors}},
#'   this often has little effect.
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
#' recluster.obj <- recluster(recolored_corbetti,
#'                            similarity_cutoff = 60,
#'                            plot_hclust = TRUE)
#'
#' # compare to the result using k-means clustering and the same n:
#' kmeans.fit <- recolorize(corbetti, "k", n = 6)
#' # (you also get different clusters every time you run this)
#'
#' # you get slightly different results if you prevent the center adjustment:
#' recluster.obj <- recluster(recolored_corbetti,
#'                            similarity_cutoff = 60,
#'                            adjust_centers = FALSE)
#'
#' # a cutoff that's too severe will usually just produce "light",
#' # "dark", and "other" colors:
#' recluster.obj <- recluster(recolored_corbetti,
#'                            similarity_cutoff = 100,
#'                            plot_hclust = TRUE)
#'
#' # we get the same result by specifying n_final = 6
#' recluster.obj <- recluster(recolored_corbetti,
#'                            n_final = 6,
#'                            plot_hclust = TRUE)
#'
#' @export
#'
recluster <- function(recolorize.obj,
                       similarity_cutoff = 60,
                       n_final = NULL,
                       plot_hclust = FALSE,
                      resid = FALSE,
                      plot_final = TRUE,
                      adjust_centers = TRUE) {

  # courtesy:
  current_par <- graphics::par()

  # rename, to keep things clear
  init_fit <- recolorize.obj

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
  lab_init <- grDevices::convertColor(centers, "sRGB", "Lab")

  # get distance matrix
  d <- stats::dist(lab_init)

  # perform clustering
  # hc <- hclust(d / max(d))
  hc <- stats::hclust(d)

  # plot clustering:
  if (plot_hclust) {

    graphics::par(mfrow = c(1, 1), mar = c(1, 3, 3, 1))
    plot(hc, xlab = "")

    # plot cutoff value if provided:
    if (is.null(n_final)) {
      graphics::abline(h = similarity_cutoff, lty = 2, col = "red")
    }

  }

  # form groups
  clust_groups <- stats::cutree(hc, k = n_final,
                         h = similarity_cutoff)

  # for each group...
  for (i in unique(clust_groups)) {

    # find all original clusters assigned to it
    group_idx <- which(clust_groups == i)

    # get the colors
    group_colors <- lab_init[group_idx, ]

    # get their sizes
    group_sizes <- init_fit$sizes[group_idx]

    if (all(group_sizes == 0)) {
      next
    }

    # if there's more than one color in the group
    if (length(group_idx) > 1) {
      # get their weighted average
      lab_avg <- apply(group_colors, 2,
                       function(j) stats::weighted.mean(j, group_sizes))
    } else {
      # otherwise, just copy it
      lab_avg <- group_colors
    }

    # get their RGB color
    rgb_avg <- grDevices::convertColor(lab_avg, "Lab", "sRGB")

    if (i == 1) {

      final_clusters <- rgb_avg
      final_sizes <- sum(group_sizes)

    } else {

      final_clusters <- rbind(final_clusters, rgb_avg)
      final_sizes <- c(final_sizes, sum(group_sizes))

    }
  }

  # fit to our reclustered colors!
  final_fit <- imposeColors(init_fit$original.img,
                            final_clusters, resid = resid,
                            adjust.centers = adjust_centers,
                            plotting = FALSE)

  # if plotting...
  if (plot_final) {

    # first, set nice margins and layout
    graphics::par(mar = rep(0, 4))
    graphics::layout(matrix(1:4, nrow = 1), widths = c(0.3, 0.3, 0.3, 0.1))

    # plot original image
    plotImageArray(init_fit$original.img, main = "original")

    # plot initial fit
    plotImageArray(init_fit$recolored.img, main = "initial fit")

    # plot reclustered fit
    plotImageArray(final_fit$recolored.img, main = "reclustered fit")

    # and the new color palette
    plotColorPalette(final_fit$centers, sizes = final_fit$sizes, horiz = FALSE)
  }

  # be nice!
  graphics::par(mfrow = current_par$mfrow,
      mar = current_par$mar)

  return(final_fit)

}