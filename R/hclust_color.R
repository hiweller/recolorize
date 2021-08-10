#' Plot and group colors by similarity
#'
#' A wrapper for [stats::hclust] for clustering colors by similarity.
#' This works by converting a matrix of RGB centers to a given color space
#' (CIE Lab is the default), generating a distance matrix for those colors
#' in that color space (or a subset of channels of that color space),
#' clustering them, and plotting them with labels and colors. If either a
#' cutoff or a final number of colors is provided and `return_list = TRUE`,
#' function also returns a list of which color centers to combine.
#'
#' @param rgb_centers A matrix of RGB centers. Rows are centers and columns
#' are R, G, and B values.
#' @param dist_method Method passed to [stats::dist]. One of  "euclidean",
#'   "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @param hclust_method Method passed to [stats::hclust]. One of "ward.D",
#'   "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#'   "median" (= WPGMC) or "centroid" (= UPGMC).
#' @param channels Numeric: which color channels to use for clustering. Probably
#'   some combination of 1, 2, and 3, e.g., to consider only luminance and
#'   blue-yellow (b-channel) distance in CIE Lab space, `channels = c(1, 3` (L
#'   and b).
#' @param ref_white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight. See
#'   [grDevices::convertColor].
#' @param color_space Color space in which to do the clustering.
#' @param cutoff Either `NULL` or a numeric cutoff passed to [stats::cutree].
#'   Distance below which to combine clusters, i.e. height at which the tree
#'   should be cut.
#' @param n_final Numeric. Desired number of groups. Overrides `cutoff` if
#'   both are provided.
#' @param return_list Logical. Return a list of new group assignments from
#'   the `cutoff` or `n_final` values?
#' @param plotting Logical. Plot a colored dendrogram?
#'
#' @return A list of group assignments (i.e. which centers belong to which
#'   groups), if `return_list = TRUE`.
#'
#' @details This is mostly useful in deciding where and in which color space
#'   to place a cutoff for a `recolorize` object, since it is very fast. It
#'   is called by [recluster] when combining layers by similarity.
#'
#' @seealso [recluster]
#'
#' @examples
#'
#' # 50 random RGB colors
#' rgb_random <- matrix(runif(150), nrow = 50, ncol = 3)
#'
#' # default clustering (Lab space):
#' hclust_color(rgb_random, return_list = FALSE)
#'
#' # clustering in RGB space (note change in Y-axis scale):
#' hclust_color(rgb_random, color_space = "sRGB", return_list = FALSE)
#'
#' # clustering using only luminance:
#' hclust_color(rgb_random, channels = 1, return_list = FALSE)
#'
#' # or only red-green ('a' channel):
#' hclust_color(rgb_random, channels = 2, return_list = FALSE)
#'
#' # or only blue-yellow ('b' channel(:
#' hclust_color(rgb_random, channels = 3, return_list = FALSE)
#'
#' # use a cutoff to get groups:
#' groups <- hclust_color(rgb_random, cutoff = 100)
#' print(groups)
#'
#' @export
hclust_color <- function(rgb_centers,
                         dist_method = "euclidean",
                         hclust_method = "complete",
                         channels = 1:3,
                         color_space = "Lab",
                         ref_white = "D65",
                         cutoff = NULL,
                         n_final = NULL,
                         return_list = TRUE,
                         plotting = TRUE) {

  # convert to hex colors (for plotting) and specified color space (for
  # distances)
  hex_cols <- grDevices::rgb(rgb_centers)
  conv_cols <- col2col(rgb_centers,
                       from = "sRGB",
                       to = color_space,
                       ref_white = ref_white)

  # get distance matrix
  d <- stats::dist(conv_cols[ , channels], method = dist_method)

  # get hierarchical clustering
  hc <- stats::hclust(d, method = hclust_method)

  # convert to dendrogram
  hcd <- stats::as.dendrogram(hc)

  # set colors
  hcd <- stats::dendrapply(hcd, function(x) labelCol(x, hex_cols, cex = 3))

  if (plotting) {
    # plot
    graphics::par(mar = c(3, 4, 0, 0))
    plot(hcd, xlab = "", ylab = paste(color_space, "color distance"))

    # plot cutoff value if provided:
    if (!is.null(cutoff)) {
      graphics::abline(h = cutoff, lty = 2, col = "red", lwd = 2)
    }
  }

  # get list of layers to merge
  if (return_list) {
    if (is.null(cutoff)) { cutoff <- 0 }
    clust_groups <- stats::cutree(hc, h = cutoff, k = n_final)
    merge_list <- lapply(unique(clust_groups),
                         function(i) which(clust_groups == i))
    return(merge_list)
  }

}
