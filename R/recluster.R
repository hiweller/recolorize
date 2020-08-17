# recluster: takes a recolorize object,
# then clusters those colors by similarity based on
# 1. a similarity cutoff (in Lab space, range is ~0-140), or
# 2. an end number of colors
recluster <- function(recolorize.obj,
                       similarity_cutoff = 60,
                       n_final = NULL,
                       plot_hclust = FALSE,
                      plot_final = TRUE) {

  # courtesy:
  current_par <- par()

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
  lab_init <- convertColor(centers, "sRGB", "Lab")

  # get distance matrix
  d <- dist(lab_init)

  # perform clustering
  hc <- hclust(d)

  # plot clustering:
  if (plot_hclust) {
    par(mfrow = c(1, 1), mar = c(1, 3, 3, 1))
    plot(hc, xlab = "")
  }

  # plot cutoff value if provided:
  if (is.numeric(similarity_cutoff)) {
    abline(h = similarity_cutoff, lty = 2, col = "red")
  }

  # form groups
  clust_groups <- cutree(hc, k = n_final,
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
                       function(j) weighted.mean(j, group_sizes))
    } else {
      # otherwise, just copy it
      lab_avg <- group_colors
    }

    # get their RGB color
    rgb_avg <- convertColor(lab_avg, "Lab", "sRGB")

    if (i == 1) {

      final_clusters <- rgb_avg
      final_sizes <- sum(group_sizes)

    } else {

      final_clusters <- rbind(final_clusters, rgb_avg)
      final_sizes <- c(final_sizes, sum(group_sizes))

    }
  }

  # fit to our reclustered colors!
  final_fit <- imposeColors(init_fit$recolored.img, final_clusters,
                            adjust.centers = FALSE,
                            plotting = FALSE)

  # if plotting...
  if (plot_final) {

    # first, set nice margins and layout
    par(mar = rep(0, 4))
    layout(matrix(1:4, nrow = 1), widths = c(0.3, 0.3, 0.3, 0.1))

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
  par(mfrow = current_par$mfrow,
      mar = current_par$mar)

  return(final_fit)

}
