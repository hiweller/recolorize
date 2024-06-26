#' Generate color clusters from an image
#'
#' Clusters all the pixels in an image according to the specified method and
#' returns color centers, cluster assignments, and cluster sizes.
#'
#' @param bg_indexed A list returned by [backgroundIndex()].
#' @param method Binning scheme to use, one of either `kmeans` or `histogram`.
#'   Produce very different results (see details).
#' @param n If `method = "kmeans"`, the number of colors to fit.
#' @param bins If `method = "histogram"`, either the number of bins per color
#'   channel (if a single number is provided) OR a vector of length 3 with the
#'   number of bins for each channel.
#' @param color_space Color space in which to cluster colors, passed to
#'   \code{[grDevices]{convertColor}}. One of "sRGB", "Lab", or "Luv".
#'   Default is "Lab", a perceptually uniform (for humans) color space.
#' @param ref_white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight.
#' @param bin_avg Logical. Return the color centers as the average of the pixels
#'   assigned to the bin (the default), or the geometric center of the bin?
#'
#'
#' @return
#' A list with the following elements:
#' \enumerate{
#'         \item `pixel_assignments`: A vector of color center assignments for
#'         each pixel.
#'         \item `centers`: A matrix of color centers, in RGB color space.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details
#' [stats::kmeans()] clustering tries to find the set of `n` clusters
#' that minimize overall distances. Histogram binning divides up color space
#' according to set breaks; for example, bins = 2 would divide the red, green,
#' and blue channels into 2 bins each (> 0.5 and < 0 .5), resulting in 8
#' possible ranges. A white pixel (RGB = 1, 1, 1) would fall into the R \> 0.5, G
#' \> 0.5, B \> 0.5 bin. The resulting centers represent the average color of all
#' the pixels assigned to that bin.
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
#' @examples
#'
#' # make a 100x100 'image' of random colors
#' img <- array(runif(30000), dim = c(100, 100, 3))
#' plotImageArray(img)
#'
#' # make a background index object:
#' bg_indexed <- backgroundIndex(img, backgroundCondition())
#'
#' # histogram clustering
#' hist_clusters <- colorClusters(bg_indexed, method = "hist", bins = 2)
#' plotColorPalette(hist_clusters$centers)
#'
#' # we can use a different number of bins for each channel
#' uneven_clusters <- colorClusters(bg_indexed, method = "hist",
#'                                  bins = c(3, 2, 1))
#' plotColorPalette(uneven_clusters$centers)
#'
#' # using kmeans
#' kmeans_clusters <- colorClusters(bg_indexed, method = "kmeans",
#'                                  n = 5)
#' plotColorPalette(kmeans_clusters$centers)
#'
#' @export
colorClusters <- function(bg_indexed,
                          method = c("histogram", "kmeans"),
                          n = 10,
                          bins = 3,
                          color_space = "Lab",
                          ref_white = "D65",
                          bin_avg = TRUE) {

  # coerce method argument
  method <- match.arg(method)

  # use clustering function appropriate to specified method
  if (method == "kmeans") {

    color_clusters <- colorClustersKMeans(pixel_matrix = bg_indexed$non_bg,
                                          n = n,
                                          color_space = color_space,
                                          ref_white = ref_white)
    color_clusters$method <- "kmeans"

  } else if (method == "histogram") {

    color_clusters <- colorClustersHist(pixel_matrix = bg_indexed$non_bg,
                                        bins = bins,
                                        color_space = color_space,
                                        ref_white = ref_white,
                                        bin_avg = bin_avg)
    color_clusters$method <- "histogram"

  } else {

    stop("uh oh!!")

  }

  # convert to a full matrix
  sizes <- color_clusters$sizes
  color_clusters <- pixelAssignMatrix(bg_indexed, color_clusters)
  color_clusters$sizes <- sizes

  # note: returned centers should ALWAYS be RGB 0-1 range
  # this is because they're used for recoloring, not quantifying
  # however, clustering and pixel assignment can be done in other
  # color spaces
  class(color_clusters) <- "color_clusters"
  return(color_clusters)

}

#' Cluster pixel colors using K-means clustering
#'
#' Clusters pixel colors using [stats::kmeans()].
#'
#' @param pixel_matrix 2D matrix of pixels to classify (rows = pixels, columns =
#'   channels).
#' @param n Number of clusters to fit.
#' @param color_space Color space in which to cluster colors, passed to
#'   \code{[grDevices]{convertColor}}. One of "sRGB", "Lab", "Luv", or
#'   "XYZ". Default is "Lab", a perceptually uniform (for humans) color space.
#' @param ref_white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight.
#'
#' @return
#' A list with the following elements:
#' \enumerate{
#'         \item `pixel_assignments`: A vector of color center assignments for each pixel.
#'         \item `centers`: A matrix of color centers.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details Called by [colorClusters()]. See that documentation for
#'   examples.
colorClustersKMeans <- function(pixel_matrix, n = 10,
                                color_space = "Lab",
                                ref_white = "D65") {

  # start with 20 iterations and try clustering
  iter_max <- 20L

  # first, convert to color space for clustering:
  pm <- col2col(pixel_matrix,
                from = "sRGB",
                to = color_space,
                ref_white = ref_white)

  img_k <- stats::kmeans(pm, n, iter.max = iter_max)

  # if that doesn't converge, up the number of iterations
  # while (img_k$ifault == 4) {
  #
  #   # up the number of iterations
  #   iter_max <- iter_max + 10L
  #   img_k <- stats::kmeans(pixel_matrix, n, iter.max = iter_max)
  #
  #   # but let's not get ridiculous
  #   if (iter_max > 100) { break }
  #
  # }

  # convert color centers back to RGB space
  centers <- col2col(img_k$centers,
                     from = color_space,
                     to = "sRGB",
                     ref_white = ref_white)

  # return
  return(list(pixel_assignments = img_k$cluster,
              centers = centers,
              sizes = img_k$size))

}


#' Cluster pixel colors using histogram binning
#'
#' Clusters pixel colors by dividing color space up into specified bins,
#' then taking the average color of all the pixels within that bin.
#'
#' @param pixel_matrix 2D matrix of pixels to classify (rows = pixels, columns =
#'   channels).
#' @param bins Number of bins for each channel OR a vector of length 3 with bins
#'   for each channel. `bins = 3` will result in 3^3 = 27 bins; `bins = c(2, 2, 3)`
#'   will result in 2*2*3 = 12 bins (2 red, 2 green, 3 blue if you're in RGB
#'   color space), etc.
#' @param color_space Color space in which to cluster colors, passed to
#'   \code{[grDevices]{convertColor}}. One of "sRGB", "Lab", or "Luv".
#'   Default is "Lab", a perceptually uniform (for humans) color space.
#' @param ref_white Reference white for converting to different color spaces.
#'   D65 (the default) corresponds to standard daylight.
#' @param bin_avg Logical. Return the color centers as the average of the pixels
#'   assigned to the bin (the default), or the geometric center of the bin?
#'
#' @return
#' A list with the following elements:
#' \enumerate{
#'         \item `pixel_assignments`: A vector of color center assignments for
#'         each pixel.
#'         \item `centers`: A matrix of color centers.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details Called by [colorClusters()]. See that documentation for
#'   examples.
colorClustersHist <- function(pixel_matrix,
                              bins = 3,
                              color_space = c("Lab", "sRGB", "Luv", "HSV"),
                              ref_white = "D65",
                              bin_avg = TRUE) {

  # make sure bins is either a number or a vector of length 3
  stopifnot(length(bins) == 1 | 3)

  # match argument for color space
  color_space <- match.arg(color_space)

  # first, convert to color space for clustering:
  pm <- col2col(pixel_matrix,
                from = "sRGB",
                to = color_space,
                ref_white = ref_white)

  # format bins
  if (length(bins) == 1) {
    message(paste("\nUsing ", bins, "^3 = ", paste(bins^3),
                  " total bins", sep = ""))
    bins <- rep(bins, 3)
  } else {
    message(paste("\nUsing ", bins[1], "*", bins[2],
                  "*", bins[3], " = ", bins[1] * bins[2] * bins[3],
                  " bins", sep = ""))
  }

  # color space ranges
 if (color_space == "Lab") {

    # Lab is 0-100 (L), -127-127 (a and b)
    # HOWEVER, these extremes are virtually unoccupied by RGB colors
    # these are a little outside the range of sRGB in Lab space:
    brange <- list(c(0, 100),
                   c(-90, 100),
                   c(-110, 95))

  }  else if (color_space == "Luv") {

    # please don't ever ask me about this
    brange <- list(c(0, 100),
                   c(-85, 175),
                   c(-135, 107))

  } else {

    #sRGB/HSV range is 0-1 in all channels
    brange <- list(c(0, 1),
                   c(0, 1),
                   c(0, 1))

  }

  # from bins, generate breaks/ranges
  breaks <- lapply(1:3, function(x) seq(brange[[x]][1],
                                        brange[[x]][2],
                                        length = bins[x] + 1))

  # bin the image?
  binned_image <- data.frame(c1 = cut(pm[, 1], breaks = breaks[[1]],
                                     include.lowest = T, labels = F),
                             c2 = cut(pm[, 2], breaks = breaks[[2]],
                                     include.lowest = T, labels = F),
                             c3 = cut(pm[, 3], breaks = breaks[[3]],
                                     include.lowest = T, labels = F))

  # possible bins!
  possible_bins <- expand.grid(c(1:bins[1]),
                               c(1:bins[2]),
                               c(1:bins[3]))

  # defaults = centers of bins
  break_means <- lapply(breaks, function(i) sapply(2:length(i),
                                function(m) mean(c(i[m-1], i[m]))))
  centers <- as.matrix(expand.grid(break_means))
  colnames(centers) <- strsplit(gsub("s", "", color_space), "")[[1]]

  # how many pixels in each bin?
  d <- mgcv::uniquecombs(binned_image, ordered = TRUE)

  # which bin does each pixel go in?
  pixel_assignments <- attr(d, "index")
  pixel_assignments_2 <- rep(0, length(pixel_assignments))

  # make a vector for sizes
  sizes <- rep(0, prod(bins))

  # for matching
  bin_names <- apply(possible_bins, 1,
                     \(x) paste0(x, collapse = ""))

  # for every color center...
  for (j in 1:dim(d)[1]) {

    # match to possible_bins bin
    bin_idx <- match(paste0(d[j, ], collapse = ""),
          bin_names)

    # extract all the pixels in that bin
    pix_temp <- pm[which(pixel_assignments == j), ]

    pixel_assignments_2[which(pixel_assignments == j)] <- bin_idx

    # if more than one pixel, use the average
    if (is.matrix(pix_temp)) {
      if (bin_avg) { centers[bin_idx, 1:3] <- colMeans(pix_temp) }
      sizes[bin_idx] <- nrow(pix_temp)
    } else {
      if (bin_avg) { centers[bin_idx, 1:3] <- pix_temp }
      sizes[bin_idx] <- 1
    }

  }

  # convert centers
  centers <- col2col(centers,
                     from = color_space,
                     to = "sRGB",
                     ref_white = ref_white)

  # return pixel assignments and centers
  return(list(pixel_assignments = pixel_assignments_2,
              centers = centers,
              sizes = sizes))

}

#' Modified convertColor
#'
#' Just like [grDevices::convertColor], but with HSV as an option.
#'
#' @param pixel_matrix A matrix of pixel colors, rows are pixels and columns
#' are channels.
#' @param from Color space to convert from.
#' @param to Color space to convert to.
#' @param ref_white Reference white.
#'
#' @return A pixel matrix in the specified `to` color space.
#'
#' @details As my mother used to say: good enough for government work.
col2col <- function(pixel_matrix,
                               from = c("sRGB", "Lab", "Luv", "HSV"),
                               to = c("sRGB", "Lab", "Luv", "HSV"),
                            ref_white = "D65") {

  # match color space args
  from_color_space <- match.arg(from)
  to_color_space <- match.arg(to)

  # if HSV is not in site, we can use convertColor
  if (from_color_space != "HSV" & to_color_space != "HSV") {

    # ok, first convert pixels
    pm <- grDevices::convertColor(pixel_matrix,
                                  from = from_color_space,
                                  to = to_color_space,
                                  to.ref.white = ref_white,
                                  from.ref.white = ref_white)

  } else if (from_color_space == "sRGB" & to_color_space == "HSV") {

    # if we're converting from RGB to HSV, we can use rgb2hsv:
    pm <- t(grDevices::rgb2hsv(t(pixel_matrix), maxColorValue = 1))

  } else if (from_color_space == "HSV") {

    # if we're converting from HSV, first convert to RGB
    pm_temp <- grDevices::hsv(pixel_matrix[ , 1],
                              pixel_matrix[ , 2],
                              pixel_matrix[ , 3])

    pm_rgb <- t(grDevices::col2rgb(pm_temp)) / 255

    # then proceed as usual
    pm <- grDevices::convertColor(pm_rgb,
                                  from = "sRGB",
                                  to = to_color_space,
                                  to.ref.white = ref_white)
  }

  return(pm)

}
