#' Generate color clusters from an image
#'
#' Clusters all the pixels in an image according to the specified method and
#' returns color centers, cluster assignments, and cluster sizes.
#'
#' @param pixel.matrix 2D matrix of pixels to classify (rows = pixels, columns = channels).
#' @param method Binning scheme to use, one of either `kmeans` or `histogram`.
#'   Produce very different results (see details).
#' @param n If `method = "kmeans"`, the number of colors to fit.
#' @param bins If `method = "histogram"`, either the number of bins per color
#'   channel (if a single number is provided) OR a vector of length 3 with the
#'   number of bins for each channel.
#'
#' @return
#' A list with the following elements:
#' \enumerate{
#'         \item `pixel.assignments`: A vector of color center assignments for each pixel.
#'         \item `centers`: A matrix of color centers.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details
#' \code{\link[stats]{kmeans}} clustering tries to find the set of `n` clusters
#' that minimize overall distances. Histogram binning divides up color space
#' according to set breaks; for example, bins = 2 would divide the red, green,
#' and blue channels into 2 bins each (> 0.5 and < 0 .5), resulting in 8 possible
#' ranges. A white pixel (RGB = 1, 1, 1) would fall into the R > 0.5, G > 0.5, B > 0.5 bin.
#' The resulting centers represent the average color of all the pixels assigned to that bin.
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
#' # pixel matrix
#' img.2d <- img
#' dim(img.2d) <- c(100 * 100, 3)
#'
#' # histogram clustering
#' hist.clusters <- colorClusters(img.2d, method = "hist", bins = 2)
#' plotColorPalette(hist.clusters$centers)
#'
#' # we can use a different number of bins for each channel
#' uneven.clusters <- colorClusters(img.2d, method = "hist",
#'                                  bins = c(3, 2, 1))
#' plotColorPalette(uneven.clusters$centers)
#'
#' # using kmeans
#' kmeans.clusters <- colorClusters(img.2d, method = "kmeans",
#'                                  n = 5)
#' plotColorPalette(kmeans.clusters$centers)
#'
#' @export
colorClusters <- function(pixel.matrix,
                          method = "histogram", n = 10,
                          bins = 3) {

  # coerce method argument
  method <- match.arg(tolower(method), c("kmeans", "histogram"))

  # we don't care about the alpha channel; remove if present
  pixel.matrix <- pixel.matrix[ , 1:3]

  # use clustering function appropriate to specified method
  if (method == "kmeans") {

    color.clusters <- colorClustersKMeans(pixel.matrix = pixel.matrix,
                                          n = n)
    color.clusters$method <- "kmeans"

  } else if (method == "histogram") {

    color.clusters <- colorClustersHist(pixel.matrix = pixel.matrix,
                                        bins = bins)
    color.clusters$method <- "histogram"

  } else {

    stop("uh oh!!")

  }

  # note: returned centers should ALWAYS be RGB 0-1 range
  # this is because they're used for recoloring, not quantifying
  # however, clustering and pixel assignment can be done in other
  # color spaces
  # eventually
  class(color.clusters) <- "color.clusters"
  return(color.clusters)

}

#' Cluster pixel colors using K-means clustering
#'
#' Clusters pixel colors using \code{\link[stats]{kmeans}}.
#'
#' @param pixel.matrix 2D matrix of pixels to classify (rows = pixels, columns =
#'   channels).
#' @param n Number of clusters to fit.
#'
#' @return
#' A list with the following elements:
#' \enumerate{
#'         \item `pixel.assignments`: A vector of color center assignments for each pixel.
#'         \item `centers`: A matrix of color centers.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details Called by \code{\link{colorClusters}}. See that documentation for
#'   examples.
colorClustersKMeans <- function(pixel.matrix, n = 10) {

  # start with 10 iterations and try clustering
  iter.max <- 10L

  img.k <- stats::kmeans(pixel.matrix, n, iter.max = iter.max)

  # if that doesn't converge, up the number of iterations
  # while (img.k$ifault == 4) {
  #
  #   # up the number of iterations
  #   iter.max <- iter.max + 10L
  #   img.k <- stats::kmeans(pixel.matrix, n, iter.max = iter.max)
  #
  #   # but let's not get ridiculous
  #   if (iter.max > 100) { break }
  #
  # }

  # return
  return(list(pixel.assignments = img.k$cluster,
              centers = img.k$centers,
              sizes = img.k$size))

}


#' Cluster pixel colors using histogram binning
#'
#' Clusters pixel colors by dividing color space up into specified bins,
#' then taking the average color of all the pixels within that bin.
#'
#' @param pixel.matrix 2D matrix of pixels to classify (rows = pixels, columns =
#'   channels).
#' @param bins Number of bins for each channel OR a vector of length 3 with bins
#'   for each channel. `bins = 3` will result in 3^3 = 27 bins; `bins = c(2, 2, 3)`
#'   will result in 2*2*3 = 12 bins (2 red, 2 green, 3 blue), etc.
#'
#' @return
#' A list with the following elements:
#' \enumerate{
#'         \item `pixel.assignments`: A vector of color center assignments for each pixel.
#'         \item `centers`: A matrix of color centers.
#'         \item `sizes`: The number of pixels assigned to each cluster.
#' }
#'
#' @details Called by \code{\link{colorClusters}}. See that documentation for
#'   examples.
colorClustersHist <- function(pixel.matrix,
                              color.space = "Lab",
                              ref.white = "D65",
                              bins = 3) {

  # make sure bins is either a number or a vector of length 3
  stopifnot(length(bins) == 1 | 3)

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

  # from bins, generate breaks/ranges
  breaks <- lapply(bins + 1, function(x) seq(0, 1, length = x))

  # bin the image?
  binned.image <- data.frame(R = cut(pixel.matrix[, 1], breaks = breaks[[1]],
                                     include.lowest = T, labels = F),
                             G = cut(pixel.matrix[, 2], breaks = breaks[[2]],
                                     include.lowest = T, labels = F),
                             B = cut(pixel.matrix[, 3], breaks = breaks[[3]],
                                     include.lowest = T, labels = F))

  # possible bins!
  possible.bins <- expand.grid(c(1:bins[1]),
                               c(1:bins[2]),
                               c(1:bins[3]))

  # defaults = centers of bins
  break.means <- lapply(breaks, function(i) sapply(2:length(i),
                                function(m) mean(c(i[m-1], i[m]))))
  centers <- as.matrix(expand.grid(break.means))
  colnames(centers) <- c("R", "G", "B")

  # how many pixels in each bin?
  d <- mgcv::uniquecombs(binned.image)

  # which bin does each pixel go in?
  pixel.assignments <- attr(d, "index")

  # make a vector for sizes
  sizes <- rep(0, prod(bins))

  # for every color center...
  for (j in 1:dim(d)[1]) {

    # extract all the pixels in that bin
    pix.temp <- pixel.matrix[which(pixel.assignments == j), ]

    # if more than one pixel, use the average
    if (is.matrix(pix.temp)) {
      centers[j, 1:3] <- colMeans(pix.temp)
      sizes[j] <- nrow(pix.temp)
    } else {
      centers[j, 1:3] <- pix.temp
      sizes[j] <- 1
    }
  }

  # return pixel assignments and centers
  return(list(pixel.assignments = pixel.assignments,
              centers = centers,
              sizes = sizes))

}
