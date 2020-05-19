# fxn 3: non-bg pixels in -> centers & assignments
# 4. find centers -- either thru histogram or kmeans
colorClusters <- function(pixel.matrix,
                          method = "kmeans", n = 10,
                          bins = 3) {

  # coerce method argument
  method <- match.arg(tolower(method), c("kmeans", "histogram"))

  # we don't care about the alpha channel; remove if present
  pixel.matrix <- pixel.matrix[ , 1:3]

  # use clustering function appropriate to specified method
  if (method == "kmeans") {

    color.clusters <- colorClustersKMeans(pixel.matrix = pixel.matrix,
                                          n = n)

  } else if (method == "histogram") {

    color.clusters <- colorClustersHist(pixel.matrix = pixel.matrix,
                                        bins = bins)

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

# clustering with kmeans
colorClustersKMeans <- function(pixel.matrix, n = 10) {

  # start with 10 iterations and try clustering
  iter.max <- 10L

  img.k <- stats::kmeans(pixel.matrix, n)

  # if that doesn't converge, up the number of iterations
  while (img.k$ifault == 4) {

    # up the number of iterations
    iter.max <- iter.max + 10L
    img.k <- stats::kmeans(pixel.matrix, n, iter.max = iter.max)

    # but let's not get ridiculous
    if (iter.max > 100) { break }

  }

  # return
  return(list(pixel.assignments = img.k$cluster,
              centers = img.k$centers))

}

# clustering with histograms
colorClustersHist <- function(pixel.matrix, bins = 3) {

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

  # how many pixels in each bin?
  d <- mgcv::uniquecombs(binned.image)

  # which bin does each pixel go in?
  pixel.assignments <- attr(d, "index")

  # for every color center...
  for (j in 1:dim(d)[1]) {

    # extract all the pixels in that bin
    pix.temp <- pixel.matrix[which(pixel.assignments == j), ]

    # if more than one pixel, use the average
    if (is.matrix(pix.temp)) {
      centers[j, 1:3] <- colMeans(pix.temp)
    } else {
      centers[j, 1:3] <- pix.temp
    }
  }

  # return pixel assignments and centers
  return(list(pixel.assignments = pixel.assignments,
              centers = centers))

}
