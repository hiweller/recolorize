# Assigns a 2d matrix of pixels to a provided set of clusters
# input:
# 1. 2d pixel matrix (rows = px and cols = channels)
# 2. clusters (rows = centers and cols = channels)
# output:
# a vector of length = nrow(pixels) of the cluster to which that pixel was assigned
# kmeans does this automatically!
# returns a list with pixel.assignments, color.centers, and sizes
# which can be passed to recolorImage
assignPixels <- function(color.centers, pixel.matrix, adjust.centers = TRUE) {

  # I'm not sure this is really as fast as it could be
  tmp <- sapply(1:nrow(pixel.matrix),
                  function(i) apply(color.centers, 1,
                                    function(v) sum((pixel.matrix[i, ]-v)^2)))

  # make returnables
  pixel.assignments <- max.col(-t(tmp))  # find index of min distance
  sizes <- table(pixel.assignments) # empty clusters?

  # if specified: make new color centers based on average of assigned pixels
  if (adjust.centers) {

    for (i in 1:nrow(color.centers)) {

      pixel.idx <- which(pixel.assignments == i)

      if (length(pixel.idx) == 0) { next } else {
        color.centers[i, ] <- colMeans(pixel.matrix[pixel.idx, ])
      }

    }

  }

  color.clusters <- list(pixel.assignments = pixel.assignments,
                         centers = color.centers,
                         sizes = sizes)
  class(color.clusters) <- "color.clusters"
  return(color.clusters)

}
