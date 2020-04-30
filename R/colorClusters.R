# fxn 3: non-bg pixels in -> centers & assignments
# 4. find centers -- either thru histogram or kmeans
colorClusters <- function(pixel.matrix, method = "kmeans", n = 10) {

  # coerce method argument
  method <- match.arg(tolower(method), c("kmeans", "histogram"))

  # we don't care about the alpha channel; remove if present
  pixel.matrix <- pixel.matrix[ , 1:3]

  # histogram method to come...
  if (method == "kmeans") {

    img.k <- stats::kmeans(pixel.matrix, n)

  } else if (method == "histogram") {

    stop("Histogram binning coming soon...")

  } else {

    stop("uh oh")

  }

  # note: returned centers should ALWAYS be RGB 0-1 range
  # this is because they're used for recoloring, not quantifying
  # however, clustering and pixel assignment can be done in other
  # color spaces
  color.clusters <- list(centers = img.k$centers,
                         pixel.assignments = img.k$cluster)
  class(color.clusters) <- "color.clusters"
  return(color.clusters)

}
