# ex:
# img <- jpeg::readJPEG("cow.jpg")
# imgRecolor <- kmeansRecolor(img, 10)
kmeansRecolor <- function(img.array, n = 10,
                          rounding = 2,
                          plotting = TRUE,
                          lower = NULL, upper = NULL,
                          center = NULL, radius = NULL,
                          transparent = TRUE,
                          return.original = TRUE) {

  # copy original image for recoloring
  img.recolor <- img.array
  img.dim <- dim(img.recolor)

  # reshape it for faster indexing
  dim(img.recolor) <- c(img.dim[1] * img.dim[2], img.dim[3])

  # assign color clusters for each pixel
  img.k <- stats::kmeans(img.recolor, n)

  # rounding - a bit faster if turned on
  if (is.numeric(rounding)) {
    centers <- round(img.k$centers, digits = rounding)
  } else {
    centers <- img.k$centers
  }

  # change those colors
  for (i in 1:nrow(centers)) {
    new.color <- centers[i, ]
    pix.idx <- which(img.k$cluster == i)
    replacements <- matrix(new.color, ncol = 3, byrow = TRUE,
                           nrow = length(pix.idx))
    img.recolor[pix.idx, ] <- replacements
  }

  # reshape
  dim(img.recolor) <- dim(img.array)

  # get cluster sizes
  ktable <- img.k$size / sum(img.k$size)
  centers <- setNames(cbind(img.k$centers, ktable),
                      c("r", "g", "b", "size"))

  # return recolored image + centers at a minimum
  return.list <- list(centers = centers,
                      img.recolor = img.recolor)

  # return original if requested
  if (return.original == TRUE) {
    return.list$img.original <- img.array
  }

  # and plot
  if (plotting) {

    # get current row/column layout for resetting
    current.par <- par()$mfrow

    # set layout
    par(mfrow = c(1, 2))

    # plot images
    main <- paste(n, if (n == 1) { "cluster" } else { "clusters" })
    countcolors::plotArrayAsImage(img.array, main = "input")
    countcolors::plotArrayAsImage(img.recolor, main)

    # reset layout
    par(mfrow = current.par)
  }

  return(return.list)

}
