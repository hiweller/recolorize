
recolorImage <- function(bg.indexed, color.clusters,
                         plotting = FALSE, main = "",
                         remove.empty.clusters = FALSE,
                         bg.recolor = "transparent") {

  # just in case...
  if (class(bg.indexed) != "bg.index" |
      class(color.clusters) != "color.clusters") {
    warning("bg.index and color.clusters should be
            the output of backgroundIndex() and colorClusters(),
            respectively")
  }

  # only two options for background recoloring...please...i have looked at
  # enough magenta backgrounds
  bg.recolor <- match.arg(bg.recolor, choices = c("transparent", "white"))

  # copy non-background pixels to change them
  pix.recolor <- bg.indexed$non.bg

  # index for removing empty centers
  core.removal <- c()

  # for every color cluster:
  for (i in 1:nrow(color.clusters$centers)) {

    # get the new color
    new.color <- as.vector(color.clusters$centers[i, ])

    # find which pixels should be changed
    # IMPORTANT: we're assuming that color.clusters$pixel.assignments matches
    # the indices of bg.indexed$non.bg
    pix.idx <- which(color.clusters$pixel.assignments == i)

    # if no pixels were assigned to that cluster, mark it
    if (length(pix.idx) == 0) {
      core.removal <- c(core.removal, i)
      next
    } else {
      # repeat the new color for a substitute matrix
      replacements <- matrix(new.color, ncol = 3, byrow = TRUE,
                             nrow = length(pix.idx))

      # and stick it back in!
      pix.recolor[pix.idx, 1:3] <- replacements

    }
  }

  # slot new pixels back in
  recolored.img <- bg.indexed$flattened.img

  # if there's no background...
  if (length(bg.indexed$idx.flat) == 0) {

    recolored.img <- pix.recolor

  } else {
    recolored.img[-bg.indexed$idx.flat, ] <- pix.recolor
    # color background in white or transparent
    if (bg.recolor == "white") {

      recolored.img[bg.indexed$idx.flat, ] <- 1

    } else if (bg.recolor == "transparent") {

      if (bg.indexed$img.dims[3] == 4) {

        recolored.img[bg.indexed$idx.flat , 4] <- 0

      } else {

        alpha.channel <- rep(1, nrow(recolored.img))
        alpha.channel[bg.indexed$idx.flat] <- 0
        recolored.img <- cbind(recolored.img, alpha.channel)
        colnames(recolored.img) <- NULL
        bg.indexed$img.dims[3] <- 4

      }

    }

  }

  # reshape
  dim(recolored.img) <- bg.indexed$img.dims

  if (plotting) {
    plotImageArray(recolored.img, main = main)
  }

  # make returnables
  if (length(core.removal) > 0 & isTRUE(remove.empty.clusters)) {
    centers <- color.clusters$centers[-core.removal, ]
  } else {
    centers <- color.clusters$centers
  }
  return(list(recolored.img = recolored.img,
              centers = centers))

}
