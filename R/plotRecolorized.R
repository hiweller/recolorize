# plots a recolored image + its color palette
plotRecolorized <- function(recolored.img, original.img,
                            plot.original = TRUE,
                            color.centers,
                            horiz = FALSE,
                            cex.text = 2, sizes = NULL) {

  # for resetting
  user.par <- graphics::par(no.readonly = TRUE)

  # layout
  if (plot.original) {

    if(horiz) {
      graphics::layout(matrix(c(1, 2, 3), 1, 3),
                       widths = c(0.42, 0.42, 0.16))
      h <- FALSE
    } else {
      graphics::layout(matrix(c(1, 2, 3), 3, 1),
                       heights = c(0.42, 0.42, 0.16))
      h <- TRUE
    }

  } else {

    if (horiz) {
      graphics::layout(matrix(c(2, 1), 1, 2),
             widths = c(0.8, 0.2))
      h <- FALSE
    } else {
      graphics::layout(matrix(c(1, 2), 2, 1),
             heights = c(0.8, 0.2))
      h <- TRUE
    }

  }

  # plot original if specified
  if (plot.original) {
    graphics::par(mar = rep(0.5, 4))
    plotImageArray(original.img)
  }

  # plotting image
  graphics::par(mar = rep(0.5, 4))
  plotImageArray(recolored.img)

  # plotting palette
  graphics::par(mar = rep(0.5, 4))
  plotColorPalette(color.centers, horiz = h,
                   cex.text = cex.text, sizes = sizes)

  # reset parameters
  graphics::par(user.par)

}
