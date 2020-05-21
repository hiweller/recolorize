# plots a recolored image + its color palette
plotRecolorized <- function(recolored.img, original.img,
                            plot.original = TRUE,
                            color.centers,
                            horiz = FALSE,
                            cex.text = 2, sizes = NULL) {

  # for resetting
  user.par <- par(no.readonly = TRUE)

  # layout
  if (plot.original) {

    if(horiz) {
      layout(matrix(c(1, 2, 3), 1, 3),
             widths = c(0.42, 0.42, 0.16))
      h <- FALSE
    } else {
      layout(matrix(c(1, 2, 3), 3, 1),
             heights = c(0.42, 0.42, 0.16))
      h <- TRUE
    }

  } else {

    if (horiz) {
      layout(matrix(c(2, 1), 1, 2),
             widths = c(0.8, 0.2))
      h <- FALSE
    } else {
      layout(matrix(c(1, 2), 2, 1),
             heights = c(0.8, 0.2))
      h <- TRUE
    }

  }

  # plot original if specified
  if (plot.original) {
    par(mar = rep(0.5, 4))
    plotImageArray(original.img)
  }

  # plotting image
  par(mar = rep(0.5, 4))
  plotImageArray(recolored.img)

  # plotting palette
  par(mar = rep(0.5, 4))
  plotColorPalette(color.centers, horiz = h,
                   cex.text = cex.text, sizes = sizes)

  # reset parameters
  par(user.par)

}
