# plots a recolored image + its color palette
plotRecolorized <- function(recolored.img, color.centers, horiz = FALSE,
                            cex.text = 2) {

  # for resetting
  user.par <- par(no.readonly = TRUE)

  # layout
  if (horiz) {
    layout(matrix(c(2, 1), 1, 2),
           widths = c(0.2, 0.8))
  } else {
    layout(matrix(c(1, 2), 2, 1),
           heights = c(0.8, 0.2))
  }

  # plotting image
  par(mar = rep(0.5, 4))
  plotImageArray(recolored.img)

  # plotting palette
  par(mar = rep(0.5, 4))
  plotColorPalette(color.centers, horiz = horiz, cex.text = cex.text)

  # reset parameters
  par(user.par)

}
