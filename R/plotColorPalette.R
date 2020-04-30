# takes and n x 3 matrix of color centers and plots a little palette with numbers
plotColorPalette <- function(color.centers,
                             cex.text = 2, horiz = TRUE, ...) {

  # make color vector
  rgb.exp <- rgb(color.centers[, 1],
                 color.centers[, 2],
                 color.centers[, 3])

  # make a plot
  colorbar <- rep(1, nrow(color.centers))
  setNames(colorbar, as.character(1:nrow(color.centers)))

  # plot the colors as a uniform bar
  barplot(colorbar, col = rgb.exp,
          axes = FALSE, space = 0, horiz = horiz,
          border = NA, axisnames = FALSE,
          ...)

  # text colors - black if the color is light, white if the color is dark
  hsv.exp <- rgb2hsv(t(color.clusters$centers), maxColorValue = 1)
  text.colors <- round(hsv.exp[3, ]) + 1

  if (horiz == FALSE) {
    text.x <- seq(0.5, length(rgb.exp) - 0.5)
    text.y <- 0.5
  } else {
    text.y <- seq(0.5, length(rgb.exp) - 0.5)
    text.x <- 0.5
  }

  text(text.x, text.y,
       cex = cex.text, col = c("white", "black")[text.colors])

}
