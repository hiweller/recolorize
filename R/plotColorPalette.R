# takes and n x 3 matrix of color centers and plots a little palette with numbers
plotColorPalette <- function(color.centers, sizes = NULL,
                             cex.text = 2, horiz = TRUE, ...) {

  # make color vector
  rgb.exp <- rgb(color.centers[, 1],
                 color.centers[, 2],
                 color.centers[, 3])

  # make a plot
  if (is.null(sizes)) {

    # if sizes are not included, make bars equal in size
    colorbar <- rep(1, nrow(color.centers))
    setNames(colorbar, as.character(1:nrow(color.centers)))

  } else {

    # if so, make a fake "table" with counts
    # this is a bit hacky, but it does make the bars adjacent instead of stacked
    sizes <- round(sizes * 1000)
    colorbar <- unlist(sapply(1:length(sizes),
                              function(j) rep(j, sizes[j])))
    colorbar <- table(colorbar, rep("", length(colorbar)))
    rownames(colorbar) <- as.character(1:nrow(color.centers))
  }


  # plot the colors as a uniform bar
  barplot(colorbar, col = rgb.exp,
          axes = FALSE, space = 0, horiz = horiz,
          border = NA, axisnames = FALSE,
          ...)

  # text colors - black if the color is light, white if the color is dark
  hsv.exp <- rgb2hsv(t(color.centers), maxColorValue = 1)
  text.colors <- round(hsv.exp[3, ]) + 1

  # make text locations
  if (horiz == FALSE) {
    text.x <- seq(0.5, length(rgb.exp) - 0.5)
    text.y <- 0.5
  } else {
    text.y <- seq(0.5, length(rgb.exp) - 0.5)
    text.x <- 0.5
  }

  # only plot numbers if the sizes are equal
  # when they're distorted by sizes it gets unpredictable
  if (is.null(sizes)) {
    text(text.x, text.y,
         cex = cex.text,
         col = c("white", "black")[text.colors])
  }

}
