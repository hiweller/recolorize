# similar to recolorize, but you specify color centers instead of getting them
# from the image
imposeClusters <- function(img.path, color.centers,
                           adjust.centers = TRUE,
                           lower = NULL, upper = NULL,
                           transparent = TRUE,
                           resize = NULL, rotate = NULL,
                           plotting = TRUE, horiz = TRUE,
                           cex.text = 1.5, scale.palette = TRUE) {

  # read in image
  img <- readImage(img.path, resize = resize, rotate = rotate)

  # make background condition
  alpha.channel <- dim(img)[3] == 4 # is there a transparency channel?
  bg.condition <- backgroundCondition(lower = lower, upper = upper,
                                      center = NULL, radius = NULL,
                                      transparent = transparent,
                                      alpha.channel = alpha.channel)

  # index background
  bg.indexed <- backgroundIndex(img, bg.condition)

  # color clusters & assign pixels
  color.clusters <- assignPixels(color.centers, bg.indexed$non.bg,
                                 adjust.centers = adjust.centers)

  # recolor based on assignments/centers
  recolored <- recolorImage(bg.indexed, color.clusters,
                            plotting = FALSE,
                            remove.empty.clusters = FALSE)

  # get sizes vector
  sizes <- color.clusters$sizes
  if (scale.palette) { s <- sizes } else { s <- NULL }

  # plot result
  if (plotting) {
    plotRecolorized(recolored$recolored.img, img,
                    plot.original = TRUE,
                    recolored$centers, horiz = horiz,
                    cex.text = cex.text,
                    sizes = s)
  }

  # returnables:
  original.img <- img
  recolored.img <- recolored$recolored.img

  # only rgb for now...would others be useful?
  color.space <- "RGB"
  centers <- color.clusters$centers
  pixel.assignments <- color.clusters$pixel.assignments

  # return em
  return.list <- list(original.img = original.img,
                      recolored.img = recolored.img,
                      color.space = color.space,
                      centers = centers,
                      sizes = sizes,
                      pixel.assignments = pixel.assignments)

}
