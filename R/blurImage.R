#' Blur an image
#'
#' Blurs an image using the one of five blur functions in `imager`.
#' Useful for decreasing image noise.
#'
#' @param img An image array, as read in by [png::readPNG] or [readImage].
#' @param blur_function A string matching the name of an imager blur function.
#' One of c("isoblur", "medianblur", "blur_anisotropic", "boxblur",
#' "boxblur_xy").
#' @param ... Parameters passed to whichever `blur_function` is called.
#' @param plotting Logical. Plot the blurred image next to the input
#' for comparison?
#' @return An image array of the blurred image.
#'
#' @details The parameters passed with the `...` argument are specific
#' to each of the five blur functions; see their documentation for what to
#' specify: [imager::isoblur], [imager::medianblur], [imager::boxblur],
#' [imager::blur_anisotropic], [imager::boxblur_xy]. The `medianblur` and
#' `blur_anisotropic` functions are best for preserving edges.
#'
#' @examples
#' img_path <- system.file("extdata/fulgidissima.png", package = "recolorize")
#' img <- readImage(img_path)
#' median_img <- blurImage(img, "medianblur", n = 5, threshold = 0.5)
#' anisotropic_img <- blurImage(img, "blur_anisotropic",
#'                              amplitude = 5, sharpness = 0.1)
#' boxblur_img <- blurImage(img, "boxblur", boxsize = 5)
#'
#' # save current graphical parameters:
#' current_par <- graphics::par(no.readonly = TRUE)
#' graphics::layout(matrix(1:4, nrow = 1))
#'
#' plotImageArray(img, "original")
#' plotImageArray(median_img, "median")
#' plotImageArray(anisotropic_img, "anisotropic")
#' plotImageArray(boxblur_img, "boxblur")
#'
#' # and reset:
#' graphics::par(current_par)
#' @export
blurImage <- function(img, blur_function = "medianblur",
                      ..., plotting = TRUE) {

  blur_function <- match.arg(blur_function,
                             c("isoblur", "blur_anisotropic",
                               "boxblur", "boxblur_xy", "medianblur"))
  c_img <- array_to_cimg(img)
  blur_img <- switch(blur_function,
                     isoblur = imager::isoblur(c_img, ...),
                     medianblur = imager::medianblur(c_img, ...),
                     boxblur = imager::boxblur(c_img, ...),
                     blur_anisotropic = imager::blur_anisotropic(c_img, ...),
                     boxblur_xy = imager::boxblur_xy(c_img, ...))
  new_img <- cimg_to_array(blur_img)
  if (dim(img)[3] == 4) {
    new_img <- abind::abind(new_img, img[ , , 4])
  }

  # I don't get why this happens but alas
  new_img[which(new_img < 0)] <- 0
  new_img[which(new_img >= 1)] <- 1

  # plot if we're plotting
  if (plotting) {

    # courtesy:
    current_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(current_par))

    graphics::layout(matrix(1:2, nrow = 1))
    plotImageArray(img, "original")
    plotImageArray(new_img, "blurred")

  }

  return(new_img)

}
