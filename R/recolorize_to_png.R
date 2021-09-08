#' Save a recolored image as a PNG
#'
#' Saves a recolored image from a recolorize object to a PNG. This is
#' done by calling [recoloredImage] and [png::writePNG].
#'
#' @param recolorize_obj A recolorize object.
#' @param filename Filename for saving the PNG.
#'
#' @details This function saves a png with the same dimensions (in pixels) as the
#' image that was originally provided to recolorize (meaning if you resized your original
#' image, the resulting PNG will also be smaller). Anything more complicated can be
#' created with custom scripts: for example, you could create a vector image using
#' [recolorizeVector], and then save this as a PNG of any resolution/size.
#'
#' @examples
#' \dontrun{
#' img <- system.file("extdata/corbetti.png", package = "recolorize")
#' rc <- recolorize2(img, cutoff = 45)
#' recolorize_to_png(rc, "corbetti_recolored.png")
#' }
#'
#' @export
recolorize_to_png <- function(recolorize_obj, filename = "") {
  img <- recoloredImage(recolorize_obj)
  png::writePNG(img, target = filename)
}
