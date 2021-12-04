#' Generate a 'coldist' object for CIE Lab colors
#'
#' A stopgap function for generating a [pavo::coldist] object
#' from CIE Lab colors. This a pretty serious abstraction of the
#' original intention of a `coldist` object, which is to use
#' a combination of spectra data, visual model, and/or receptor-noise
#' model to calculate perceived chromatic and achromatic distances
#' between colors. Because CIE Lab color space is an approximately
#' perceptually uniform color space for human vision, we can calculate
#' a version of those distances for a human viewer directly from
#' CIE Lab. A decent option if you want preliminary results,
#' if you only care about human perception, or if you don't have access
#' to spectral data.
#'
#' @param rgbcols An nx3 matrix of RGB colors (rows are colors and
#' columns are R, G, and B channels).
#'
#' @return A [pavo::coldist] object with four columns: the patches
#' being contrasted (columns 1-2), the chromatic contrast (`dS`),
#' and the achromatic contrast (`dL`), all in units of Euclidean
#' distance in CIE Lab space.
#'
#'
#' @details I have mixed feelings about this function and would like to
#' replace it with something a little less hand-wavey.
cielab_coldist <- function(rgbcols) {

  lab_coldist <- data.frame(t(utils::combn(nrow(rgbcols), 2)),
                            dS = NA, dL = NA)

  colnames(lab_coldist)[1:2] <- c("c1", "c2")

  labcols <- grDevices::convertColor(rgbcols, "sRGB", "Lab")

  for (i in 1:nrow(lab_coldist)) {
    ref_idx <- as.numeric(lab_coldist[i, 1:2])
    lab_coldist$dS[i] <- stats::dist(labcols[ref_idx, 2:3])
    lab_coldist$dL[i] <- stats::dist(labcols[ref_idx, 1])
  }

  return(lab_coldist)
}
