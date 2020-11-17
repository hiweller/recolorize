#' Run `pavo`'s adjacency and boundary strength analysis on a `recolorize`
#' object
#'
#' Run adjacency (Endler 2012) and boundary strength (Endler et al. 2018)
#' analysis directly on a `recolorize` object, assuming a human viewer
#' (i.e. using CIE Lab and HSL color distances that correspond to
#' perceptual distances of human vision). This is achieved by
#' converting the `recolorize` object to a [pavo::classify] object,
#' converting the colors to HSL space, and calculating a [pavo::coldist] object
#' for CIE Lab color space before running [pavo::adjacent].
#'
#' @param recolorize_obj A `recolorize` object.
#' @param coldist A [pavo::coldist] object; otherwise, this argument
#'   is ignored and a `coldist` object for human vision is calculated from
#'   RGB colors converted to CIE Lab using [recolorize::cielab_coldist].
#' @param hsl A dataframe with `patch`, `hue`, `sat` and `lum` columns
#'   specifying the HSL values for each color patch, to be
#'   passed to [pavo::adjacent]. Otherwise, this argument
#'   is ignored and HSL values are calculated for human vision from the RGB
#'   colors in the `recolorize` object.
#' @param xscale The length of the x-axis, in preferred units. Passed to
#'   [pavo::adjacent].
#' @param ... Further arguments passed to [pavo::adjacent].
#'
#' @return The results of [pavo::adjacent]; see that documentation
#' for the meaning of each specific value.
#'
#' @seealso [pavo::adjacent], [classify_recolorize]
#'
#' @details
#' Eventually, the plan is to incorporate more sophisticated
#' color models than using human perceptual color distances, i.e.
#' by allowing users to match color patches to spectra. However,
#' this does return reasonable and informative results so long as
#' human vision is an appropriate assumption for the image data.
#'
#' @examples
#' img <- system.file("extdata/chongi.png", package = "recolorize")
#' recolorize_obj <- recolorize(img, method = "k", n = 2)
#' recolorize_adjacency(recolorize_obj)
#'
#' @export
recolorize_adjacency <- function(recolorize_obj,
                                 xscale = 1,
                                 coldist = "default",
                                 hsl = "default",
                                 ...) {
  # get image
  img <- recolorize_obj$recolored_img

  # fit a classify obj
  # class_img <- suppressWarnings(classify(as.rimg(img),
  #                       kcols = nrow(recolorize_obj$centers) + 1))
  class_img <- classify_recolorize(recolorize_obj)

  # get hsl colors
  hsl_colors <- rgb2hsl(attr(class_img, "classRGB"))
  if (is.data.frame(hsl)) {
    if (dimnames(hsl_colors[[2]]) == colnames(hsl) &
        nrow(hsl_colors) == nrow(hsl)) {
      hsl_colors <- hsl
    } else {
      warning("`hsl` dataframe incorrectly formatted; calculating
            hsl using standard (human) HSL values")
    }
  }

  # the background will be white (we made it white),
  # so we can identify it via the lightness channel:
  bkgID <- which(hsl_colors$lum == max(hsl_colors$lum))

  # get color distances in pavo coldist format:
  # important- these are CIE Lab/human color distances
  # the "chromatic" (dS) distances are in the a & b (color) channels
  # the "achromatic" (dL) channels are in the luminance (L) axis
  coldists <- cielab_coldist(attr(class_img, "classRGB"))
  if (is.data.frame(coldist)) {
    if (dimnames(coldists[[2]]) == colnames(coldist) &
        nrow(coldists) == nrow(coldist)) {
      coldists <- coldist
    } else {
      warning("`coldist` dataframe incorrectly formatted; calculating
            coldists using standard (human) perceptual color distances")
    }
  }

  img_adj <- pavo::adjacent(class_img, xscale = xscale,
                            bkgID = bkgID,
                            hsl = hsl_colors,
                            coldists = coldists,
                            exclude = "background",
                            ...)

  return(img_adj)
}

#' Convert a `recolorize` object to a `classify` object
#'
#' Converts a [recolorize] object to a [pavo::classify] object for
#' use in pavo.
#'
#' @param recolorize_obj A `recolorize` object.
#' @param imgname Name of the image (a string).
#'
#' @return A [pavo::classify] object. The background patch will always
#' be the first color (patch 1), and will be white by default.
classify_recolorize <- function(recolorize_obj, imgname = "") {
  pmat <- recolorize_obj$pixel_assignments + 1
  attr(pmat, "dim") <- dim(pmat)
  class(pmat) <- c("rimg", "matrix")
  classRGB <- rbind(c(1, 1, 1), recolorize_obj$centers)
  rownames(classRGB) <- c(1:nrow(classRGB))
  colnames(classRGB) <- c("R", "G", "B")
  attr(pmat, "classRGB") <- classRGB
  attr(pmat, "colnames") <- data.frame(name = 1:(nrow(recolorize_obj$centers) + 1))
  attr(pmat, "px_scale") <- NA
  attr(pmat, "imgname") <- imgname
  attr(pmat, "state") <- "colclass"
  attr(pmat, "k") <- nrow(recolorize_obj$centers) + 1
  return(pmat)
}


#' Convert RGB colors to HSL
#'
#' Convert RGB colors (0-1 range) to HSL (hue-saturation-luminance)
#' space. Used for passing RGB colors to [pavo::adjacent].
#'
#' @param rgb_matrix RGB colors in an nx3 matrix (rows = colors,
#' columns = channels).
#' @param radians Logical. Return HSL colors in units of radians
#' (`TRUE`) or degrees (`FALSE`)?
#' @param pavo_hsl Logical. Return HSL matrix in a format that
#' can be passed directly to [pavo::adjacent] as the `hsl` parameter?
#'
#' @return A dataframe with `patch`, `hue`, `sat`, and `lum` columns
#' and one row per color (if `pavo_hsl = TRUE`) or a matrix of the HSL
#' coordinates (if `pavo_hsl = FALSE`).
#'
#' @examples
#' cols <- expand.grid(0:1, 0:1, 0:1)
#' recolorize:::rgb2hsl(cols, radians = FALSE)
rgb2hsl <- function(rgb_matrix,
                    radians = TRUE,
                    pavo_hsl = TRUE) {

  # make sure range is 0-1
  if (max(rgb_matrix) > 1) {
    rgb_matrix <- rgb_matrix / 255
  }

  # xerox:
  hsl_matrix <- matrix(NA,
                       nrow = nrow(rgb_matrix),
                       ncol = ncol(rgb_matrix),
                       dimnames = list(rownames(rgb_matrix),
                                       c("h", "s", "l")))

  for (i in 1:nrow(rgb_matrix)) {

    # extract color
    rgb_vec <- as.vector(rgb_matrix[i, ])

    # get max and min
    xmin <- min(rgb_vec)
    xmax <- max(rgb_vec)

    # get the dominant value (determines which area of the radial space we occupy)
    V <- c("R", "G", "B")[which(rgb_vec == xmax)[1]]

    # find chroma:
    C <- xmax - xmin

    # find lightness, avg of xmax and xmin:
    lightness <- (xmax + xmin) / 2

    # if all values are equal, then the color is grey, no hue/saturation:
    if (C == 0) {

      # both are 0 if xmax == xmin:
      hue <- 0
      saturation <- 0

    } else {

      # hue conversion depends on whether R, G, or B dominate:
      if (V == "R") {
        hue <- 60 * (0 + ((rgb_vec[2] - rgb_vec[3]) / C))
      } else if (V == "G") {
        hue <- 60 * (2 + ((rgb_vec[3] - rgb_vec[1]) / C))
      } else if (V == "B") {
        hue <- 60 * (4 + ((rgb_vec[1] - rgb_vec[2]) / C))
      }

      if (hue < 0) {
        hue <- 360 + hue
      }

      # and saturation depends on xmax:
      saturation <- C / xmax

    }

    # convert to radians:
    if (radians) {
      hue <- hue * (2 * pi) / 360
    }

    hsl_matrix[i, 1] <- as.numeric(hue)
    hsl_matrix[i, 2] <- as.numeric(saturation)
    hsl_matrix[i, 3] <- as.numeric(lightness)
  }

  if (pavo_hsl) {
    hsl_dataframe <- as.data.frame(hsl_matrix)
    hsl_dataframe <- cbind(patch = rownames(hsl_dataframe),
                           hsl_dataframe)
    colnames(hsl_dataframe) <- c("patch", "hue", "sat", "lum")
    return(hsl_dataframe)
  } else {
    return(hsl_matrix)
  }

}

