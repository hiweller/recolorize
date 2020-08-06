#' Recolor an image after color clustering
#'
#' Recolors an image after each pixel has been assigned to a particular color.
#' Combines the input of \code{\link[recolorize]{backgroundIndex}} and
#' \code{\link[recolorize]{colorClusters}}.
#'
#' @param bg.indexed An object of class `bg.index`, the output of
#'   \code{\link[recolorize]{backgroundIndex}}.
#' @param color.clusters An object of class `color.clusters`, the output of
#'   \code{\link[recolorize]{colorClusters}}.
#' @param plotting Logical. Plot recolored image results?
#' @param main Plot title.
#' @param remove.empty.clusters Logical. If no pixels are assigned to a given
#'   color cluster, should that cluster be returned by the function, or dropped?
#' @param bg.recolor One of either "transparent" or "white", for masking the
#'   background in the recolored image. See details.
#'
#' @return
#' A list with the following attributes:
#' \enumerate{
#'     \item `recolored.img`: The recolored image, as an RGB array.
#'     \item `centers`: The colors to which the recolored image were mapped.
#' }
#'
#' @details
#' This function works by taking in:
#' \enumerate{
#'     \item A vector of pixel assignments, where each number represents the
#'     color cluster to which the pixel at that index was assigned;
#'     \item A matrix of colors centers whose order is assumed to match that of
#'     the pixel assignments
#'     \item An RGB array whose pixel locations are assumed to match those of
#'     the pixel assignments.
#' }
#'
#' This is all kept organized by keeping these data together in the `bg.index` and `color.clusters`
#' classes, and because this function is mostly called internally by \code{\link[recolorize]{imposeColors}}
#' and \code{\link[recolorize]{recolorize}}. However, it does present the user
#' with opportunities to remap colors at random by reshuffling or swapping the color centers
#' in the `color.clusters` object. This could be useful for a number of things,
#' such as testing analytical or color clustering robustness, or otherwise
#' ruining a nice image with math.
#'
#' For background masking, the two provided options (white or transparent) will
#' look identical when plotted within R. The difference is that transparent
#' background masking adds an alpha channel (thus retaining the RGB values of the background),
#' while "white" will overwrite those values.
#'
#' @examples
#' # load image (recolorize and imposeColors do this automatically)
#' img.path <- system.file("extdata/corbetti.png", package = "recolorize")
#' img <- readImage(img.path)
#' bg.condition <- backgroundCondition(transparent = TRUE,
#'                                     alpha.channel = TRUE)
#' bg.indexed <- backgroundIndex(img, bg.condition)
#'
#' # histogram binning
#' hist.colors <- colorClusters(bg.indexed$non.bg,
#'                              method = "hist", bins = 2)
#'
#' # shuffle colors
#' shuffle <- function(m) {
#'   m[sample(1:nrow(m), nrow(m)), ]
#' }
#' hist.shuffle <- hist.colors
#' hist.shuffle$centers <- shuffle(hist.shuffle$centers)
#'
#' # recolor based on the two cluster sets
#' hist.recolor <- recolorImage(bg.indexed, hist.colors)
#' shuffle.recolor <- recolorImage(bg.indexed, hist.shuffle)
#'
#' # plot them
#' layout(matrix(c(1, 2, 3), ncol = 3))
#' plotImageArray(img, main = "original")
#' plotImageArray(hist.recolor$recolored.img, main = "binning")
#' plotImageArray(shuffle.recolor$recolored.img, main = "shuffled colors")
#'
#' @export
recolorImage <- function(bg.indexed, color.clusters,
                         plotting = FALSE, main = "",
                         remove.empty.clusters = FALSE,
                         bg.recolor = "transparent") {

  # just in case...
  if (class(bg.indexed) != "bg.index" |
      class(color.clusters) != "color.clusters") {
    warning("bg.index and color.clusters should be
            the output of backgroundIndex() and colorClusters(),
            respectively")
  }

  # only two options for background recoloring...please...i have looked at
  # enough magenta backgrounds
  bg.recolor <- match.arg(bg.recolor, choices = c("transparent", "white"))

  # copy non-background pixels to change them
  pix.recolor <- bg.indexed$non.bg

  # clusters with any pixels assigned to them
  cluster_idx <- as.numeric(names(color.clusters$sizes))

  # empty centers
  empty_centers <- which(!1:nrow(color.clusters$centers) %in% cluster_idx)

  # for every non-empty cluster:
  for (i in cluster_idx) {

    # get the new color
    new_color <- as.matrix(color.clusters$centers[i, ])

    # get pixels assigned to that color
    pix.idx <- which(color.clusters$pixel.assignments == i)

    # repeat the new color for a substitute matrix
    replacements <- matrix(new_color, ncol = 3, byrow = TRUE,
                           nrow = length(pix.idx))

    # and stick it back in!
    pix.recolor[pix.idx, 1:3] <- replacements

  }

  # index for removing empty centers
  core.removal <- c()

  # slot new pixels back in
  recolored.img <- bg.indexed$flattened.img

  # if there's no background...
  if (length(bg.indexed$idx.flat) == 0) {

    recolored.img <- pix.recolor

  } else {
    recolored.img[-bg.indexed$idx.flat, ] <- pix.recolor
    # color background in white or transparent
    if (bg.recolor == "white") {

      recolored.img[bg.indexed$idx.flat, ] <- 1

    } else if (bg.recolor == "transparent") {

      alpha.channel <- rep(1, nrow(recolored.img))
      alpha.channel[bg.indexed$idx.flat] <- 0
      recolored.img <- cbind(recolored.img, alpha.channel)
      colnames(recolored.img) <- NULL
      bg.indexed$img.dims[3] <- 4

    }

  }

  # reshape
  dim(recolored.img) <- bg.indexed$img.dims

  if (plotting) {
    plotImageArray(recolored.img, main = main)
  }

  # make returnables
  if (length(core.removal) > 0 & isTRUE(remove.empty.clusters)) {
    centers <- color.clusters$centers[-empty_centers, ]
  } else {
    centers <- color.clusters$centers
  }
  return(list(recolored.img = recolored.img,
              centers = centers))

}
