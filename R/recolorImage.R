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
#' @param bg.color A color (either an RGB triplet, hex code, or R color name)
#'   for the background color. Will not be visible unless the alpha channel is
#'   removed when plotting; see details.
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
#' Note that regardless of input background type, the returned image
#' uses the alpha channel to indicate the background, effectively
#' storing a background mask in the alpha channel. Users can also
#' change the background color by specifying it in the function, but
#' it will appear as white unless only the first three channels
#' are plotted.
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
                         bg.color = "white") {

  # just in case...
  if (class(bg.indexed) != "bg.index" |
      class(color.clusters) != "color.clusters") {
    warning("bg.index and color.clusters should be
            the output of backgroundIndex() and colorClusters(),
            respectively")
  }

  # first, make a pixel assignment matrix:
  pixel_assignments <- pixelAssignMatrix(bg.indexed,
                                         color.clusters)

  # make an image from the above information:
  recolored_img <- constructImage(pixel_assignments$pixel_assignments,
                                  pixel_assignments$color_centers,
                                  background_color = bg.color)

  # plot if plotting:
  if (plotting) {
    plotImageArray(recolored_img, main = main)
  }

  # just in case:
  empty_centers <- which(color.clusters$sizes == 0)

  # make returnables
  if (length(empty_centers) > 0 & isTRUE(remove.empty.clusters)) {
    centers <- color.clusters$centers[-empty_centers, ]
  } else {
    centers <- color.clusters$centers
  }

  # return it!
  return(list(recolored.img = recolored_img,
              centers = centers))

}

#' Make pixel assignment matrix for recoloring
#'
#' Internal function. Generates a sort of 'paint-by-numbers' matrix, where each
#' cell is the index of the color in the color centers matrix to which that
#' pixel is assigned. An index of 0 indicates a background pixel.
#'
#' @param bg_indexed An object returned by \code{\link{backgroundIndex}}.
#' @param color_clusters An object returned by \code{\link{colorClusters}}.
#'
#' @return A matrix of pixel color assignments (`pixel_assignments`)
#' and a corresponding dataframe of color centers (`color_centers`).
pixelAssignMatrix <- function(bg_indexed, color_clusters) {

  # make a vector of 0's, one per image pixel
  pix_assign <- rep(0, nrow(bg_indexed$flattened.img))

  # swap in the color assignments for the pixels
  pix_assign[-bg_indexed$idx.flat] <- color_clusters$pixel.assignments

  # and reshape:
  dim(pix_assign) <- bg_indexed$img.dims[1:2]

  # return it!
  return(list(pixel_assignments = pix_assign,
              color_centers = color_clusters$centers))

}


#' Generate an image from pixel assignments and color matrix
#'
#' Combines a matrix of pixel assignments and a corresponding
#' matrix of colors to make a recolored RGB image.
#'
#' @param pixel_assignments A matrix of index values for each pixel which
#'   corresponds to `color_centers` (e.g. a `1` indicates that pixel is the
#'   color of the first row of `color_centers`). Pixels with an index value of 0
#'   are considered background.
#' @param color_centers An n x 3 matrix of color centers where rows are colors
#'   and columns are R, G, and B channels.
#' @param background_color A numeric RGB triplet, a hex code, or a named
#'   R color for the background. Will be masked by alpha channel (and appear
#'   white in the plot window), but will be revealed if the alpha
#'   channel is removed. If the alpha channel is a background mask,
#'   this is the 'baked in' background color.
#'
#' @return An image (raster) array of the recolored image,
#' with four channels (R, G, B, and alpha).
#'
#' @export
constructImage <- function(pixel_assignments,
                           color_centers,
                           background_color = "white") {

  # make two copies of matrix as a cimg object:
  index_cimg <- imager::as.cimg(pixel_assignments)
  final_cimg <- index_cimg

  # color the background in
  # you won't see this unless you remove the alpha layer:
  final_cimg <- imager::colorise(final_cimg,
                                 index_cimg == 0,
                                 background_color)

  # color in every color center:
  for (i in 1:nrow(color_centers)) {
    final_cimg <- imager::colorise(final_cimg,
                                   index_cimg == i,
                                   color_centers[i, ])
  }

  # convert to a regular array:
  as_array <- cimg.to.array(final_cimg)

  # and add an alpha channel:
  alpha_layer <- pixel_assignments
  alpha_layer[which(alpha_layer > 0)] <- 1
  as_array <- abind::abind(as_array,
                           alpha_layer,
                           along = 3)

  # beep boop:
  return(as_array)

}

