#' Edit a color patch using morphological operations
#'
#' Applies one of several morphological operations from `imager` to a layer of a
#' recolorize object. Convenient for cleaning up a color patch without affecting
#' other layers of the recolorized image. This can be used to despeckle, fill in
#' holes, or uniformly grow or shrink a color patch.
#'
#' @param recolorize_obj A recolorize object from \code{\link{recolorize}},
#'   \code{\link{recluster}}, or \code{\link{imposeColors}}.
#' @param layer_idx A single index value (numeric) indicating which
#'   layer to edit. Corresponds to the order of the colors in the `centers`
#'   attribute of the recolorize object, and to the indices in the
#'  `pixel_assignments` attribute of the same.
#' @param operation The name of an imager morphological operation to perform on
#'   the layer, passed as a string. See details.
#' @param px_size The size (in pixels) of the elements to filter. If
#'   `operation = "shrink"` and `px_size = 2`, for example, the color
#'   patch will be shrunk by a 2-pixel radius.
#' @param plotting Logical. Plot results?
#'
#' @return A `recolorize` object. The `sizes`, `pixel_assignments,`, and
#'   `recolored_img` attributes will differ from the input object for the
#'   relevant color patch (layer) to reflect the edited layer.
#'
#' @details
#' Current imager operations are:
#' \itemize{
#'  \item \code{\link[imager]{grow}}: Grow a pixset
#'  \item \code{\link[imager]{shrink}}: Shrink a pixset
#'  \item \code{\link[imager]{fill}}: Remove holes in an pixset. Accomplished by
#'    growing and then shrinking a pixset.
#'  \item \code{\link[imager]{clean}}: Remove small isolated elements (speckle).
#'    Accomplished by shrinking and then growing a pixset.
#' }
#'
#' @examples
#' # load image and recolorize it
#' img <- system.file("extdata/corbetti.png", package = "recolorize")
#'
#' # first do a standard color binning
#' init_fit <- recolorize(img, bins = 2, plotting = FALSE)
#'
#' # then cluster patches by similarity
#' re_fit <- recluster(init_fit, cutoff = 40)
#'
#' # to reset graphical parameters:
#' current_par <- graphics::par(no.readonly = TRUE)
#'
#' # examine individual layers:
#' layout(matrix(1:6, nrow = 2))
#' layers <- splitByColor(re_fit, plot_method = "color")
#'
#' # notice patch 2 (cream) - lots of stray pixels
#' edit_cream_layer <- editLayer(re_fit,
#'                               layer_idx = 2,
#'                               operation = "clean",
#'                               px_size = 3)
#'
#' # shrinking and growing by the same element size gives us less flexibility, so
#' # we can also shrink and then grow, using different px_size arguments:
#' edit_green_1 <- editLayer(re_fit,
#'                           layer_idx = 4,
#'                           operation = "shrink",
#'                           px_size = 2)
#' edit_green_2 <- editLayer(edit_green_1,
#'                           layer_idx = 4,
#'                           operation = "grow",
#'                           px_size = 3)
#'
#' # we can get pleasingly mondrian about it:
#' new_fit <- re_fit
#' for (i in 1:nrow(new_fit$centers)) {
#'   new_fit <- editLayer(new_fit,
#'                        layer_idx = i,
#'                        operation = "fill",
#'                        px_size = 5, plotting = FALSE)
#' }
#' plot(new_fit)
#'
#' graphics::par(current_par)
#'
#' @seealso [editLayers] for editing multiple layers (with multiple operations)
#'   at once; a wrapper for this function.
#'
#' @export
editLayer <- function(recolorize_obj,
                      layer_idx,
                      operation = "clean",
                      px_size = 2,
                      plotting = TRUE) {

  # get layer
  layer <- splitByColor(recolorize_obj,
                        layers = layer_idx,
                        plot_method = "none")[[1]]
  cimg_layer <- imager::as.cimg(layer)

  # edit
  new_layer <- apply_imager_operation(cimg_layer, operation, px_size)

  # set the original layer to background
  original_idx <- which(recolorize_obj$pixel_assignments == layer_idx)
  recolorize_obj$pixel_assignments[original_idx] <- 0

  # set the new layer
  array_layer <- cimg_to_array(new_layer)
  recolorize_obj$pixel_assignments[which(array_layer > 0)] <- layer_idx
  recolorize_obj$sizes[layer_idx] <- sum(array_layer)

  # and reconstruct the image
  recolorize_obj$recolored_img <- constructImage(recolorize_obj$pixel_assignments,
                                                 recolorize_obj$centers)

  # plot if we're plotting
  if (plotting) {

    # reset graphical parameters when function exits:
    current_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(current_par))

    graphics::layout(matrix(1:3, nrow = 1))
    plotImageArray(layer, main = "original layer")
    plotImageArray(array_layer, main = "edited layer")
    plotImageArray(recolorize_obj$recolored_img,
                   main = "resulting color map")

  }

  # append the call
  recolorize_obj$call <- append(recolorize_obj$call, match.call())

  # return
  return(recolorize_obj)

}

#' Edit multiple color patches using morphological operations
#'
#' A wrapper for [editLayer], allowing for multiple layers
#' to be edited at once, either with the same morphological operation
#' or specified for each layer.
#'
#' @param recolorize_obj A recolorize object from \code{\link{recolorize}},
#'   \code{\link{recluster}}, or \code{\link{imposeColors}}.
#' @param layer_idx A numeric vector of layer indices to be edited, or `"all"`
#'   (in which case all layers are edited). Corresponds to the order of the
#'   colors in the `centers` attribute of the recolorize object, and to the
#'   indices in the `pixel_assignments` attribute of the same.
#' @param operations Either a single string OR a character vector of imager
#'   morphological operation(s) to perform on the specified layer(s). If this is
#'   shorter than `layer_idx`, it is repeated to match the length of
#'   `layer_idx`.
#' @param px_sizes The size(s) (in pixels) of the elements to filter. Either a
#'   single number OR a numeric vector. If shorter than `layer_idx`, it is
#'   repeated to match the length of `layer_idx`. If `operation = "shrink"` and
#'   `px_size = 2`, for example, the color patch will be shrunk by a 2-pixel
#'   radius.
#' @param plotting Logical. Plot results?
#'
#' @return A `recolorize` object. The `sizes`, `pixel_assignments,`, and
#'   `recolored_img` attributes will differ from the input object for the
#'   relevant color patches (layers) to reflect their changes.
#'
#' @details
#' Current imager operations are:
#' \itemize{
#'  \item \code{\link[imager]{grow}}: Grow a pixset
#'  \item \code{\link[imager]{shrink}}: Shrink a pixset
#'  \item \code{\link[imager]{fill}}: Remove holes in an pixset. Accomplished by
#'    growing and then shrinking a pixset.
#'  \item \code{\link[imager]{clean}}: Remove small isolated elements (speckle).
#'    Accomplished by shrinking and then growing a pixset.
#' }
#'
#' @examples
#' # load image and recolorize it
#' img <- system.file("extdata/corbetti.png", package = "recolorize")
#'
#' # first do a standard color binning
#' init_fit <- recolorize(img, bins = 2, plotting = FALSE)
#'
#' # then cluster patches by similarity
#' re_fit <- recluster(init_fit, cutoff = 40)
#'
#' # to reset graphical parameters:
#' current_par <- graphics::par(no.readonly = TRUE)
#'
#' # examine individual layers:
#' layout(matrix(1:6, nrow = 2))
#' layers <- splitByColor(re_fit, plot_method = "color")
#'
#' # we can clean them all using the same parameters...
#' edited_fit <- editLayers(re_fit, layer_idx = "all",
#'                          operations = "clean",
#'                          px_sizes = 2, plotting = TRUE)
#' # ...but some of those patches don't look so good
#'
#' # we can use different px_sizes for each layer:
#' edited_fit_2 <- editLayers(re_fit, layer_idx = "all",
#'                            operations = "clean",
#'                            px_sizes = c(1, 3, 1,
#'                                         2, 1, 2),
#'                            plotting = TRUE)
#'
#' # better yet, we can fill some layers and clean others:
#' edited_fit_3 <- editLayers(re_fit, layer_idx = "all",
#'                            operations = c("fill", "clean",
#'                                           "fill", "fill",
#'                                           "fill", "clean"),
#'                            px_sizes = c(2, 3,
#'                                         2, 2,
#'                                         4, 2))
#'
#' # or you could just get weird:
#' edited_fit_3 <- editLayers(re_fit, layer_idx = c(1:6),
#'                            operations = c("fill", "clean"),
#'                            px_sizes = c(10, 20))
#'
#' # reset graphical parameters:
#' graphics::par(current_par)
#'
#' @seealso [editLayer] for editing a single layer at a time.
#'
#' @export
editLayers <- function(recolorize_obj,
                       layer_idx = "all",
                       operations = "clean",
                       px_sizes = 2,
                       plotting = TRUE) {

  if (!is.numeric(layer_idx)) {
    if (layer_idx == "all") {
      layer_idx <- 1:nrow(recolorize_obj$centers)
    } else {
      stop("'layer_idx' must be either 'all' or a numeric
         vector of layer indices to edit")
    }
  }

  # make sure operations and px_sizes are the same length
  if (length(layer_idx) != length(operations)) {
    operations <- rep(operations, length.out = length(layer_idx))
  }
  if (length(layer_idx) != length(px_sizes)) {
    px_sizes <- rep(px_sizes, length.out = length(layer_idx))
  }

  # for every layer...
  for (i in 1:length(layer_idx)) {

    # edit it accordingly
    recolorize_obj <- editLayer(recolorize_obj,
                                layer_idx = layer_idx[i],
                                operation = operations[i],
                                px_size = px_sizes[i],
                                plotting = FALSE)
  }

  # plot if we're plotting
  if (plotting) {
    plot(recolorize_obj)
  }

  # append the call
  recolorize_obj$call <- append(recolorize_obj$call, match.call())

  # that's it!
  return(recolorize_obj)

}

