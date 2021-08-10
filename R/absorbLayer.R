#' Absorb a layer into its surrounding color patches
#'
# Every component of a layer which satisfies a size and location condition is
# 'absorbed' into the color map by switching its color to that of the color
# patch with which it shares the longest border. Useful for dealing with
# reflections, gloss, speckles, small features, etc.

#' @param recolorize_obj A `recolorize` object.
#' @param layer_idx The numeric index of the layer to absorb.
#' @param size_condition A condition for determining which components to absorb,
#' written as a function. The default (`function(l) l <= Inf`) will affect
#' all components, since they all have fewer than infinity pixels.
#' @param x_range,y_range The rectangular bounding box (as proportions of the
#'   image width and length) for selecting patches. Patches with at least
#'   partial overlap are counted. Defaults (0-1) include the entire image.
#'   See details.
#' @param remove_empty_layers Logical. If the layer is completely absorbed,
#'   remove it from the layer indices and renumber the existing patches? (Example:
#'   if you completely absorb layer 3, then layer 4 -> 3 and 5 -> 4, and so on).
#' @param highlight_color Color for highlighting the affected layer.
#' @param plotting Logical. Plot results?
#'
#' @return A `recolorize` object.
#'
#' @details This function works by splitting a layer into spatially distinct
#' 'components' using [imager::split_connected]. A contiguous region of pixels
#' is considered a single component. Only components which satisfy
#' both the `size_condition` and the location condition (specified via `x_range`
#' and `y_range`) are absorbed, so you can be target specific regions with
#' (ideally) a minimum of fuss.
#'
#' The `size_condition` is passed as a function which must have a logical
#' vector output (`TRUE` and `FALSE`) when applied to a vector of sizes.
#' Usually this will be some combination of greater and less than statements,
#' combined with logical operators like `&` and `|`. For example,
#' `size_condition = function(x) x > 100 | x < 10` would affect components of
#' greater than 100 pixels and fewer than 10 pixels, but not those with 10-100
#' pixels.
#'
#' The `x_range` and `y_range` values set the bounding box of a rectangular
#' region as proportions of the image axes, with the origin (0, 0) in the bottom
#' left corner. Any patch which has at least partial overlap with this bounding
#' box will be considered to satisfy the condition. When selecting this region,
#' it can be helpful to plot a grid on the image first to narrow down an
#' approximate region (see examples).
#'
#'
#' @seealso [editLayers] for editing layers using morphological operations;
#' [thresholdRecolor] for re-fitting the entire image without minor colors.
#'
#' @examples
#'
#' \dontrun{
#' img <- system.file("extdata/fulgidissima.png", package = "recolorize")
#'
#' # get an initial fit using recolorize + recluster:
#' fit1 <- recolorize2(img, bins = 3, cutoff = 65, plotting = FALSE)
#' # this looks okay, but the brown patch (3) has some speckling
#' # in the upper right elytron due to reflection, and the orange
#' # patch (4) has the same issue
#'
#' # the brown patch is easier to deal with, since size thresholding alone is
#' # sufficient; we want to leave the stripes intact, so we'll absorb components
#' # that are 50-250 pixels OR fewer than 20 pixels (to get the tiny speckles),
#' # leaving the eyes intact
#' fit2 <- absorbLayer(fit1, layer_idx = 3,
#'                     size_condition = function(x) x <= 250 &
#'                       x >= 50 |
#'                       x < 20,
#'                     highlight_color = "cyan")
#'
#' # what about the orange speckles? this is more difficult, because
#' # we want to retain the border around the brown stripes, but those patches
#' # are quite small, so size thresholding won't work:
#' fit_bad <- absorbLayer(fit2, layer_idx = 4,
#'                        size_condition = function(x) x < 25)
#'
#' # but we just want to target pixels in that one region, so we can first
#' # determine a bounding box for it by plotting a grid:
#' plotImageArray(constructImage(fit2$pixel_assignments,
#'                     fit2$centers))
#' axis(1, line = 3); axis(2, line = 1)
#' abline(v = seq(0, 1, by = 0.1),
#'        h = seq(0, 1, by = 0.1),
#'        col = grey(0.2),
#'        lty = 2)
#' # x-axis range: 0.5-0.7
#' # y-axis range: 0.55-0.75
#' # let's try it:
#' fit3 <- absorbLayer(fit2, layer_idx = 4,
#'                     size_condition = function(x) x < 100,
#'                     x_range = c(0.5, 0.7),
#'                     y_range = c(0.55, 0.75),
#'                     highlight_color = "yellow")
#' # looks pretty good
#' }
#' @export
absorbLayer <- function(recolorize_obj,
                        layer_idx,
                        size_condition = function(s) s <= Inf,
                        x_range = c(0, 1),
                        y_range = c(0, 1),
                        highlight_color = "yellow",
                        remove_empty_layers = TRUE,
                        plotting = TRUE) {

  # get object layer
  layer <- splitByColor(recolorize_obj, layers = layer_idx,
                         plot_method = "none")[[1]]

  # convert to an imager pixset for splitting
  px <- imager::as.pixset(imager::as.cimg(layer) > 0)

  # split separate components
  layer_split <- imager::split_connected(px)

  # get component sizes
  component_sizes <- unlist(lapply(layer_split, sum))

  # find which components satisfy the size condition
  condition_met <- which(ifelse(size_condition(component_sizes), TRUE, FALSE))

  # location condition - bounding box
  # get bounding box (in pixel coordinates) of each component:
  component_bbox <- lapply(layer_split, function(l) apply(which(l[ , , 1, 1],
                                              arr.ind = TRUE), 2,
                                        range))

  # convert to fraction of image dimensions:
  imdim <- dim(recolorize_obj$original_img)[1:2]
  norm_matrix <- matrix(imdim, nrow = 2, ncol = 2, byrow = TRUE)
  component_bbox_norm <- lapply(component_bbox,
                                function(b) round(b / norm_matrix, 3))
  # important - remember that imager rotates images 90 degrees (argh),
  # so y-axis is rows and x-axis is columns
  y_range <- 1 - y_range # also the y-axis is flipped
  bbox_condition_met <- lapply(component_bbox_norm,
                               function(b) any(any(b[ , 1] >= y_range[2]) &
                                               any(b[ , 1] <= y_range[1])) &
                                           any(any(b[ , 2] >= x_range[1]) &
                                               any(b[ , 2] <= x_range[2])))
  condition_met <- intersect(condition_met,
                             which(unlist(bbox_condition_met)))

  # make a color center map from the pixel assignments (this will make sense in
  # a bit)
  old_map <- recolorize_obj$pixel_assignments
  old_centers <- recolorize_obj$centers
  map <- imager::as.cimg(old_map)

  if(length(condition_met) == 0) {
    stop("No layer components meet specified conditions.")
  }

  # for every component that meets the size condition:
  for (i in condition_met) {

    # extract the component
    component <- layer_split[[i]]

    # get indices of immediately adjacent pixels
    px_contour <- imager::boundary(imager::grow(component, 3))

    # count which color patches those pixels are assigned to
    border_cols <- table(map[px_contour])

    # we don't want to turn these pixels transparent, so we'll ignore
    # any borders with the background (patch 0):
    if(names(border_cols)[1] == "0") {
      border_cols <- border_cols[-1]
    }

    # get name of longest border
    ctr_idx <- names(which(border_cols == max(border_cols)))
    ctr_idx <- as.numeric(ctr_idx)

    # if there is more than one color patch bordering it...
    if (length(ctr_idx) > 1) {

      # ...switch that component to the color of the patch
      # with which it shares the longest border
      majority_rule <- which(recolorize_obj$sizes[ctr_idx] ==
                               max(recolorize_obj$sizes[ctr_idx]))

      ctr_idx <- ctr_idx[majority_rule]
    }

    # and change the indices
    map[component] <- as.numeric(ctr_idx)
    i <- i + 1
  }

  # switch the new patch map in
  map <- cimg_to_array(map)

  # if we completely eliminated a patch...
  if (length(condition_met) == length(layer_split) &
      remove_empty_layers) {

    # change the higher indices to match new centers
    if (layer_idx < nrow(recolorize_obj$centers)) {
      map[map > layer_idx] <- map[map > layer_idx] - 1
    }

    # remove it from the color centers
    recolorize_obj$centers <- recolorize_obj$centers[-layer_idx, ]
    rownames(recolorize_obj$centers) <- 1:nrow(recolorize_obj$centers)
  }

  # switch in the new map
  recolorize_obj$pixel_assignments <- map

  # and make the image
  new_img <- constructImage(map,
                            recolorize_obj$centers)

  # then, get new sizes (minus background)
  recolorize_obj$sizes <- table(map)[-1]

  if (nrow(recolorize_obj$centers) < length(recolorize_obj$sizes)) {
    recolorize_obj$sizes <- c(recolorize_obj$sizes[1:(layer_idx - 1)],
                              0,
                  recolorize_obj$sizes[layer_idx:length(recolorize_obj$sizes)])
  }

  # plot if we're plotting
  if (plotting) {

    # get boundaries of original pixels & make highlighted array
    components <- layer_split[condition_met]
    layer_px <- imager::as.pixset(imager::add(components) > 0)
    px_bound <- imager::boundary(imager::grow(layer_px, 1))
    old_img <- array_to_cimg(constructImage(old_map,
                                            old_centers))
    highlight_img <- imager::colorise(old_img,
                     px_bound, col = highlight_color)
    highlight_img <- cimg_to_array(highlight_img)

    # plot highlighted
    graphics::layout(matrix(1:3, 1), widths = c(0.4, 0.4, 0.2))
    plotImageArray(highlight_img, main = paste("selected components"))
    plotImageArray(new_img, paste("result"))
    plotColorPalette(recolorize_obj$centers,
                     recolorize_obj$sizes,
                     horiz = FALSE)

  }

  # append the call
  recolorize_obj$call <- append(recolorize_obj$call, match.call())

  return(recolorize_obj)

}
