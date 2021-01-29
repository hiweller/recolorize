#' Absorb a layer into its surrounding color patches
#'
#' Every component of a layer which satisfies a size condition is 'absorbed' into
#' the color map by switching its color to that of the color patch with which
#' it shares the longest border. Useful for dealing with reflections, gloss,
#' speckles, small features, etc.
#'
#' @param recolorize_obj A `recolorize` object.
#' @param layer_idx The numeric index of the layer to absorb.
#' @param size_condition A condition for determining which components to absorb,
#' written as a function. The default (`function(x) x <= 100`) will only
#' affect components that are 100 pixels or fewer in size.
#' @param highlight_color Color for highlighting the affected layer.
#' @param plotting Logical. Plot results?
#'
#' @return A `recolorize` object.
#'
#' @details This function works by splitting a layer into spatially distinct
#' 'components' using [imager::split_connected]. A contiguous region of pixels
#' is considered a single component. Any components whose size satisfies the condition
#' provided in the `size_condition` argument is then 'absorbed' by the surrounding
#' color patches. This works by counting up all of the pixels that border that
#' component, and switching the color of that component to the patch with which
#' it shares the longest border.
#'
#' @seealso [editLayers] for editing layers using morphological operations;
#' [thresholdRecolor] for re-fitting the entire image without minor colors.
#'
#' @examples
#' img <- system.file("extdata/ephippigera.png", package = "recolorize"); img
#' fit1 <- recolorize(img, bins = 3)
#' fit2 <- recluster(fit1)
#' fit3 <- absorbLayer(recolorize_obj = fit3, layer_idx = 4,
#'                    size_condition = function(x) x <= 30 & x > 0,
#'                    highlight_color = "cyan")
#'
#'
#' @export
absorbLayer <- function(recolorize_obj,
                          layer_idx,
                          size_condition = function(x) x <= 100,
                          highlight_color = "yellow",
                          lwd = 0.25,
                          plotting = TRUE) {
  # get object layer
  layers <- splitByColor(recolorize_obj, plot_method = "none")
  layer <- layers[[layer_idx]]

  # convert to an imager pixset for splitting
  px <- imager::as.pixset(imager::as.cimg(layer) > 0)
  px <- imager::imrotate(px, 90)

  # split separate components
  layer_split <- imager::split_connected(px)

  # get component sizes
  component_sizes <- unlist(lapply(layer_split, sum))

  # find which components satisfy the condition
  condition_met <- which(ifelse(size_condition(component_sizes), TRUE, FALSE))

  # make a color center map from the pixel assignments (this will make sense in
  # a bit)
  map <- imager::as.cimg(recolorize_obj$pixel_assignments)
  map <- imager::imrotate(map, 90)

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
  map2 <- recolorize:::cimg_to_array(imager::imrotate(map, -90))
  recolorize_obj$pixel_assignments <- map2

  # and make the image
  new_img <- constructImage(map2,
                            recolorize_obj$centers)

  # plot if we're plotting
  if (plotting) {

    # get boundaries of original pixels & make highlighted array
    components <- layer_split[condition_met]
    layer_px <- imager::as.pixset(imager::add(components) > 0)
    px_bound <- imager::boundary(imager::grow(layer_px, 1))
    old_img <- array_to_cimg(recolorize_obj$recolored_img)
    highlight_img <- imager::colorise(imager::imrotate(old_img, 90),
                     px_bound, col = highlight_color)
    highlight_img <- cimg_to_array(imager::imrotate(highlight_img, -90))

    # plot highlighted
    layout(matrix(1:3, 1), widths = c(0.4, 0.4, 0.2))
    plotImageArray(highlight_img, main = paste("selected components"))
    plotImageArray(new_img, paste("result"))
    plotColorPalette(recolorize_obj$centers,
                     recolorize_obj$sizes,
                     horiz = FALSE)

  }

  # tidying up

  # first, swap out the new image
  recolorize_obj$recolored_img <- new_img

  # then, get new sizes (minus background)
  recolorize_obj$sizes <- table(map2)[-1]

  # if we completely eliminated a patch, remove it from the color centers
  if (!any(map2 == layer_idx)) {
    recolorize_obj$centers <- recolorize_obj$centers[-layer_idx, ]
  }
  return(recolorize_obj)

}
