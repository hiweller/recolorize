#' Reorder colors in a recolorize object
#'
#' Often for batch processing purposes, it is important to ensure
#' that color centers fit using different methods are in the same
#' order.
#'
#' @param recolorize_obj An object of class `recolorize`.
#' @param col_order A numeric vector of the length of the number of color
#'   centers in the `recolorize` object specifying the order of the colors.
#' @param plotting Logical. Plot the results?
#'
#' @details While you can manually specify the `col_order` vector, one way to
#'   automatically order the colors according to an external color palette (as
#'   might be needed for batch processing) is to use the [match_colors] function,
#'   although it is recommended to double-check the results.
#'
#' @return A `recolorize` object.
#'
#' @examples
#' img <- system.file("extdata/corbetti.png", package = "recolorize")
#' rc <- recolorize2(img, cutoff = 45)
#' ref_palette <- c("mediumblue", "olivedrab", "tomato2", "beige", "grey10")
#' col_order <- match_colors(ref_palette, rc$centers, plotting = TRUE)
#' rc2 <- reorder_colors(rc, col_order, plotting = FALSE)
#'
#' # the colors are reordered, but not changed to match the reference palette:
#' plot(rc2)
#'
#' # you can also change them to the reference palette:
#' rc2$centers <- t(grDevices::col2rgb(ref_palette) / 255)
#' plot(rc2)
#' @export
reorder_colors <- function(recolorize_obj,
                           col_order,
                           plotting = FALSE) {

  rc <- recolorize_obj
  pa <- rc$pixel_assignments

  for (i in 1:length(col_order)) {
    pa[which(rc$pixel_assignments == i)] <- which(col_order == i)
  }

  rc$centers <- rc$centers[col_order, ]
  rc$pixel_assignments <- pa
  if (plotting) {plot(rc)}
  return(rc)
}

#' Reorder a color palette to best match a reference palette
#'
#' Often for batch processing purposes, it is important to ensure
#' that color centers fit using different methods are in the same
#' order. This function reorders a provided color palette (`match_palette`)
#' according a provided reference palette (`reference_palette`) by minimizing
#' their overall distance using the
#' \href{https://en.wikipedia.org/wiki/Hungarian_algorithm}{Hungarian algorithm}
#' as implemented by [clue::solve_LSAP].
#'
#' If the color palettes are wildly different, the returned order may not be
#' especially meaningful.
#'
#' @param reference_palette The palette whose order to match. Either a character
#'   vector of colors (hex codes or color names) or an nx3 matrix in **sRGB
#'   color space**.
#' @param match_palette The palette to reorder, same formats as
#'   `reference_palette`
#' @param plotting Logical. Plot the ordered palettes?
#'
#' @return A vector of color orders for `match_palette`.
#' @examples
#' ref_palette <- c("mediumblue", "olivedrab", "tomato2", "beige", "chocolate4")
#' match_palette <- c("#362C34", "#E4D3A9", "#AA4E47", "#809C35", "#49468E")
#' match_colors(ref_palette, match_palette, plotting = TRUE)
#'
#' @seealso [reorder_colors]
#' @export
match_colors <- function(reference_palette,
                         match_palette,
                         plotting = FALSE) {

  pal_list <- list(ref = reference_palette,
                   mat = match_palette)

  # first assert palette types:
  is_color <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(grDevices::col2rgb(X)),
               error = function(e) FALSE)
    })
  }

  # if it's hex codes/color names, convert to a color matrix
  for (i in 1:length(pal_list)) {
    if(all(is.character(pal_list[[i]])) & all(is_color(pal_list[[i]]))) {
      pal_list[[i]] <- t(grDevices::col2rgb(pal_list[[i]]) / 255)
    }
  }

  # throw an error if they're not formatted correctly:
  if(any(unlist(lapply(pal_list, \(x) ncol(x) != 3)))) {
    stop("Color palettes must be a vector of hex codes, built-in color names,
         or an n x 3 numeric matrix of RGB values.")
  }

  # make sure they have the same range:
  lapply(pal_list, range)
  if (max(unlist(lapply(pal_list, max))) <= 1) {
    box <- c(0, 1)
  } else {
    box <- c(0, 255)
  }

  # Use pp3 function from spatstat.geom package to create 3D point pattern objects
  # for use with solve_LSAP
  ref_pp3 <- spatstat.geom::pp3(pal_list$ref[, 1],
                             pal_list$ref[, 2],
                             pal_list$ref[, 3],
                             spatstat.geom::box3(box))

  # Order each subsequent dataframe by the clusters in the first element
  mat_pp3 <- spatstat.geom::pp3(pal_list$mat[, 1],
                                pal_list$mat[, 2],
                                pal_list$mat[, 3],
                               spatstat.geom::box3(box))

  # Get distance matrix between the two
  dist.matrix <- spatstat.geom::crossdist(ref_pp3, mat_pp3)

  # Use solve_LSAP from clue package to get the minimum sum of distances
  # between point pairs (implements Hungarian Algorithm)
  orders <- clue::solve_LSAP(dist.matrix, maximum = FALSE)

  if (plotting) {
    # reset graphical parameters when function exits:
    current_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(current_par))

    graphics::layout(matrix(1:2, nrow = 2))
    graphics::par(mar = rep(1, 4))
    plotColorPalette(pal_list$ref)
    plotColorPalette(pal_list$mat[orders, ])
  }

  # return orders
  return(orders)
}
