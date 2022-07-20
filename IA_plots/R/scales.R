# basic colors ----
IA_colors <- c(
  "black" = "black",
  "gray1" = "gray30",
  "blue1" = "dodgerblue3",
  "pink1" = "#FBD3C6",
  "gray2" = "#D9D9D9",
  "blue2" = "deepskyblue",
  "pink2" = "#FCE0D7",
  "pink3" = "#E2B8AC"
)

#' Function to extract IA_colors as hex codes
#'
#'
#' returns the vector colors defined in the palette.
#'
#' currently there are 8 colors.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples IA_cols()[1] is the black color. IA_cols()[2] is the gray.
IA_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (IA_colors)

  IA_colors[cols]
}


# color pallets ----
IA_palettes <- list(
  "DP" = IA_cols("black", "pink1"),
  "DB" = IA_cols("black","blue1" ),
  "DG" = IA_cols("black","gray1" ),
  "GB" = IA_cols("gray1","blue1" ),
  "GP" = IA_cols("gray1","pink1" ),
  "BP" = IA_cols("blue1","pink1" ),

  "DBP"= IA_cols("black", "blue1", "pink1"),
  "DGP" = IA_cols("black", "gray1", "pink1"),
  "DGB"= IA_cols("black", "gray1", "blue1"),
  "GBP"= IA_cols("gray1", "blue1", "pink1"),
  "c4"= IA_cols("black", "gray1", "blue1", "pink1"),
  "c5"= IA_cols("black", "gray1", "blue1", "pink1", "pink3"),
  "c8" = IA_cols() )


# function to interpolate a IA_color palette ----
IA_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- IA_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


#' Scale color and fill
#'
#'
#' Add scale_color or scale_fill to a ggplot object with IA color palettes of choice
#'
#'
#'
#' @param palette   chose palette from a list of options.
#' currently there are 13 variants. the colors are marked with capital letters.
#' "D" - Dark (black), "B" Blue, "G" gray and P pink.
#' the options are: DP, DB, DG, GB, GP, BP,
#' DBP, DGP, DGB, GBP, c4, c5, c8.
#' @param discrete  specify if the color/fill variable is discrete or continoues
#' @param reverse  reverse colors of the palette.
#' @param ... pass other arguments to the  scale parameters such as "guide = "none""
#' or "position = "left",see ggplot discrete_scale for more options.
#' @return
#' @examples just add scale_color_IA at any position inside a ggplot command:
#'
#'  ggplot(mpg, aes(x = displ, y = cty, color = drv)) +
#'  geom_point(size = 4) +
#'  IA_scale_color(palette = "GBP", reverse = T)

#' @name Scale_Color_and_Fill
NULL

#'
#' @rdname Scale_Color_and_Fill
#' @export
IA_scale_color <- function(palette = "DBP", discrete = TRUE, reverse = FALSE, ...) {
  pal <- IA_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("IA_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#'
#'
#'
#' @rdname Scale_Color_and_Fill
#' @export
IA_scale_fill <- function(palette = "DBP", discrete = TRUE, reverse = FALSE, ...) {
  pal <- IA_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("IA_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


