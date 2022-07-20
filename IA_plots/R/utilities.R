#' Rotate the x axis text
#'
#' @param angle numeric value specifying the rotation angle. Default is 90 for vertical x-axis text.
#' @param hjust horizontal adjustments
#' @param vjust vertical adjustments
#' @param ... other arguments to pass to the function element_text().
#'
#' @return
#' @export
#'
#' @examples add it to a ggplot.
IA_rotate_x_text <- function (angle = 90, hjust = NULL, vjust = NULL, ...)
{
  if (missing(hjust) & angle > 5)
    hjust <- 1
  if (missing(vjust) & angle == 90)
    vjust <- 0.5
  theme(axis.text.x = element_text(angle = angle, hjust = hjust,
                                   vjust = vjust, ...))
}



#' remove the y axis.
#'
#' the line, ticks, text and title. just add it to a ggplot function as the last theme adjustment.
#'
#' @return
#' @export
#'
#' @examples ggplot(mpg, aes(x = displ, y = cty, color = drv)) +
#' geom_point() + IA_remove_y()

IA_remove_y <- function(){
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())
}

