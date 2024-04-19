#' @name dum_box
#' 
#' @description Create a box for a ggplot CA-like plot
#'
#' @param gplot A ggplot.
#' @param lbox A list of vector with xim, xmax, ymin and ymax.
#' @param label.box Add the number of the box in the ggplot.
#' @param label.box.hjust,label.box.vjust Adjust the label placement.
#' @param color The color of the box.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list of ggplot.
#'
#' @examples
#' 
#' 
#' @export
dum_box <- function(gplot = NA,
                    lbox = NA,
                    label.box = TRUE,
                    label.box.hjust = -.5,
                    label.box.vjust = 1.5,
                    color = "black",
                    size = .3,
                    verbose = TRUE){
  gout <- gplot
  for (i in seq_along(lbox)) {
    local({
      if(verbose){
        print(paste0("Read the coordinates of the boxes"))
      }
      xmin <- lbox[[i]][[2]][1]
      xmax <- lbox[[i]][[2]][2]
      ymin <- lbox[[i]][[2]][3]
      ymax <- lbox[[i]][[2]][4]
      box_index <- i  # local copy of the loop index
      if (verbose) {
        print(paste0("   - draw box: ", box_index))
      }
      gout <<- gout +
        ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                           color = color, linewidth = size, fill = NA)
      if (label.box) {
        gout <<- gout +
          ggplot2::annotate("text", x = xmin, y = ymax, 
                            label = box_index, 
                            hjust = label.box.hjust, 
                            vjust = label.box.vjust)
      }
    })
  }
  return(gout)
}