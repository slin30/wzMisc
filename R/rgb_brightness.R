#' Calculate the perceived brightness of an rgb color matrix
#'
#' @param col_mat (required) An rgb matrix; most typically, the output from \code{grDevices::col2rgb}
#' @param rgb_fx (optional) A named numeric vector of weights for each of \code{r,g,b}.
#'        If provided, must have names of \code{red, green, blue}.
#'
#' @details
#' Mainly useful to set a threshold for contrasting label purposes. Perceived brightness
#' is of course subjective, and there is no single definitive calculation approach
#' This function is not meant to be compliant with any standards or technical specifications.
#' It simply implements an approach using defaults as shown
#' \href{https://www.nbdtech.com/Blog/archive/2008/04/27/Calculating-the-Perceived-Brightness-of-a-Color.aspx}{here}.
#'
#' @return
#' A numeric vector of equal length to the input, with values ranging from \code{0} to \code{255}.
#' @export
#'
#' @examples
#' # contrast is subjective, so you will want to tweak the cutoff depending
#' # on audience, palette, presentation format, etc.
#' # this shows dark-light-dark as an example
#' gradient_fun <- colorRampPalette(c("black", "white", "black"), space = "rgb")
#'
#' # set matrix dimensions for plotting
#' n_col = 18
#' n_row = 20
#'
#' # make a vector of hex codes from gradient_fun, then calculate
#' # perceived brightness
#' hex_v <- gradient_fun(n_col * n_row)
#' brt_v <- rgb_brightness(col2rgb(hex_v))
#'
#' # cutoff to switch text color for contrast, using perceived brightness
#' # default black, switch to white text if < cutoff
#' cutoff <- 140
#' txt_v <- rep("#000000", n_col*n_row)
#' txt_v[brt_v < cutoff] <- "#ffffff"
#'
#' # calculate the position labels
#' pos_y <- rep(seq(0, 1, length.out = n_col), each = n_row)
#' pos_x <- rep(seq(0, 1, length.out = n_row), times = n_col)
#'
#' # plot
#' image(z = matrix(sort(brt_v), ncol = n_col, byrow = FALSE), col = hex_v,
#'       main = "Gradient matrix with contrasting label colors based on brightness")
#' text(pos_x, pos_y, labels = as.integer(brt_v), col = txt_v)
rgb_brightness <- function(col_mat, rgb_fx = NULL) {

  # col_mat must be a matrix with rownames as expected
  if(!is.matrix(col_mat)) {
    stop("col_mat must be a matrix; try calling col2rgb on input?")
  }

  mat_nms <- c("red", "green", "blue")
  if(!all(identical(rownames(col_mat), mat_nms))){
    stop("col_mat must have expected rownames of",
         paste(mat_nms, collapse = ","))
  }

  # only check rgb_fx if not using default
  if(is.null(rgb_fx)) {
    rgb_fx = c(red = 0.241, green = 0.691, blue = 0.068)
  } else {
    if(length(rgb_fx) != 3L || !all(identical(names(rgb_fx), mat_nms))) {
      stop("rgb_fx must be a length 3 vector with names of ",
           paste(mat_nms, collapse = ","))
    }
    if(!is.numeric(rgb_fx) || max(rgb_fx) > 1) {
      stop("rgb_fx must be numeric with a max value of 1")
    }
  }

  #basic formula is sqrt(fx_red * R^2 + fx_green * G^2 + fx_blue * B^2)
  mat <- as.matrix(rgb_fx)
  col_n <- ncol(col_mat)
  mat_x <- rep(mat, col_n)

  mat_sq <- col_mat^2
  res <- mat_sq * mat_x
  coll <- colSums(res)
  sqrt(coll)
}
