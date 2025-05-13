#' Custom Stat for Binned Pseudocolor Density
#'
#' Uses 2D kernel density estimation to assign discrete color bins to scatter points.
#' @keywords internal
StatPseudocolorBinned <- ggplot2::ggproto("StatPseudocolorBinned", ggplot2::Stat,
                                  required_aes = c("x", "y"),
                                  compute_group = function(data, scales, bins = 5,
                                                           n = 100, h = 1) {
                                      dens <- MASS::kde2d(data$x, data$y, n = n, h = h)
                                      grid <- list(x = dens$x, y = dens$y, z = dens$z)
                                      dvals <- fields::interp.surface(grid, cbind(data$x, data$y))
                                      bins_cut <- cut(dvals, breaks = bins, labels = FALSE)
                                      data$density_bin <- factor(bins_cut, levels = 1:bins)
                                      data
                                  }
)

#' Custom Stat for Binned Pseudocolor Density
#'
#' Uses 2D kernel density estimation to assign continous color to scatter points.
#' @keywords internal
StatPseudocolor <- ggplot2::ggproto("StatPseudocolor", ggplot2::Stat,
                                  required_aes = c("x", "y"),

                                  compute_group = function(data, scales, bins = 5,
                                                           n = 100, h = 1) {
                                      dens <- MASS::kde2d(data$x, data$y, n = n, h = h)
                                      grid <- list(x = dens$x, y = dens$y, z = dens$z)
                                      data$density <- fields::interp.surface(grid, cbind(data$x, data$y))
                                      data
                                  }
)

#' @rdname geom_pseudocolor
#' @param bins Number of bins to split densitities into
#' @param n Resolution of the KDE grid
#' @param h KDE bandwidth
#' @param stat Two possible options: pseudocolor which plots densitites as a continuous spectrum and
#' pseudocolor_binned which groups densities into n bins
#' @export
geom_pseudocolor <- function(mapping = NULL, data = NULL,
                                     stat = "pseudocolor",
                                     position = "identity",
                                     bins = 5, n = 100, h = 1,
                                     show.legend = T,
                                     inherit.aes = TRUE,
                                     ...) {
    # Create the layer and add the color scale

    if(stat == 'pseudocolor_binned'){
    layer <- ggplot2::layer(
        stat = StatPseudocolorBinned,
        geom = ggplot2::GeomPoint,
        mapping = modifyList(mapping %||% ggplot2::aes(),
                             ggplot2::aes(color = ggplot2::after_stat(density_bin))),
        data = data,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(bins = bins, n = n, h = h, ...)
    )
    }

    if(stat == 'pseudocolor'){
        layer <- ggplot2::layer(
            stat = StatPseudocolor,
            geom = GeomPoint,
            mapping = modifyList(mapping %||% ggplot2::aes(),
                                 ggplot2::aes(color = ggplot2::after_stat(density))),
            data = data,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(bins = bins, n = n, h = h, ...)
        )


    }

    list(
        layer
    )
}


#df <- data.frame(x=append(rnorm(5000, mean = 0,sd =1 ), rnorm(5000, mean = 5,sd =1 )),
#                 y=append(rnorm(5000, mean = 0,sd =1 ), rnorm(5000, mean = 5,sd =1 )))

#ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
#   geom_pseudocolor(n = 100, h = 1, size=0.5, stat='pseudocolor_binned') +
#    theme_minimal()
