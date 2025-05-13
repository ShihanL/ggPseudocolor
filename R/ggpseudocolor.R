#' Custom Stat for Binned KDE Pseudocolor Density
#'
#' Uses 2D kernel density estimation to assign discrete color bins to scatter points.
#' @keywords internal
#' @importFrom MASS kde2d
#' @importFrom fields interp.surface
StatPseudcolorBinned <- ggplot2::ggproto("StatPseudocolorBinned", ggplot2::Stat,
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

#' Custom Stat for Continuous KDE Pseudocolor Density
#'
#' Uses 2D kernel density estimation to assign continuous color to scatter points.
#' @keywords internal
#' @importFrom MASS kde2d
#' @importFrom fields interp.surface
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




#' Stat for nearest neighbour distance Pseudocolor Density
#'
#' Calculate density based on mean Euclidean distance of nearest neighbours
#' @importFrom FNN get.knn
#' @keywords internal
StatNN <- ggplot2::ggproto("StatNN", ggplot2::Stat,
                                    required_aes = c("x", "y"),

                                    compute_group = function(data, scales, k) {
                                        neighbour_dist = FNN::get.knn(data.frame(as.numeric(data$x),
                                                                                 as.numeric(data$y)),
                                                                      k = k)
                                        data$density <- -log(rowMeans(neighbour_dist$nn.dist))
                                        data
                                    }
)

#' Custom Stat for number of nearest neighbours
#'
#' Calculate number of nearest neighbours within a specified radius
#'@importFrom FNN get.knn
#' @keywords internal
StatNNCount <- ggplot2::ggproto("StatNNCount", ggplot2::Stat,
                           required_aes = c("x", "y"),

                           compute_group = function(data, scales, r) {
                               neighbour_dist = FNN::get.knn(data.frame(as.numeric(data$x),
                                                                        as.numeric(data$y)),
                                                             k = length(data$x) * 0.2)
                               counts = apply(neighbour_dist$nn.dist, FUN=function(x){
                                   counts = table(x < r)
                                   ifelse(is.na(counts['TRUE']), 0, counts['TRUE'])


                               }, MARGIN=1)
                               data$density <- counts
                               data
                           }
)

#' geom_pseudocolor
#'
#' Creates pseudocolor point plot with specified stat calculation
#'
#' @rdname geom_pseudocolor
#' @param bins Number of bins to split densitities into
#' @param n Resolution of the KDE grid
#' @param h KDE bandwidth
#' @param k Number of nearest neighbours
#' @param r Maximum distance to classify as nearest neighbour
#' @param stat Four possible options:
#' pseudocolor: calculates densitites as a continuous spectrum, requires bins, n, h
#' pseudocolor_binned: groups densities into bins, requires bins, n, h
#' pseudocolor_nn: calculates euclidan distance of nearest neighbours, requires k
#' pseudocolor_nn_count: counts number nearest neighbours, requires r
#' @import ggplot2
#' @export
geom_pseudocolor <- function(mapping = NULL,
                             data = NULL,
                             stat = "pseudocolor",
                             position = "identity",
                             bins = 5,
                             n = 100,
                             h = 1,
                             k=100,
                             r=0.5,
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
            geom = ggplot2::GeomPoint,
            mapping = modifyList(mapping %||% ggplot2::aes(),
                                 ggplot2::aes(color = ggplot2::after_stat(density))),
            data = data,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(bins = bins, n = n, h = h, ...)
        )
        }

        if(stat == 'pseudocolor_nn'){
            layer <- ggplot2::layer(
                stat = StatNN,
                geom = ggplot2::GeomPoint,
                mapping = modifyList(mapping %||% ggplot2::aes(),
                                     ggplot2::aes(color = ggplot2::after_stat(density))),
                data = data,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(k = k, ...)
            )
        }

    if(stat == 'pseudocolor_nn_count'){
        layer <- ggplot2::layer(
            stat = StatNNCount,
            geom = ggplot2::GeomPoint,
            mapping = modifyList(mapping %||% ggplot2::aes(),
                                 ggplot2::aes(color = ggplot2::after_stat(density))),
            data = data,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(r = r, ...)
        )
    }

    list(
        layer
    )
}




df <- data.frame(x=c(rnorm(1000, mean = 0,sd =1 ),
                          rnorm(1000, mean = 5,sd =2 ),
                          sample(seq(-5,10,0.001), size = 1000, replace = T)),
                 y=c(rnorm(1000, mean = 0,sd =1 ),
                          rnorm(1000, mean = 2,sd =1 ),
                          sample(seq(-5,10, 0.001), size = 1000, replace = T)))


ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
    geom_pseudocolor(stat='pseudocolor') +
    theme_minimal() + scale_color_viridis_c()



