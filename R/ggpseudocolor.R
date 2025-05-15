#' Custom Stat for Binned KDE Pseudocolor Density
#'
#' Uses 2D kernel density estimation to assign discrete color bins to scatter points.
#' @keywords internal
#' @importFrom MASS kde2d
#' @importFrom fields interp.surface
StatPseudcolorBinned <- ggplot2::ggproto("StatPseudcolorBinned", ggplot2::Stat,
                                  required_aes = c("x", "y"),
                                  compute_group = function(data, scales, bins = 5,
                                                           n = 100, h = NULL) {
                                      if(length(is.na(h)) == 0){
                                          h=c(MASS::bandwidth.nrd(data$x),
                                              MASS::bandwidth.nrd(data$y))
                                      }
                                      cat(paste0("Bandwidth",c('x', 'y'),": ", h))
                                      dens <- MASS::kde2d(data$x, data$y, n = n, h = h)
                                      grid <- list(x = dens$x, y = dens$y, z = dens$z)
                                      dvals <- fields::interp.surface(grid, cbind(data$x, data$y))
                                      bins_cut <- cut(dvals, breaks = bins, labels = FALSE)
                                      data$density_bin <- factor(bins_cut, levels = 1:bins)
                                      dplyr::arrange(data, density_bin)
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
                                                           n = 100, h = NULL) {

                                      if(length(is.na(h)) == 0){
                                          h=c(MASS::bandwidth.nrd(data$x),
                                              MASS::bandwidth.nrd(data$y))
                                      }
                                      cat(paste0("Bandwidth",c('x', 'y'),": ", h))
                                      dens <- MASS::kde2d(data$x, data$y, n = n, h = h)
                                      grid <- list(x = dens$x, y = dens$y, z = dens$z)
                                      data$density <- fields::interp.surface(grid, cbind(data$x, data$y))
                                      dplyr::arrange(data, density)
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
                                        neighbour_dist = FNN::get.knn(data.frame(scale(as.numeric(data$x)),
                                                                                 scale(as.numeric(data$y))),
                                                                      k = k)
                                        data$mean_dist <- -log(rowMeans(neighbour_dist$nn.dist))
                                        dplyr::arrange(data, mean_dist)
                                    }
)

#' Custom Stat for number of nearest neighbours
#'
#' Calculate number of nearest neighbours within a specified radius
#'@importFrom FNN get.knn
#' @keywords internal
StatNNCount <- ggplot2::ggproto("StatNNCount", ggplot2::Stat,
                           required_aes = c("x", "y"),

                           compute_group = function(data, scales, r, k) {

                               x_weight = 1#MASS::bandwidth.nrd(data$y)
                               y_weight = 1#MASS::bandwidth.nrd(data$x)

                               neighbour_dist = FNN::get.knn(data.frame(scale(as.numeric(data$x)),
                                                                        scale(as.numeric(data$y))),
                                                             k = k)

                               if(length(is.na(r)) == 0){
                                   r =  quantile(neighbour_dist$nn.dist[,1], 0.75)

                                   cat(paste0("Cutoff Distance: ", r))
                               }

                               counts = apply(neighbour_dist$nn.dist, FUN=function(x){
                                   counts = sum(x < r)
                               }, MARGIN=1)
                               data$neighbourhood_count <- counts
                               dplyr::arrange(data, neighbourhood_count)

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
#' @param stat Four possible options:  \c
#'  pseudocolor: calculates densitites as a continuous spectrum, requires bins, n, hr
#'  pseudocolor_binned: groups densities into bins, requires bins, n, h \cr
#'  pseudocolor_nn: calculates euclidan distance of nearest neighbours, requires k \cr
#'  pseudocolor_nn_count: counts number nearest neighbours, requires k, r
#' @import ggplot2
#' @export
geom_pseudocolor <- function(mapping = NULL,
                             data = NULL,
                             stat = "pseudocolor",
                             position = "identity",
                             bins = 5,
                             n = 100,
                             h=NULL,
                             k=100,
                             r=NULL,
                             show.legend = T,
                             inherit.aes = TRUE,
                             ...) {
    # Create the layer and add the color scale

    if(stat == 'pseudocolor_binned'){
    layer <- ggplot2::layer(
        stat = StatPseudcolorBinned,
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
                                     ggplot2::aes(color = ggplot2::after_stat(mean_dist))),
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
                                 ggplot2::aes(color = ggplot2::after_stat(neighbourhood_count))),
            data = data,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(r = r, k=k, ...)
        )
    }

    list(
        layer
    )
}

#' Flowjo color palette
#'#'
#' @import grDevices
#' @keywords internal
flowjo_colors <- function(n = 256) {
    grDevices::colorRampPalette(c(
        "#0000FF",
        "#00FFFF",
        "#00FF00",
        "#FFFF00",
        "#FF0000"
    ))(n)
}

#' Continuous color palette for flowjo colors
#'
#' @import ggplot2
#' @export
scale_color_flowjo <- function(...) {
    ggplot2::scale_color_gradientn(colors = flowjo_colors(), ...)
}

#' Continuous color palette for flowjo colors
#'
#' @import ggplot2
#' @export
scale_color_flowjo_discrete <- function(...) {
    ggplot2::scale_color_manual(values = flowjo_colors(5), ...)
}

