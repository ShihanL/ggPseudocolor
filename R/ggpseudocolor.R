flojo_colors <- function(n = 256) {
    colorRampPalette(c(
        "#000000",  # Black
        "#0000FF",  # Blue
        "#00FFFF",  # Cyan
        "#00FF00",  # Green
        "#FFFF00",  # Yellow
        "#FF0000",  # Red
    ))(n)
}

scale_color_flojo <- function(...) {
    ggplot2::scale_color_gradientn(colors = flojo_colors(), ...)
}


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
                                      cat(paste0("Bandwidth: ", h))
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
                                                           n = 100, h = NULL) {

                                      if(length(is.na(h)) == 0){
                                          h=c(MASS::bandwidth.nrd(data$x),
                                              MASS::bandwidth.nrd(data$y))
                                      }
                                      cat(paste0("Bandwidth: ", h))
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

                           compute_group = function(data, scales, r, k) {
                               neighbour_dist = FNN::get.knn(data.frame(as.numeric(data$x),
                                                                        as.numeric(data$y)),
                                                             k = k)

                               if(length(is.na(r)) == 0){
                                   r =  quantile(neighbour_dist$nn.dist[,1], 0.75)

                                   cat(paste0("Cutoff Distance: ", r))
                               }

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
            params = list(r = r, k=k, ...)
        )
    }

    list(
        layer
    )
}



