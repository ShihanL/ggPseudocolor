#' Custom Stat for Binned KDE Pseudocolor Density
#'
#' Uses 2D kernel density estimation to assign discrete color bins to scatter points.
#' @keywords internal
#' @importFrom MASS kde2d
#' @importFrom fields interp.surface
StatPseudcolorBinned <- ggplot2::ggproto("StatPseudcolorBinned", ggplot2::Stat,
                                  required_aes = c("x", "y"),
                                  compute_group = function(data, scales, bins = 5,
                                                           n = 100, h = NULL, adjust=1) {
                                      if(length(is.na(h)) == 0){
                                          h=c(KernSmooth::dpik(data$x),
                                              KernSmooth::dpik(data$y))
                                          if(0 %in% h){
                                              h=ifelse(max(h) != 0, max(h), 1)
                                          }
                                      }

                                      h= adjust * h


                                      if(length(is.na(n)) == 0 | length(n) != 2){

                                          n=c(100,100)

                                      }
                                      #cat(paste0("Bandwidth",c('x', 'y'),": ", h))

                                      dens <- KernSmooth::bkde2D(data.frame(data$x, data$y),
                                                                 bandwidth = h,
                                                                 gridsize = n)
                                      grid <- list(x = dens$x1, y = dens$x2, z = dens$fhat)
                                      dvals <- fields::interp.surface(grid, cbind(data$x, data$y))


                                      #bins_cut <- cut(dvals, breaks = bins, labels = FALSE)
                                      bins_cut <- Hmisc::cut2(dvals, cuts = quantile(dvals))

                                      data$density_bin <- factor(bins_cut, levels = 1:bins)
                                      dplyr::arrange(data, density_bin)
                                  }
)

#' Custom Stat for Continuous KDE Pseudocolor Density
#'
#' Uses 2D kernel density estimation to assign continuous color to scatter points.
#' @keywords internal
#' @importFrom KernSmooth bkde2D
#' @importFrom fields interp.surface
StatPseudocolor <- ggplot2::ggproto("StatPseudocolor", ggplot2::Stat,
                                  required_aes = c("x", "y"),

                                  compute_group = function(data, scales, bins = 5,
                                                           n = NULL, h = NULL, adjust=1) {

                                      if(length(is.na(h)) == 0){
                                          h=c(KernSmooth::dpik(data$x),
                                              KernSmooth::dpik(data$y))
                                          if(0 %in% h){
                                              h=ifelse(max(h) != 0, max(h), 1)
                                          }
                                      }
                                      h=adjust * h



                                      if(length(is.na(n)) == 0 | length(n) != 2){

                                          n=c(100,100)

                                      }
                                      #cat(paste0("Bandwidth",c('_x', '_y'),": ", h, '\n', collapse = ''))
                                      dens <- KernSmooth::bkde2D(data.frame(data$x, data$y),
                                                                 bandwidth = h,
                                                                 gridsize = n)
                                      grid <- list(x = dens$x1, y = dens$x2, z = dens$fhat)
                                      data$density <- fields::interp.surface(grid, cbind(data$x, data$y))
                                      data
                                      #dplyr::arrange(data, density)
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

                               # sets arbitrary disance threshold
                               if(length(is.na(r)) == 0){
                                   r =  quantile(neighbour_dist$nn.dist[,1], 0.75)

                                   cat(paste0("No Cutoff Set,\nDefault Cutoff Distance: ", r))
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
#'  kde: calculates densitites as a continuous spectrum, requires bins, n, hr
#'  pseudocolor_binned: groups densities into bins, requires bins, n, h \cr
#'  pseudocolor_nn: calculates euclidan distance of nearest neighbours, requires k \cr
#'  pseudocolor_count: counts number nearest neighbours, requires k, r
#' @import ggplot2
#' @importFrom utils modifyList
#' @export
geom_pseudocolor <- function(mapping = NULL,
                             data = NULL,
                             stat = "pseudocolor",
                             position = "identity",
                             bins = 5,
                             n = NULL,
                             h= NULL,
                             k= 100,
                             r= NULL,
                             show.legend = T,
                             inherit.aes = TRUE,
                             adjust=1,
                             ...) {
    # Create the layer and add the color scale

    if(stat == 'pseudocolor_binned'){
    layer <- ggplot2::layer(
        stat = StatPseudcolorBinned,
        geom = ggplot2::GeomPoint,
        mapping = utils::modifyList(mapping %||% ggplot2::aes(),
                             ggplot2::aes(color = ggplot2::after_stat(density_bin))),
        data = data,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(bins = bins, n = n, h = h, adjust=adjust, ...)
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
            params = list(bins = bins, n = n, h = h,adjust=adjust, ...)
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

#' Discrete color palette for flowjo colors
#'
#' @import ggplot2
#' @export
scale_color_flowjo_discrete <- function(...) {
    ggplot2::discrete_scale(
        "colour",
        "flowjo",
        palette = function(n) flowjo_colors(n),
        ...
    )
}
