#' @title Visualize the data with ggplot
#' @description Visualize data with simple graphical techniques such as boxplots, density plots, histograms.
#'
#' @param long_data Long format data
#' @param plot_type A character string with for choosing the the visual demonstration.
#'                  The current options are "boxplot", "density", and "histogram".
#' @param facet A logical value. If true, the plot will be faceted with the facet column "variables".
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' data(iris)
#' iris_m <- iris
#' colnames(iris_m) <- "variables"
#' visualize_data(iris_m)
visualize_data <- function(long_data,
                           plot_type = c("boxplot", "density", "histogram"),
                           facet = T){
  # 1. reading data
  p <- ggplot(long_data)

  # 2. plot type
  if(missing(plot_type)){
    plot_type <- "boxplot"
  }

  if(plot_type == "boxplot"){
    p <- p + geom_boxplot(aes(y = values))
  }

  if(plot_type == "density"){
    p <- p + geom_density(aes(x = values))
  }

  if(plot_type == "histogram"){
    p <- p + geom_histogram(aes(x = values))
  }

  # 3. Other features
  if(isTRUE(facet)){
    p <- p + facet_wrap(~variables, scales = "free")
  }
  p
}
