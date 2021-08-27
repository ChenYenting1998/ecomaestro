#' @title Plot the PCA result with ggplot
#' @description  Plots the PCA result from a `rda` object in package `vegan` with `ggplot2`. Note that the terminology in the descriptions follow that in `vegan.`
#'
#' @param rda_object The `rda` object calculated by `vegan`.
#' @param scaling The scaling of the PCA result. In breif, scaling = 1 illustrates the distance between sites. scaling = 2 illustrates the correlation between species.
#' @param sites_addins The factored information for sites.
#' @param species_addins The factored information for species.
#' @param sites_label The column in the `sites_addins` for showing the labels of the sites.
#' @param sites_color The column in the `sites_addins` for changing the colors of sites
#' @param species_color A character with the length of 1 to change the coloring of species.
#'
#' @return A ggplot object. Any further `theme` manipulations can be directly added following
#'         the ggplot grammar.
#' @export
#'
#' @examples
#' data(iris)
#' values <- iris[-5] # extract numerical values from `iris`
#' normalized <- scale(values) # normalize the values
#' pca_iris <- rda(normalized) # returns a "rda" object
#' plot_PCA(pca_iris, sites_addins = iris[5], sites_label = "Species", sites_color = "Species")
plot_PCA <- function(rda_object, scaling = 1,
                     sites_addins = NULL, species_addins = NULL,
                     sites_label, sites_color,
                     species_color) {

  # scaling default = 1

  # 1. libraries --------------------------------------------------------------
  # library(vegan)
  # library(dplyr)
  # library(tidyr)

  if (missing(rda_object) | !(class(rda_object)[class(rda_object) %in% "rda"] == "rda")) {
    message("input is not a rda_object from the package `vegan`")
    stop()
  }

  # 2a. extract site scores --------------------------------------------------
  sites <-
    scores(rda_object, scaling = scaling)$sites %>%
    data.frame()

  if (!is.null(sites_addins) && !is.data.frame(sites_addins)) {
    message("sites_addins is not a data.frame")
    stop()
  } else {
    sites <-
      cbind(sites, sites_addins)
  }


  # 2b. extract species scores ----------------------------------------------
  species <-
    scores(rda_object, scaling = scaling)$species %>%
    data.frame() %>%
    mutate(variables = rownames(scores(rda_object, scaling = scaling)$species))

  if (!is.null(species_addins)) {
    if (!is.data.frame(species_addins)) {
      message("sites_addins is not a data.frame")
      stop()
    } else {
      species <-
        cbind(species, species_addins)
    }
  }

  # extract variation
  variation <- rda_object$CA$eig / sum(rda_object$CA$eig)
  variation <- round(variation, digits = 4) * 100


  # 3. ggplot ---------------------------------------------------------------
  p <- ggplot() +
    geom_blank()

  # species_arrow
  p <- p +
    geom_segment(
      data = species,
      aes(x = PC1, y = PC2, xend = 0, yend = 0),
      size = 1,
      alpha = 0.6,
      arrow = arrow(ends = "first", angle = 20, length = unit(0.03, "npc")),
      color = "blue"
    )

  # species_text
  if (!is.null(species_addins)) {
    p <- p +
      geom_label(
        data = species,
        aes_string(x = "PC1", y = "PC2", label = "variables", color = species_color)
      )
  } else {
    p <- p +
      geom_label(
        data = species,
        aes_string(x = "PC1", y = "PC2", label = "variables"),
        color = "black"
      )
  }

  # sites_text
  if (!is.null(sites_addins)) {
    p <- p +
      geom_label(
        data = sites,
        aes_string(x = "PC1", y = "PC2", label = sites_label, color = sites_color)
      )
  } else {
    p <- p +
      geom_label(
        data = sites,
        aes_string(x = "PC1", y = "PC2", label = rownames(sites_addins))
      )
  }

  # axis titles
  p <- p +
    xlab(paste("PC1 ", "(", variation[1], "% explained)", sep = "")) +
    ylab(paste("PC2 ", "(", variation[2], "% explained)", sep = ""))

  # output
  p + coord_fixed()
}
