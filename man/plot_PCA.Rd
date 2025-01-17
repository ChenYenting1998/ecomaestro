% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_PCA.R
\name{plot_PCA}
\alias{plot_PCA}
\title{Plot the PCA result with ggplot}
\usage{
plot_PCA(
  rda_object,
  scaling = 1,
  sites_addins = NULL,
  species_addins = NULL,
  sites_label,
  sites_color,
  species_color
)
}
\arguments{
\item{rda_object}{The `rda` object calculated by `vegan`.}

\item{scaling}{The scaling of the PCA result. In breif, scaling = 1 illustrates the distance between sites. scaling = 2 illustrates the correlation between species.}

\item{sites_addins}{The factored information for sites.}

\item{species_addins}{The factored information for species.}

\item{sites_label}{The column in the `sites_addins` for showing the labels of the sites.}

\item{sites_color}{The column in the `sites_addins` for changing the colors of sites}

\item{species_color}{A character with the length of 1 to change the coloring of species.}
}
\value{
A ggplot object. Any further `theme` manipulations can be directly added following
        the ggplot grammar.
}
\description{
Plots the PCA result from a `rda` object in package `vegan` with `ggplot2`. Note that the terminology in the descriptions follow that in `vegan.`
}
\examples{
data(iris)
values <- iris[-5] # extract numerical values from `iris`
normalized <- scale(values) # normalize the values
pca_iris <- rda(normalized) # returns a "rda" object
plot_PCA(pca_iris, sites_addins = iris[5], sites_label = "Species", sites_color = "Species")
}
