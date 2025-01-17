% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_data.R
\name{visualize_data}
\alias{visualize_data}
\title{Visualize the data with ggplot}
\usage{
visualize_data(
  long_data,
  plot_type = c("boxplot", "density", "histogram"),
  facet = T
)
}
\arguments{
\item{long_data}{Long format data}

\item{plot_type}{A character string with for choosing the the visual demonstration.
The current options are "boxplot", "density", and "histogram".}

\item{facet}{A logical value. If true, the plot will be faceted with the facet column "variables".}
}
\value{
a ggplot object
}
\description{
Visualize data with simple graphical techniques such as boxplots, density plots, histograms.
}
\examples{
data(iris)
iris_m <- iris
colnames(iris_m) <- "variables"
visualize_data(iris_m)
}
