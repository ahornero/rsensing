% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsensing.R
\name{extractRasterLayer}
\alias{extractRasterLayer}
\title{extractRasterLayer}
\usage{
extractRasterLayer(raster, shape, multicore = TRUE, gsize = 100,
  showMessages = TRUE, fun = stats2perform, as.shape = FALSE)
}
\arguments{
\item{raster}{Raster. Raster to be extracted}

\item{shape}{Shapefile. Shape which define the polygon to be extracted}

\item{multicore}{Optional. Logical. If TRUE means multicore processing. By default is TRUE}

\item{gsize}{Optional. Numeric. Size of each cell in meters. 100 m by default}

\item{showMessages}{Optional. Logical. If TRUE all processing messages will be shown}

\item{fun}{Optional. Function. Function to summarize the values (e.g. mean). By default \link{stats2perform} is used.}

\item{as.shape}{Optional. Logical. If as.shape=TRUE, it returns a shape instead of a data.frame. By default is FALSE}
}
\value{
a data.frame or a Spatial object.
}
\description{
\code{extractRasterLayer} extracts data from a raster layer with a given polygon shape. It support multicore processing
through the \link{parallel} package and clustering with a default cellsize of 100 meters.
}
\examples{
\dontrun{
shape.extracted <- extractRasterLayer(raster, shape, as.shape = T)
}
}
