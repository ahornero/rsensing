% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsensing.R
\name{toGeoTIFF}
\alias{toGeoTIFF}
\title{toGeoTIFF}
\usage{
toGeoTIFF(input.path, output.path)
}
\arguments{
\item{input.path}{Character. The raster object path to be converted.}

\item{output.path}{Character. The path where it should be written.}
}
\description{
\code{toGeoTIFF} is a tiny \code{gdal_translate} wrapper.
}
\details{
The aim is just to convert the given image into a GeoTIFF image.

It has a dependency on GDAL Tools to be installed
}
\examples{
toGeoTIFF('./image.JPG', './outputimage.tif')

}
