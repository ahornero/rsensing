#' read.shape
#'
#' \code{read.shape} is an easier alternative to \link{readOGR}.
#'
#' This is an alternative function to \link{readOGR}, which pretends to be the easiest way
#' to load shapefiles.
#'
#' @param shape.path the path of the shapefile.
#'
#' @import rgdal
#' @export
#'
#' @examples
#' read.shape('./myshapefile.shp')
#'
read.shape <- function(shape.path) {
  rgdal::readOGR(dsn = dirname(shape.path), layer = sub('\\..*$', '', basename(shape.path)), verbose = F)
}

#' write.shape
#'
#' \code{write.shape} is an easier alternative to \link{writeOGR}.
#'
#' This is an alternative function to \link{writeOGR}, which pretends to be the easiest way
#' to write shapefiles.
#'
#' @param shape the spatial object to be saved.
#' @param shape.path the path where it should be written.
#'
#' @import rgdal
#' @export
#'
#' @examples
#' write.shape(shapeObject, './myshapefile.shp')
#'
write.shape <- function(shape, shape.path) {
  rgdal::writeOGR(obj = shape, dsn = dirname(shape.path), layer = sub('\\..*$', '', basename(shape.path)), driver = "ESRI Shapefile")
}

#' rasterToPolygons0
#'
#' \code{rasterToPolygons0} is a faster alternative to \link{rasterToPolygons}.
#'
#' This is an alternative function to \link{rasterToPolygons}, which pretends to be
#' the fastest way to perform this task. Original idea from \code{gdal_polygonizeR()}
#' It also offers a directly way to create a mask.
#'
#' It has a dependency on GDAL Tools to be installed
#'
#' @param raster the raster object to be converted.
#' @param value2keep the path where it should be written.
#'
#' @return a spatial object
#'
#' @import maptools raster sp
#' @export
#'
#' @examples
#' shape <- rasterToPolygons0(rasterObject)
#' shape <- rasterToPolygons0(rasterObject, 255)
#'
rasterToPolygons0 <- function(raster, value2keep = '') {
  if ( !is(raster, "Raster")) stop('Provided raster is not a raster')
  shape_path <- tempfile()
  #gdal_polygonize_path <- Sys.which('gdal_polygonize.py')
  gdal_polygonize_path <- '/Library/Frameworks/GDAL.framework/Programs/gdal_polygonize.py'
  system2('python', args = (sprintf('"%1$s" "%2$s" -f "ESRI Shapefile" "%3$s.shp"', gdal_polygonize_path, raster@file@name, shape_path)))

  shape <- maptools::readShapeSpatial(shape_path)
  sp::proj4string(shape) <- raster::projection(raster)

  if (value2keep != '' & is.numeric(value2keep) & round(value2keep) == value2keep) {
    shape <- shape[shape$DN == value2keep,]
    shape$DN <- NULL
    shape$ID <- c(1:length(shape))
  }

  unlink(shape_path)
  return(shape)
}

#' toGeoTIFF
#'
#' \code{toGeoTIFF} is a tiny \code{gdal_translate} wrapper.
#'
#' The aim is just to convert the given image into a GeoTIFF image.
#'
#' It has a dependency on GDAL Tools to be installed
#'
#' @param input.path the raster object to be converted.
#' @param output.path the path where it should be written.
#'
#' @export
#'
#' @examples
#' toGeoTIFF('./image.JPG', './outputimage.tif')
#'
toGeoTIFF <- function(input.path, output.path) {
  system2('gdal_translate.exe', args = (sprintf('"%1$s" "%2$s"', input.path, output.path)), stdout = F)
}

# devtools::use_package("rgdal")
# devtools::use_package('maptools')
# devtools::use_package('sp')
# devtools::use_package('raster')


#roxygen2::roxygenise()
# devtools::document()

