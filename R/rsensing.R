#' extractRasterLayer
#'
#' \code{extractRasterLayer} extracts data from a raster layer with a given polygon shape. It support multicore processing
#' through the \link{parallel} package and clustering with a default cellsize of 100 meters.
#'
#' @param raster Raster. Raster to be extracted
#' @param shape Shapefile. Shape which define the polygon to be extracted
#' @param multicore Optional. Logical. If TRUE means multicore processing. By default is TRUE
#' @param gsize Optional. Numeric. Size of each cell in meters. 100 m by default
#' @param showMessages Optional. Logical. If TRUE all processing messages will be shown
#' @param fun Optional. Function. Function to summarize the values (e.g. mean). By default \link{stats2perform} is used.
#' @param as.shape Optional. Logical. If as.shape=TRUE, it returns a shape instead of a data.frame. By default is FALSE
#'
#' @return a data.frame or a Spatial object.
#'
#' @import parallel raster sp
#' @export
#'
#' @examples
#' \dontrun{
#' shape.extracted <- extractRasterLayer(raster, shape, as.shape = T)
#' }
extractRasterLayer <- function(raster, shape, multicore = TRUE, gsize = 100, showMessages = TRUE, fun = stats2perform, as.shape = FALSE) {
  if (showMessages) cat('Generating grid ... ')
  grid <- shape2grid(shape, gsize)
  if (showMessages) cat(ncell(grid), ' cells\n')

  #
  extractCell <- function(i) {
    cell <- grid[i,]
    # subset del shape pero respectando los bordes
    shape.subset <- shape[!is.na(sp::over(shape, cell)[,1]),]
    #plot(cell); plot(shape.subset, add =T)

    raster.subset <- raster::crop(raster, shape.subset)
    df.extracted.data <- raster::extract(raster.subset, shape.subset, fun = fun, method = 'simple', na.rm = TRUE)
    df.extracted.link <- data.frame(id = shape.subset$id)

    df.extracted <- cbind(df.extracted.link, df.extracted.data)
    return(df.extracted)
  }

  if (multicore) {
    # Initialising cluster
    if (showMessages) cat('Initialising cluster ...\n')
    no_cores <- ifelse(parallel::detectCores() < 2, 1, parallel::detectCores() - 1)
    cl <- parallel::makeCluster(no_cores)
    parallel::clusterExport(cl, "stats2perform")
    parallel::clusterExport(cl, "grid")
    parallel::clusterExport(cl, "shape")
    parallel::clusterExport(cl, "crop")
    parallel::clusterExport(cl, "extract")
    parallel::clusterExport(cl, "over")
    parallel::clusterExport(cl, "raster")
  }

  if (showMessages) cat('Commencing extraction ...\n')
  if (multicore) {
    dfL <- parallel::parLapply(cl, 1:ncell(grid), extractCell)
    parallel::stopCluster(cl)
  } else
    dfL <- lapply(1:ncell(grid), extractCell)

  # Aunamos todo el listado, eliminamos dupes (por los bordes) y ordenamos
  if (showMessages) cat('Merging data ...\n')
  df <- do.call('rbind', dfL)
  df <- df[!duplicated(df$id),]
  df <- df[order(df$id), ]
  if ('std' %in% colnames(df)) df$std[is.na(df$std)] <- 0

  if (as.shape) {
    if (identical(shape$id , df$id)) {
      for (colname in colnames(df)) {
        shape@data[,colname] <- df[,colname]
      }
    } else warning(paste0('Size mismatch between extracted data and shape.', nrow(df), '/', nrow(shape)))
    return(shape)
  }

  return(df)
}

#' extractRasterStack
#'
#' \code{extractRasterStack} extracts data from a raster stack with a given polygon shape. It support multicore processing
#' through the \link{parallel} package and clustering with a default cellsize of 100 meters. It returns the same as
#' \link{extractRasterLayer} but it also works with multiband raster (\link{stack} or \link{brick}) and extract the
#' wavelengths from the file if exist.
#'
#' @param raster Raster. Raster to be extracted
#' @param shape Shapefile. Shape which define the polygon to be extracted
#' @param multicore Optional. Logical. If TRUE means multicore processing. By default is TRUE
#' @param gsize Optional. Numeric. Size of each cell in meters. 100 m by default
#' @param showMessages Optional. Logical. If TRUE all processing messages will be shown
#' @param fun Optional. Function. Function to summarize the values (e.g. mean). By default \link{stats2perform} is used.
#' @param as.shape Optional. Logical. If as.shape=TRUE, it returns a shape instead of a data.frame. By default is FALSE
#'
#' @return a data.frame or a Spatial object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' shape.extracted <- extractRasterStack(rasterBrick, shape, as.shape = T)
#' }
extractRasterStack <- function(raster, shape, multicore = TRUE, gsize = 100, showMessages = TRUE, fun = stats2perform, as.shape = F) {
  wavelengths <- as.numeric(sub('[^0-9]+$', '', sub('[^0-9]', '', names(raster))))

  list.df <- lapply(1:nlayers(raster), function(ch) {
    if (showMessages) cat('- Layer ', ch, '\n')
    df <- extractRasterLayer(raster[[ch]], shape, multicore, gsize, showMessages, fun, FALSE)
    colnames(df)[-1] <- paste0(wavelengths[ch],'_',colnames(df)[-1])
    return(df)
  })

  #df <- do.call(merge, list.df)
  df <- Reduce(function(x, y) merge(x, y, by = "id"), list.df)

  if (as.shape) {
    if (identical(shape$id , df$id)) {
      for (colname in colnames(df)) {
        shape@data[,colname] <- df[,colname]
      }
    } else warning(paste0('Size mismatch between extracted data and shape.', nrow(df), '/', nrow(shape)))
    return(shape)
  }

  return(df)
}

#' shape2grid
#'
#' \code{shape2grid} converts a shape into a grid. It is very useful for
#' clustering purposes.
#'
#' @param shape Shapefile. Spatial object to be converted
#' @param gsize Optional. Numeric. Size of each cell in meters. 100 m by default.
#'
#' @return a spatial object.
#'
#' @import raster sp methods
#' @export
#'
#' @examples
#' \dontrun{
#' shape <- shape2grid(shape, 100)
#' }
shape2grid <- function(shape, gsize = 100) {
  shape.extent <- raster::extent(shape)
  cols <- ceiling((shape.extent@xmax - shape.extent@xmin) / gsize)
  rows <- ceiling((shape.extent@ymax - shape.extent@ymin) / gsize)
  r <- raster::raster(shape.extent, nrow = rows, ncol = cols)
  raster::projection(r) <- raster::projection(shape)
  r[] <- 1:ncell(r)
  grid <- methods::as(r, 'SpatialPolygonsDataFrame')
  grid.subset <- grid[!is.na(sp::over(grid, shape)[,1]),]
  return(grid.subset)
}

#' shapeInterpolation
#'
#' \code{shapeInterpolation} interpolates a shapefile field to a raster with \link{krige}
#'
#' @param shape Shapefile. Spatial object to be interpolated
#' @param field Character. Field to be considered
#' @param cellsize Numeric. Raster output resolution in meters
#' @param shape.mask Optional. Shapefile object as a mask
#' @param showPercentage Optional. Logical. By default no percentage is shown
#'
#' @return a raster.
#'
#' @import raster sp methods gstat
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' \dontrun{
#' raster.interpolated <- shapeInterpolation(shape2, 'NDVI', 0.2, showPercentage = T)
#' raster.interpolated <- shapeInterpolation(shape2, 'TC', 5, shape.mask = shapemask)
#' }
shapeInterpolation <- function(shape, field, cellsize, shape.mask = NULL, showPercentage = FALSE) {
  shape.extent <- methods::as(extent(shape), 'SpatialPolygons')
  sp::proj4string(shape.extent) <- raster::projection(shape)
  grid <- sp::spsample(shape.extent, type = "regular", offset = c(0,0), cellsize = c(cellsize,cellsize))
  sp::gridded(grid) <- TRUE

  sp.df = gstat::krige(stats::as.formula(paste0(field,'~1')), shape, grid, debug.level = ifelse(showPercentage, -1, 0))
  raster <- raster::raster(sp.df)

  if (!is.null(shape.mask)) {
    raster <- raster::crop(raster::mask(raster, shape.mask), shape.mask)
  }

  return(raster)
}

#' stats2perform
#'
#' \code{stats2perform} defines the default stats for \link{extractRasterLayer} and \link{extractRasterStack}
#'
#' @param x set of data to summarised
#' @param na.rm Optional. Logical. If na.rm=TRUE, NA values are removed
#'
#' @importFrom stats sd
#' @export
#'
#' @return statistics. Mean, min, max, sd and number of pixels.
#'
stats2perform <- function(x, na.rm = FALSE) {
  c(mean = mean(x, na.rm = na.rm), min = min(x, na.rm = na.rm), max = max(x, na.rm = na.rm), std = stats::sd(x, na.rm = na.rm), pixels = length(x))
}

#' polygonize
#'
#' \code{polygonize} is a faster alternative to \link{rasterToPolygons}.
#'
#' This is an alternative function to \link{rasterToPolygons}, which pretends to be
#' the fastest way to perform this task. Original idea from \code{gdal_polygonizeR()}
#' It also offers a directly way to create a mask from a binary raster.
#'
#' It has a dependency on GDAL Tools to be installed
#'
#' @param raster Raster. The binary raster object to be converted.
#'
#' @return a spatial object.
#'
#' @import raster
#' @export
#'
#' @examples
#' \dontrun{
#' shape <- polygonize(raster)
#' }
polygonize <- function(raster) {
  python.location <- Sys.which('python')
  gdal_poligonize.location <- Sys.which('gdal_polygonize.py')
  input.path <- file.path(tempdir(), 'input.tif')
  output.path <- paste0(tempfile(), '.shp')

  raster::writeRaster(x = raster, filename = input.path, format = "GTiff", datatype = 'INT1U', overwrite = TRUE)
  system(sprintf('%s %s %s -mask %s -f "ESRI Shapefile" %s', python.location, gdal_poligonize.location, input.path, input.path, output.path), show.output.on.console = F)

  shape <- read.shape(output.path)
  shape <- shape[,-1]
  shape$id <- 1:length(shape)
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
#' @param input.path Character. The raster object path to be converted.
#' @param output.path Character. The path where it should be written.
#'
#' @export
#'
#' @examples
#' toGeoTIFF('./image.JPG', './outputimage.tif')
#'
toGeoTIFF <- function(input.path, output.path) {
  system2('gdal_translate.exe', args = (sprintf('"%1$s" "%2$s"', input.path, output.path)), stdout = FALSE)
}

#' gdal_fillnodata
#'
#' \code{gdal_fillnodata} is a tiny \code{gdal_fillnodata.py} wrapper from the GDAL library tools.
#'
#' The aim is just to convert the given raster and mask into a filled raster.
#'
#' It has a dependency on GDAL Tools to be installed
#'
#' @param raster Raster. The raster object to be processed, just one band.
#' @param mask Raster. The raster mask with zeroes and ones. It should be a binary mask.
#'
#' @import raster
#' @export
#'
#' @examples
#' \dontrun{
#' gdal_fillnodata(raster, binaryMask)
#' }
gdal_fillnodata <- function(raster, mask) {
  gdal_fillnodata.location <- Sys.which('gdal_fillnodata.py')
  python.location <- Sys.which('python')

  path.mask <- file.path(tempdir(), 'mask.tif')
  path.input <- file.path(tempdir(), 'input.tif')
  path.output <- paste0(tempfile(), '.tif')

  raster::writeRaster(x = mask, filename = path.mask, format = "GTiff", datatype = 'INT1U', overwrite = TRUE)
  raster::writeRaster(x = raster, filename = path.input, format = "GTiff", overwrite = TRUE)

  system(sprintf('%s %s -mask %s -of GTiff %s %s', python.location, gdal_fillnodata.location, path.mask, path.input, path.output))

  file.remove(path.mask, path.input)
  return(raster(path.output))
}

#' long2UTM
#'
#' \code{long2UTM} converts the given longitude into the corresponing UTM zone.
#'
#' @param long Numeric. Longitude coordinate
#'
#' @export
#'
#' @examples
#' long2UTM(-4)
#'
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

#' read.shape
#'
#' \code{read.shape} is an easier alternative to \link{readOGR}.
#'
#' This is an alternative function to \link{readOGR}, which pretends to be the easiest way
#' to load shapefiles.
#'
#' @param shape.path Character. Shapefile path
#'
#' @return a spatial object.
#'
#' @import rgdal
#' @export
#'
#' @examples
#' \dontrun{
#' read.shape('./myshapefile.shp')
#' }
read.shape <- function(shape.path) {
  rgdal::readOGR(dsn = dirname(shape.path), layer = sub('\\..*$', '', basename(shape.path)), verbose = FALSE)
}

#' write.shape
#'
#' \code{write.shape} is an easier alternative to \link{writeOGR}.
#'
#' This is an alternative function to \link{writeOGR}, which pretends to be the easiest way
#' to write shapefiles.
#'
#' @param shape Shapefile. Spatial object to be saved.
#' @param shape.path Character. Path where it should be written.
#' @param overwrite Logical. force overwrite or not.
#'
#' @import rgdal
#' @export
#'
#' @examples
#' \dontrun{
#' write.shape(shapeObject, './myshapefile.shp')
#' }
write.shape <- function(shape, shape.path, overwrite = FALSE) {
  rgdal::writeOGR(obj = shape, dsn = dirname(shape.path), layer = sub('\\..*$', '', basename(shape.path)), driver = "ESRI Shapefile", overwrite_layer = overwrite)
}

#' genLUT
#'
#' \code{genLUT} generates a Lookup table with given parameters. This is very useful when you are generating a data entry for
#' RTM inversion
#'
#' @param ... At least 2 parameters or a list composed by 2 parameters are needed. It could be an array of numbers or characters.
#'
#' @return a matrix which is indeed the Lookup Table
#'
#' @export
#'
#' @examples
#' genLUT(Cab = 10:30, LAI = seq(0, 0.05, 0.01), Car = runif(10, 2.0, 3.5))
genLUT <- function(...) {
  # repeat a row n times
  rep.row <- function(x,n){
    matrix(rep(x, each = n), nrow = n)
  }

  # main recursive function
  genLUT0 <- function(...) {
    vars <- list(...)

    # if ellipsis contains one parameter and it's a list we should restore it
    if (length(vars) == 1) {
      if (class(vars[[1]]) == 'list')
        vars = vars[[1]]
    }

    # Error reporting. We need at least 2 parameters
    if (length(vars) < 2)
      stop('At least 2 parameters or a list composed by 2 parameters are needed')

    if (length(vars) > 2)
      return(genLUT0(genLUT0(vars[-length(vars)]), vars[[length(vars)]]))

    x <- vars[[1]]
    y <- vars[[2]]

    if (class(x) == 'matrix') {
      list.df <- lapply(1:nrow(x), function(i) { cbind(x = rep.row(x[i,], length(y)), y)})
    } else {
      list.df <- lapply(1:length(x), function(i) { cbind(x = rep(x[i], length(y)), y)})
    }
    df <- do.call(rbind, list.df)

    return(df)
  }

  vars <- list(...)
  lut <- genLUT0(...)
  colnames(lut) <- names(vars)

  return(lut)
}

# devtools::use_package('parallel')
# devtools::use_package('raster')
# devtools::use_package("rgdal")
# devtools::use_package('sp')
# devtools::use_package('methods')
# devtools::use_package('gstat')
# devtools::use_package('stats')

#roxygen2::roxygenise()
# devtools::document()

