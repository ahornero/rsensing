# rsensing
Remote Sensing and GIS tools for R

![R Package rsensing](https://img.shields.io/badge/R%20Package-rsensing-green.svg)
[![Licence](https://img.shields.io/badge/license-GPLv3-orange.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

# Included alternative functions

### read.shape
_read.shape_ is an easier alternative to [readOGR](https://www.rdocumentation.org/packages/rgdal/versions/1.2-13/topics/readOGR), which pretends to be the easiest way to load shapefiles.
```R
read.shape('./myshapefile.shp')
```

### write.shape
_write.shape_ is an easier alternative to [writeOGR](https://www.rdocumentation.org/packages/rgdal/versions/1.2-15/topics/writeOGR), which pretends to be the easiest way to write shapefiles.
```R
write.shape(shapeObject, './myshapefile.shp')
```

### rasterToPolygons0
This is an alternative function to [rasterToPolygons](https://www.rdocumentation.org/packages/raster/versions/2.5-8/topics/rasterToPolygons), which pretends to be the fastest way to perform this task. Original idea from [gdal_polygonizeR](https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/). It also offers a directly way to create a mask.
```R
shape <- rasterToPolygons0(rasterObject)
shape <- rasterToPolygons0(rasterObject, 255)
```

### toGeoTIFF
_toGeoTIFF_ is a tiny [gdal_translate](http://www.gdal.org/gdal_translate.html) wrapper. The aim is just to convert the given image into a GeoTIFF image.
```R
toGeoTIFF('./image.JPG', './outputimage.tif')
```
