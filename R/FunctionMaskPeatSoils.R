## ############################## ##
##
## Script Name: Mask to peat soils
##
## Author: Becky Trippier
##
## Contact (if different from above):
##
## Date Created: 2020-07-28
##
## Date Modified:  2020-09-11
##
## Licence: JNCC
##
##
## Abstract: Function to mask tiles to the peat soils and delete any tiles which do not overlap with peat soils. This will overwrite the original imagery with the masked layer and you can use the delete argument to remvoe imagery which does not overlap with the mask.
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## fasterize_1.0.2 sf_0.9-2
## raster_3.0-7    sp_1.3-1
##
## ############################## ##



#' Title
#'
#' @param img_folder folder path containing the imagery to iterate through
#' @param mask mask to retain the imagery within
#' @param epsg coordinate reference system, EPSG code
#' @param delete logical, if you wish to delete those images which do not overlap within the mask layer
#'
#' @return
#' @export
#'
#' @examples
MaskPeatSoils <- function(img_folder,mask, epsg = 27700, delete=T){
  #list all files
  imgs <- list.files(img_folder, pattern='.tif', full.names = T )
  # read in the mask
  mask_sf <- sf::st_read(mask) %>% sf::st_transform(epsg)
  #iterate through images
  purrr::map(imgs, .f=function(img){
    #load in image
    img_r <- raster::brick(img)
    #check if overlaps with the mask
    img_poly <- sf::st_as_sf(as(raster::extent(img_r), 'SpatialPolygons')) %>% sf::st_set_crs(epsg)
    int_poly <- suppressWarnings(sf::st_intersection(mask_sf,img_poly))
    # if no overlap remove tile
    if(nrow(int_poly)==0){
      file.remove(img)
      print(paste0(basename(img), " done."))
    } else {
      # if overlap then mask tile to peat soil
    r <-raster::raster(ext = raster::extent(img_r),crs = raster::crs(img_r),res = raster::res(img_r))
    mask_r <- fasterize::fasterize(mask_sf, r)
    img_masked <-raster::overlay(img_r,mask_r,fun = function(x, y) {
      x[is.na(y[])] <- NA
      return(x)
    })
    #overwrite result
    raster::writeRaster(img_masked,img, overwrite=T)
    print(paste0(basename(img), " done."))
    }

  })

}
