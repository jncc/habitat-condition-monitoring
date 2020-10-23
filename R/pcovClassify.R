## ############################## ##
##
## Script Name: Preparing the APGB classified images as percentage covers of bare peat.
##
##
## Contact (if different from above):
##
## Date Created: 2020-08-13
##
## Date Modified: 2020-09-11
##
## Author: JNCC
##
## Licence: MIT licence##
##
## Abstract: Function to convert the classified bare peat images into a percentage cover at 10m spatial resolution
##
##
##
## R version 3.6.1 (2019-07-05)
## Dependencies:
## gdalUtils_2.0.3.2 raster_3.0-7  sp_1.3-1          dplyr_0.8.5
## stringr_1.4.0     purrr_0.3.4   sf_0.9-2
##
## ############################## ##

#' pcov_classify
#'
#' @param img_path folder path to where the Sentinel-2 iamgery is stored
#' @param bare_path folder path to where the classified bare peat rasterlayers are stored
#' @param out_path folder path to store the outputs
#' @param polymask shapefile of polygons to mask to
#'
#' @return
#' @export
#'
#' @examples
pcov_classify <- function(img_path, bare_path, out_path, polymask){

  #list all files
  imgs <- list.files(img_path,pattern='tif',full.names=T)
  bareimgs <- data.frame(filename=list.files(bare_path,pattern='tif',full.names=T))

  #create folders
  if(!dir.exists(paste0(out_path,'bare_classed'))){
    dir.create(paste0(out_path,'bare_classed'))
  }
  if(!dir.exists(paste0(out_path,'bare_pcov'))){
    dir.create(paste0(out_path,'bare_pcov'))
  }

  #read in mask layer
  habmask <- sf::st_read(polymask)
  #iterate through imagery tiles
  purrr::map(imgs, .f=function(tile){

    #load in imagery tile
    tile_r <- raster::raster(tile, band=1)

    #### Compiling the classified bare peat tile ####
    ## 0 = no bare peat, 1 = bare peat, NA = masked area
    #update all values to 0
    tile_r[!is.na(tile_r)] <- 0
    #get tile name
    name <- gsub(unlist(stringr::str_split(basename(tile),'_'))[2],pattern='.tif',replacement='')
    #find any bare tiles with name
    bare_tile <-  bareimgs %>% dplyr::filter(stringr::str_detect(filename,name))
    #mosaic bare images
    if(nrow(bare_tile)!=0){
      barelist <- purrr::map(as.character(bare_tile$filename),raster::raster)
      #if more than 1 bare layer for the tile mosaic together
      if(length(barelist)>1){
        barelist$fun <- mean
        bare_r<- do.call(raster::mosaic,c(barelist,progress="window"))
        } else {
          bare_r<- barelist[[1]]
          }
      #extend to whole tile extent
      bare_whole <- raster::extend(bare_r,raster::extent(tile_r))
      # if bare extent is too big then crop to tile extent
      if(!(raster::extent(tile_r)==raster::extent(bare_whole))){
        bare_whole <- raster::crop(bare_whole,tile_r)
        }
      #overlay to update any bare tiles
      tile_bare <-raster::overlay(tile_r,bare_whole,fun = function(x, y) {
        x[y[]==1] <- 1
        return(x)
        })

      } else { # if no bare tiles then use tile with 0s
        tile_bare <- tile_r
      }
    #write out tile layer
    raster::writeRaster(tile_bare,paste0(out_path,'bare_classed/BC_',basename(tile)),overwrite=T)

    #### Converting to percentage cover of bare peat at 10m pixel ####
    #convert NA values to 0
    tile_bare[is.na(tile_bare)]<-0
    raster::writeRaster(tile_bare,paste0(out_path,'bare_pcov/baretmp_',basename(tile)),overwrite=T)
    #create a 10m pixel version of raster
    # use machine's gdal installation
    gdalUtils::gdal_setInstallation('D:/Programs/QGIS/bin/')
    gdalUtils::gdalwarp(srcfile = paste0(out_path,'bare_pcov/baretmp_',basename(tile)),
                        dstfile = paste0(out_path,'bare_pcov/pcov_',basename(tile)),
                        s_srs = 'EPSG:27700',t_srs = 'EPSG:27700',
                        r = 'average',tr=c(10,10), overwrite = T, verbose = T)

    #annoyingly gdalwarp doesnt summarise on sum so we will use average*pixelcellcount = sum
    ## calculate no.cells in high res compared to low res
    highres_count <- (10 / res(tile_bare)[1])^2

    pcov_img <- raster::raster(paste0(out_path,'bare_pcov/pcov_',basename(tile)))
    pcov_df <- raster::as.data.frame(pcov_img)
    names(pcov_df) <- 'average'
    pcov_df <- pcov_df %>% dplyr::mutate(sum = average*highres_count) %>%
      dplyr::mutate(pcov = sum/highres_count)
    pcov_r <- raster::setValues(pcov_img,values=pcov_df$pcov)

    #mask to remove border effect of incomplete cells
    pcov_mask <- raster::mask(pcov_r,habmask)

    raster::writeRaster(pcov_mask,paste0(out_path,'bare_pcov/pcov_',basename(tile)),
                overwrite=T)
    file.remove(paste0(out_path,'bare_pcov/baretmp_',basename(tile)))
    print(paste0(name, ' done.'))

    })
}
