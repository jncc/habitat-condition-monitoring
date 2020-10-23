## ############################## ##
##
## Script Name: Bare Peat Masking
##
##
## Contact (if different from above):
##
## Date Created: 2020-06-23
##
## Date Modified: 2020-09-11
##
## Author: JNCC
##
## Licence: MIT licence
##
##
## Abstract: Will mask  your classified bare peat image given specified areas to include and exclude
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## raster_3.0-7 sp_1.3-1
##
## ############################## ##
#' Bare Peat Masking
#'
#' @param bare.path path to the bare peat classified image
#' @param out.path path to save the outputs to, a new folder called 'training' will be created
#' @param incl.masks list of shapefiles to mask the information to, bare peat areas falling within these polygons with be kept
#' @param excl.masks list of shapefiles to mask the information from, bare peat areas falling within these polygons with be excluded
#' @param start image number to start on - for iterating, otherwise will start on first image
#' @param end  image number to end on - for iterating, otherwise will take all
#'
#' @return
#' @export
#'
#' @examples
mask_bare <- function(bare.path, out.path, incl.masks, excl.masks,start=1,end=NA){
  #create new directory
  if (!file.exists(paste0(out.path,"training"))){
    dir.create(paste0(out.path,"training"))
  }
  #list all files
  bare.listed <- list.files(bare.path,full.names = T,pattern=".tif")
  if (is.na(end)){
    end <- length(bare.listed)
  }
  #iterate through bare thresholded images
  for (m in start:end){
    granule <- raster::raster(bare.listed[m])
    granule[granule==0] <- NA #remove shadow class

    train.msk <- granule
    #mask to layers
    for(n in 1:length(incl.masks)){
      masklayer <- incl.masks[[n]]  %>% as("Spatial")
      maskcrop <- raster::crop(masklayer, raster::extent(train.msk))
      if(!is.null(maskcrop)){
        train.msk <- raster::mask(train.msk,maskcrop)
      }
    }
    #mask out layers
    for (o in 1:length(excl.masks)){
      masklayer <- excl.masks[[o]]  %>% as("Spatial")
      maskcrop <- tryCatch(raster::crop(masklayer, raster::extent(train.msk)),error=function(e){NULL})
      if(!is.null(maskcrop)){
        train.msk <- raster::mask(train.msk,maskcrop,inverse=TRUE)
      }
    }
    #save out
    name <- basename(bare.listed[m]) %>% gsub(pattern="Bare_",replacement="")
    raster::writeRaster(train.msk,paste0(out.path,"training/TRAIN_",name), overwrite=T)
    print(paste(m, "of", length(bare.listed), "done."))
  }
}

