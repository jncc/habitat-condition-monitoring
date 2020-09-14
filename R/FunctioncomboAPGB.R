## ############################## ##
##
## Script Name: Function for combining the apgb RGB and CIR images
##
## Author: Becky Trippier
##
## Contact (if different from above):
##
## Date Created: 2020-06-17
##
## Date Modified: 2020-09-11
##
## Licence: JNCC
##
##
## Abstract: function designed for use with DEFRAs APGB aerial photography to resample the coarser 50cm resolution CIR image in order to combine the near-infrared band with the 25cm resolution RGB image.
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## raster_3.0-7 sp_1.3-1
##
## ############################## ##

#' #### comboAPGB ####
#' Function designed to combine RGB imagery with additional NIR band from infrared image. It resamples image b to image a resolution then combines the two
# adds just the first layer from the b.path image but can be modified to add all or select layers
#' @param a.path path to RGB image folder
#' @param b.path path to infrared image folder
#' @param nir band in b image which contains the Near-infrared band to join
#' @param out.path output folder to save to
#' @param start image to start on, will use list.files of the .tif files in folder
#' @param end image to end on, will use list.files of the .tif files in folder
#'
#' @return
#' @export
#'
#' @examples
comboAPGB <- function(a.path, b.path,nir=1,out.path, start=1,end=NULL){
  #get all files
  aerial.list <- list.files(a.path, pattern='.tif')
  infra.list <- list.files(b.path, pattern='.tif')
  #makes ouput folder
  if (!file.exists(paste(out.path,"combined",sep=""))){
    dir.create(paste(out.path,"combined",sep=""))
  }
  #iterates through resample to image a res and combine with infra-red band
  if(is.null(end)){
    end <- length(aerial.list)
  }
  for (i in start:end){
    #load in imagery
    granule <- raster::stack(paste0(a.path, aerial.list[i]))
    inf.gran <- raster::stack(paste0(b.path, infra.list[i]))
    #resample granule to same resolution
    gran.samp <- raster::resample(inf.gran,granule,  method = "bilinear")
    #stack the rgb with nir added to 4th band
    gran.all <- raster::addLayer(granule, gran.samp[[nir]])
    name <- aerial.list[i]
    #write out
    raster::writeRaster(gran.all, paste0(out.path,"combined/",name), overwrite=T)
    print(paste(i,"of",length(aerial.list),"done."))
  }

}
