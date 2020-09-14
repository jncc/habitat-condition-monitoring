## ############################## ##
##
## Script Name: Thresholding bare peat based on indices
##
## Author: Becky Trippier
##
## Contact (if different from above):
##
## Date Created: 2019-11-14
##
## Date Modified: 2019-03-16
##
## Licence: JNCC
##
##
## Abstract: function for thresholding based on indices. This will generate the specified indices and threshold based on a supplied function using these layers. The function must use the indices in alphabetical order in order to read in correctly.
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
##  dplyr_0.8.1  rgeos_0.4-3  rgdal_1.4-4
##  raster_3.0-7 sp_1.3-1
##
## ############################## ##
#


#' thresholding bare peat based on indices
#'
#' @param Img.path path to the folder with indices values or list of file paths
#' @param out.path output folder to write output to
#' @param spec.bands the spectral band numbers you wish to include in the thresholding rule
#' @param ind.name name of indices, will assume that the file naming convention is ind.name_... and will replace with BARE_ when writing out
#' @param c.fun function with which to calculate across the indices stack
#' @param nir near infrared band in the multispectral imagery
#' @param r red band in the multispectral imagery
#' @param g green band in the multispectral imagery
#' @param b blue band in the multispectral imagery
#' @param swir short wave infrared band in the multispectral imagery
#' @param start starting image to process, defaults to the first image
#' @param end  ending image to process, if NULL will default to the last image in the folder
#'
#' @return
#' @export
#'
#' @examples
barethresh <- function(Img.path,out.path,spec.bands=NA,ind.name = NA, c.fun, nir=4, r=1,g=2,b=3,swir=NA,start=1,end=NA){

  #create bare peat folder
  if (!file.exists(paste(out.path,"bare",sep=""))){
    dir.create(paste(out.path,"/bare",sep=""))
  }

  #creates a temporary file to write to
  if (!file.exists(paste0(out.path,"bare","/temp"))){
    dir.create(paste0(out.path,"bare","/temp"))
  }

  # list all images
  if (file_test("-f",Img.path)==T){
    Img.list <- list(Img.path)
  } else {
    Img.list <- list.files(Img.path, full.names = T, pattern=".tif")
  }

  # get the last image number to loop through if none supplied
  if(is.na(end)){
    end <- length(Img.list)
  }



  #loop through images
  for (i in start:end){
    # get spec bands if included in thresholding rule
    if (!is.na(spec.bands[1])){
      spec.1 <- raster::stack(paste0(Img.list[i]))
      #loop through spectral bands required
      all.band <- list()
      for (j in spec.bands){
        spec.band <- list(spec.1[[j]])
        names(spec.band) <- paste0("sb", j)
        all.band <-append(all.band,spec.band)
      }
    } else{
      all.band <- list()
    }

    if(!is.na(ind.name[1])){
      #generates the indices
      runIndices(imagepath=Img.list[[i]],outpath=paste0(out.path,"bare","/temp/"), nir=4, r=1,g=2,b=3,swir=swir, indices=ind.name,nf=F)
      name <- gsub(basename(Img.list[i]),pattern=".tif",replacement="")

      #takes first indices and lists files in folder
      Indices.list <- list.files(paste0(out.path,"bare/temp/Indices/"), full.names = T,pattern=name)
      indat.list <- purrr::map(Indices.list,.f=raster::raster)
    } else {
      indat.list <- list()
    }

    #join the lists and stack
    all.list <- append(all.band,indat.list)
    all.stack <- raster::stack(all.list)
    #Apply function to raster stack
    r.class <- raster::overlay(all.stack, fun=c.fun)
    #save it
    raster::writeRaster(r.class,paste0(out.path,"bare/",name,'_Bare.tif'), overwrite=T)
    print(paste(i, "of", length(Img.list), "done"))
    #remove the temporary folder
    unlink(paste0(out.path,"bare/temp/Indices"), recursive =TRUE)
  }

}
