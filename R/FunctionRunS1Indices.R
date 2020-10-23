## ############################## ##
##
## Script Name: Calculate indices for Sentinel-1 imagery
##
##
## Contact (if different from above):
##
## Date Created: 2020-07-02
##
## Date Modified:2020-09-11
##
## Author: JNCC
##
## Licence: MIT licence
##
##
## Abstract: Function to calculate RVI and RVIv indices from Sentinel 1 imagery
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## dplyr_0.8.5  purrr_0.3.4  raster_3.0-7 tibble_3.0.1  purrr_0.3.4
##
## ############################## ##


#' runS1Indices
#'
#' @param imagepath folder path to the Sentinel-1 imagery
#' @param outpath path to save NDVI output files
#' @param vv numeric, number of the vv band in the imagery
#' @param vh numeric, number of the vh band in the imagery
#' @param indices a vector of indices to retrieve, e.g. c('RVI','RVIv')
#' @param nf logical, if you wish to create new folder per image
#'
#' @return
#' @export
#'
#' @examples
runS1Indices <- function(imagepath, outpath, vv,vh,indices=c("RVI","RVIv"),nf=F){

  #new folder and checks
  if (!file.exists(paste0(outpath,"Indices"))){
    dir.create(paste0(outpath,"Indices"))
    }
  if(is.na(vv)|is.na(vh)){
    stop("need to supply min band numbers.")
  }
  #test if file or folder
  if (file_test("-f",imagepath)==T){
    files <- list(imagepath)
    } else {
      files <- list.files(imagepath, full.names = T, pattern=".tif")
    }

  #create indices function lookup
  RVI_fun <- function(x){
    dop <- (x[[vv]]/(x[[vv]]+x[[vh]]))
    rvi <- (sqrt(abs(dop)))*(4*(x[[vh]]))/(x[[vv]]+x[[vh]])
    return(rvi)}
  RVIv_fun <- function(x){(4*(x[[vh]]))/(x[[vv]]+x[[vh]])}

  #lookup table creating
  all_ind <- tibble::tribble(~name,~formula,
                           "RVI",  RVI_fun,
                           "RVIv", RVIv_fun)

  #filter to specified indices
  indi_tib <- all_ind %>% dplyr::filter(name %in% indices)

  # iterate through files and indices calculations
  inputgrid <- expand.grid(x=files, y=indi_tib$name)
  purrr::map2(as.character(inputgrid$x),as.character(inputgrid$y), function(x,y){
    # get file and break up into raster bands
    filename <- x
    filebasename <- gsub(basename(filename),pattern=".tif",replacement="")
    #create place to save if nf=T
    if(nf==T){
      if(!dir.exists(paste0(outpath,"Indices/",filebasename))){
        dir.create(paste0(outpath,"Indices/",filebasename))
        }
      fileout_path <- paste0(outpath,"Indices/",filebasename)
      } else {
        fileout_path <- paste0(outpath,"Indices")
      }
    # get file and indices function
    granule <- raster::brick(as.character(filename))
    indi_row <- indi_tib %>% dplyr::filter(name == y)
    #run indices function over rasterbrick
    ind_out <- raster::calc(granule, fun=indi_row$formula[[1]])
    #write out indices layer
    raster::writeRaster(ind_out,paste0(fileout_path, "/",filebasename,"_",as.character(indi_row$name), ".tif"),overwrite=T)
    #print so we know its finished
    print(paste0(filebasename,":",as.character(indi_row$name)," done."))
    })
}

