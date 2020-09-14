## ############################## ##
##
## Script Name: Function for compiling variable stack from sentinel 2 imagery and indices
##
## Author: Becky Trippier
##
## Contact (if different from above):
##
## Date Created: 2020-08-14
##
## Date Modified:2020-08-14
##
## Licence: JNCC
##
##
## Abstract:Function for compiling variable stack from sentinel 2 imagery, indices and additional variable layers
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## sf_0.9-2     raster_3.0-7 sp_1.3-1     purrr_0.3.4
##
## ############################## ##


#' sat_varstack
#'
#' @param varpath folder path to the variable layers
#' @param satimg filepath to Sentinel-2 imagery layer
#' @param satbands band required from the sentinel imagery
#' @param indices optional, vector of indices layers to use, note will search to retreive these from a folder named Indices, see code for naming convention. expects names of indices not filepaths
#' @param vars optional, vector of filepaths for additional variable layers to include in the varstack
#' @param mask optional, shapefile to mask to
#' @param outpath output folder to write to
#' @param stackname name for the output rasterstack
#'
#' @return
#' @export
#'
#' @examples
sat_varstack <- function(varpath,satimg,satbands=NA,indices=NA,vars=NA,mask=NA,outpath,stackname="stack"){

  if(all(is.na(satbands)) & all(is.na(indices)) & all(is.na(vars))){
    stop("Need to supply at least one of satbands, indices or vars")
  }

    all_vars <- NULL

  # s2 imagery bands
  if(!all(is.na(satbands))){
    #iterate through s2 imagery and pull band provided
    band_list <- purrr::map(satbands,.f=function(senimg){
      satband <- raster::raster(paste0(varpath,'S2_Masked/',satimg),band=senimg)
      })
    names(band_list)<- paste0("Band",satbands)
    all_vars <- append(all_vars,band_list)
    }

  #indices layers
  if(!all(is.na(indices))){
    #iterate through to retreive indices, naming convention follows that produces from runindices function
    ind_list <- purrr::map(indices,.f=function(indimg){
      ind <- raster::raster(paste0(varpath,'Indices/',gsub('.tif',satimg,replacement=''),'_',indimg,'.tif'))
      })
    names(ind_list)<-indices
    all_vars <- append(all_vars,ind_list)
    }

  # other variable layers
  if(!all(is.na(vars))){
    #iterate through collating these. If imagery or indices previously provide then these layers will be reprojected to match the imagery/indices
     vars_list <- purrr::map(vars,.f=function(varimg){
       var <- raster::raster(paste0(varpath,varimg))
       if(!is.null(all_vars)){
         ref_tif<- all_vars[[1]]
         var_ref <- raster::projectRaster(var,ref_tif,method='bilinear')
       }
       })
     names(vars_list)<-gsub('.tif',vars,replacement="")
     all_vars <- append(all_vars,vars_list)
     }

  #stack em
  varstack <- raster::stack(all_vars)
  #mask if mask supplied
  if(!is.na(mask)){
    mask_layer <- sf::st_read(mask)
    varstack <- raster::mask(varstack,mask_layer)
  }
  raster::writeRaster(varstack,paste0(outpath,stackname,'.tif'),overwrite=T)
  print(names(varstack))
}
