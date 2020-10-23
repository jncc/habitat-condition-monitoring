## ############################## ##
##
## Script Name: Preparing Sentinel-2 imagery by cloud and shadow masking and calculating indices
##
##
## Contact (if different from above):
##
## Date Created: 2020-08-11
##
## Date Modified:2020-09-11
##
## Author: JNCC
##
## Licence: MIT licence##
##
## Abstract: Function to crop to an AOI (optional) and conduct cloud and shadow masking on a folder of Sentinel-2 imagery. Specified indices will then be calculated from the masked imagery.
##
##
##
## R version 3.6.1 (2019-07-05)
## Dependencies:
##  raster_3.0-7 sp_1.3-1     purrr_0.3.4      stringr_1.4.0
##  dplyr_0.8.5  tibble_3.0.1 lubridate_1.7.4  sf_0.9-2
##
## ############################## ##


#' Title
#'
#' @param s2path folder path to the multispectral Sentinel-2 imagery
#' @param out_folder folder path to save output files
#' @param nirband  near infra-red band
#' @param rband numeric, red band
#' @param bband numeric, blue band
#' @param gband numeric, green band
#' @param swirband numeric, optional - Short wave infrared band
#' @param indices a vector of indices to retrieve e.g. c("EVI", "GLI","GNDVI","RDVI","SAVI","SBL"). The function will accept EVI,GLI,GNDVI,RDVI,SAVI,SBL,RB,RG,Brightness,NBR
#' @param cropAOI optional, a shapefile containing the area of interest to crop the imagery to
#'
#' @return
#' @export
#'
#' @examples
s2_processing <- function(s2path, out_folder,nirband=8,rband=3,bband=1,gband=2,swirband=NA,indices,cropAOI=NA){

  if(!dir.exists(paste0(out_folder,'S2_Masked'))){
    dir.create(paste0(out_folder,'S2_Masked'))
  }
  #get files with dates
  s2images <- data.frame(files=list.files(s2path,full.names=F,pattern='.tif')) %>%
    dplyr::mutate(files=as.character(files)) %>%
    dplyr::mutate(date=stringr::str_extract(files,"\\d{8}"))
  #get just the imagery
  s2_images <- s2images %>%
    dplyr::filter(stringr::str_detect(files,pattern='stdsref'))
  print(paste(nrow(s2_images), " images found."))

  #iterate through imagery
  purrr::map(s2_images$date, .f=function(senimg){
    #separate and load files
    datefiles <- s2images %>% dplyr::filter(date==senimg) %>% dplyr::select(files)
    imgfile <- datefiles %>% dplyr::filter(stringr::str_detect(datefiles$files,pattern="stdsref")) %>% as.character()
    cloudfile <- datefiles %>% dplyr::filter(stringr::str_detect(datefiles$files,pattern="cloud")) %>% as.character()
    topofile <- datefiles %>% dplyr::filter(stringr::str_detect(datefiles$files,pattern="topo")) %>% as.character()

    #cloud and shadow masking
    satellite <- raster::stack(paste0(s2path,imgfile))
    cloud <- raster::raster(paste0(s2path,cloudfile))
    shadow <- raster::raster(paste0(s2path,topofile))

    if(!is.na(cropAOI)){
      AOI <- sf::st_read(cropAOI)
      satellite <-raster::crop(satellite,AOI)
      cloud <-raster::crop(cloud,AOI)
      shadow <-raster::crop(shadow,AOI)
    }
    #cloud mask
    satellite_msk <- raster::overlay(satellite, cloud,fun = function(x, y){
      x[!is.na(y[])] <- NA
      return(x)
    })
    #shadow mask
    satellite_msk <- raster::overlay(satellite_msk, shadow,fun = function(x, y){
      x[!is.na(y[])] <- NA
      return(x)
    })
    raster::writeRaster(satellite_msk,paste0(out_folder,'S2_masked/',tools::file_path_sans_ext(imgfile),"_msk.tif"),overwrite=T)
    #get date
    date <- lubridate::ymd(senimg)
    #calculate indices
    runIndices(paste0(out_folder,'S2_masked/',tools::file_path_sans_ext(imgfile),"_msk.tif"),outpath=paste0(out_folder),nir=nirband, r=rband, b=bband, g=gband, swir=swirband,indices=indices, nf=F)
    print(paste0(senimg, ' done.'))
    })
}

