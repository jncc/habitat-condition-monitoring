## ############################## ##
##
## Script Name: Calculate indices for Sentinel-2 imagery
##
##
## Contact (if different from above):
##
## Date Created: 2020-06-17
##
## Date Modified:2020-09-11
##
## Licence: JNCC
##
##
## Abstract: Function to iterate through a folder of Sentinel-2 imagery and calculate specified indices. The indices available to calculate are noted in the lookup table within the function. These include:
##    EVI - Enhanced Vegetation Index
##    GLI - Green Leaf Index
##    GNDVI - Green Normalised Difference Vegetation Index
##    RDVI - Renormalized Difference Vegetation Index
##    SAVI - Soil Adjusted Vegetation Index
##    SBL - Soil Background Line
##    RB - Red-Blue ratio
##    RG - Red-Green ratio
##    Brightness - Brightness (mean of Red, Green and Blue bands)
##    NDVI - Normalized difference vegetation index
##    NBR - Normalized Difference NIR/SWIR Normalized Burn Ratio
##    NDWI  - Normalized Difference Water Index
##    NDMI - Normalized Difference Moisture Index
##    GRVI - Green Ratio Vegetation Index
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## raster_3.0-7 sp_1.3-1     purrr_0.3.4
## dplyr_0.8.5  tibble_3.0.1
##
## ############################## ##
#### Run Indices ####
#' Run Indices
#'
#' @param imagepath  folder path to the multispectral Sentinel-2 imagery
#' @param outpath folder path to save output files
#' @param nir near infra-red band
#' @param r numeric, red band
#' @param b numeric, blue band
#' @param g numeric, green band
#' @param swir numeric, optional - Short wave infrared band
#' @param indices a vector of indices to retrieve e.g. c("EVI", "GLI","GNDVI","RDVI","SAVI","SBL"). The function will accept EVI,GLI,GNDVI,RDVI,SAVI,SBL,RB,RG,Brightness,NBR
#' @param nf logical, if you wish to create new folder per image
#'
#' @return
#' @export
#'
#' @examples
#'
runIndices <- function(imagepath, outpath, nir,r, b, g, swir=NA,indices, nf=T){

  #new folder and checks
  if (!file.exists(paste(outpath,"Indices",sep=""))){
    dir.create(paste(outpath,"Indices",sep=""))
  }
  if(is.na(nir)|is.na(r)|is.na(b)|is.na(g)){
    stop("need to supply min band numbers.")
  }
  #test if file or folder
  if (file_test("-f",imagepath)==T){
    files <- list(imagepath)
  } else {
    files <- list.files(imagepath, full.names = T, pattern=".tif")
  }

  #create indices function lookup
  EVI_fun <- function(x){2.5*((x[[nir]]- x[[r]])/((x[[nir]]+(6* x[[r]])-(7.5*x[[b]]))+1))}
  GLI_fun <- function(x){((2*x[[g]])-x[[r]]-x[[b]])/((2*x[[g]])+x[[r]]+x[[b]])}
  GNDVI_fun <- function(x){(x[[nir]]-x[[g]])/(x[[nir]]+x[[g]])}
  RDVI_fun <- function(x){(x[[nir]]-x[[r]])/(x[[nir]]+x[[r]])^0.5}
  SAVI_fun <- function(x){((x[[nir]]-x[[r]])/(x[[nir]]+x[[r]]+0.5))*(1+0.5)}
  SBL_fun <- function(x){(x[[nir]]-(2.4*x[[r]]))}
  RB_fun <- function(x){(x[[r]]/x[[b]])}
  RG_fun <- function(x){(x[[r]]/x[[g]])}
  Brightness_fun <- function(x){((x[[r]]+x[[g]]+x[[b]])/3)}
  NDVI_fun <- function(x){(x[[nir]]-x[[r]])/(x[[nir]]+x[[r]])}
  NBR_fun <- function(x){(x[[nir]]-x[[swir]])/(x[[nir]]+x[[swir]])}
  NDWI_fun <- function(x){(x[[g]]-x[[nir]])/(x[[g]]+x[[nir]])}
  NDMI_fun <- function(x){(x[[nir]]-x[[swir]])/(x[[nir]]+x[[swir]])}
  GRVI_fun <- function(x){x[[nir]]/x[[g]]}

  #lookup table creating
  all_ind <- tibble::tribble(~name,~formula,
                     "EVI",  EVI_fun,
                     "GLI", GLI_fun,
                     "GNDVI",GNDVI_fun,
                     "RDVI", RDVI_fun,
                     "SAVI", SAVI_fun,
                     "SBL", SBL_fun,
                     "RB", RB_fun,
                     "RG",RG_fun,
                     "Brightness",Brightness_fun,
                     "NDVI",NDVI_fun,
                     "NBR",NBR_fun,
                     "NDWI",NDWI_fun,
                     "NDMI",NDMI_fun,
                     "GRVI",GRVI_fun
  )
  #filter to specified indices
  indi_tib <- all_ind %>% dplyr::filter(name %in% indices)

  # create output folder
  if(!dir.exists(paste0(outpath,"Indices"))){
    dir.create(paste0(outpath,"Indices"))
  }

  inputgrid <- expand.grid(x=files, y=indi_tib$name)
  # iterate through files and indices calculations
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
