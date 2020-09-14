## ############################## ##
##
## Script Name: Function to rename APGB imagery with dates flown
##
## Author: Becky Trippier
##
## Contact (if different from above):
##
## Date Created: 2020-07-14
##
## Date Modified:2020-09-11
##
## Licence: JNCC
##
##
## Abstract: Function to iterate through imagery and rename with a lookup date.
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## purrr_0.3.4 tidyr_1.0.3 sf_0.9-2    dplyr_0.8.5
##
## ############################## ##


#' APGBdate
#'
#' @param imgfolder folder path containing the imagery to rename
#' @param lookup a lookup shapefile to reference dates
#' @param field field within the shapefile containing the dates
#'
#' @return
#' @export
#'
#' @examples
APGBdate <- function(imgfolder,lookup,field='OS'){

  # read in data
  date_lookup <- sf::st_read(lookup,quiet=T)
  imgs <- list.files(imgfolder,pattern='tif',full.names = T)

  #get dates
  date_df <- date_lookup %>% sf::st_drop_geometry() %>% dplyr::select(field,DateFlown) %>%
    tidyr::separate(DateFlown,'date',sep=",",extra="drop")
  # iterate through and rename
  purrr::map(imgs,.f=function(imgname){
    date <- date_df %>% dplyr::filter(get(field) == gsub(basename(imgname),pattern='.tif',replacement=""))
    file.rename(imgname, paste0(dirname(imgname),'/',as.character(date$date),'_',basename(imgname)))
    imgname
  })

}
