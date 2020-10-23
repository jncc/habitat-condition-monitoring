## ############################## ##
##
## Script Name:Script for creating S1 false colour thumbnails
##
##
## Contact (if different from above):
##
## Date Created: 2020-08-12
##
## Date Modified: 2020-09-11
##
## Author: JNCC
##
## Licence: MIT licence
##
##
## Abstract: function to iterate through a folder of s1 imagery and create thumbnails plotting the first imagery band
##
##
##
## R version 3.6.1 (2019-07-05)
## Dependencies:
## tmap_3.0        purrr_0.3.4
## stringr_1.4.0   lubridate_1.7.4
## raster_3.0-7    sp_1.3-1
##
## ############################## ##


#' s1thumbs
#'
#' @param s1path filepath containing the Sentinel-1 imagery
#' @param out_folder filepath to save the outputs
#'
#' @return
#' @export
#'
#' @examples
s1thumbs <- function(s1path,out_folder=paste0(outfolder,'ZonalStats/s2/thumbs/')){
  ## create s1 false colour thumbnails
   #get imagery
   s1images <- list.files(s1path,full.names=T,pattern='.tif')
   print(paste(length(s1images), " images found."))

   #iterate through imagery
   s1_results <- purrr::map_df(s1images, .f=function(senimg){
     imgname <- gsub(basename(senimg),pattern='.tif',replacement="")
     #get date
     date <- lubridate::ymd(stringr::str_split(basename(senimg),"_")[[1]][2])

     ##false colour image
     image <- raster::brick(senimg)
     ## plot thumbnail
     image_thumbnail <- tmap::tm_shape(image[[1]]) +
       tmap::tm_raster(palette = "Greys",
                       style = "fixed",
                       breaks = seq(-30,20,by=5),midpoint=0,n=10) +
       tmap::tm_credits(paste0(as.character(date),"_S1"), size =1, col = "black", fontface = "bold",
                        position = c(0.7, 0.01)) +
       tmap::tm_legend(show = F) +
       tmap::tm_layout(outer.margins = c(0, 0, 0, 0)) +
       tmap::tm_facets(free.scales = FALSE)

     #save out
     suppressWarnings(tmap::tmap_save(image_thumbnail,filename =  paste0(out_folder,as.character(date),'_FalseColour.png'),
                                      width = 6, height = 6))


})
}
