## ############################## ##
##
## Script Name: Habitat change detection functions - square bb and reduce image size
##
## Contact (if different from above):
##
## Date Created: 2020-08-24
##
## Date Modified: 2020-09-11
##
## Author: JNCC
##
## Licence: MIT licence##
##
## Abstract:
##  Create square bounding box polygon - This function creates a square bounding box polygon for a site, buffered by 10m so that it includes a margin of single raster pixels
##  Reduce image size - This function reduces the size of an image, saving the original image in an archive folder
##  create legend thumbnail - This function creates a legend image for the thumbnails
##
##
## R version 3.6.1 (2019-07-05)
## Dependencies:
## fs_1.3.1   magick_2.2 sf_0.9-2
##
##
## ############################## ##

#' Create square bounding box polygon
#'
#' This function creates a square bounding box polygon for a site,
#' buffered by 10m so that it includes a margin of single raster pixels
#'
#' @param site_polygon, sf object, site polygon or multipolygon
#'
#' @return sf object, square polygon bounding box buffered by 10m
#' @export
#'
#' @examples
create_square_bounding_box_polygon <- function(site_polygon) {

  # get bounding box for site
  bb <- sf::st_bbox(site_polygon) %>%
    as.list()

  # make square extent
  xlen <- (bb$xmax - bb$xmin) / 2
  ylen <- (bb$ymax - bb$ymin) / 2
  if (xlen > ylen) {
    ymid <- bb$ymin + ylen
    bb$ymin <- ymid - xlen
    bb$ymax <- ymid + xlen
  } else if (ylen > xlen) {
    xmid <- bb$xmin + xlen
    bb$xmin <- xmid - ylen
    bb$xmax <- xmid + ylen
  }

  # convert to matrix
  coordinates <- matrix(c(bb$xmin, bb$ymin,
                          bb$xmin, bb$ymax,
                          bb$xmax, bb$ymax,
                          bb$xmax, bb$ymin,
                          bb$xmin, bb$ymin),
                        ncol = 2, byrow = TRUE)


  # convert to polygon
  polygon <- sf::st_polygon(list(coordinates))

  # convert to sf data frame
  polygon <- sf::st_sf(FID = 1L, sf::st_sfc(polygon), crs = sf::st_crs(site_polygon))

  # buffer by 10m (single raster pixel)
  sf::st_buffer(polygon, dist = 10)
}

#' Reduce image size
#'
#' This function reduces the size of an image, saving the original
#' image in an archive folder
#'
#' @param path_image character, path to full size image
#' @param image_size character, percentage size of reduced image
#'
#' @return character, path to reduced size image
#' @export
#'
#' @examples
reduce_image_size <- function(path_image, image_size = "33%") {

  # create archive directory if does not exist
  dir_archive <- fs::path(fs::path_dir(path_image), "archive")
  if(!fs::dir_exists(dir_archive)){
    fs::dir_create(dir_archive)
  }

  # move original image to archive directory
  path_archive <- path(dir_archive, path_file(path_image))
  fs::file_move(path_image, path_archive)

  # save reduced image in original directory
  magick::image_read(path_archive) %>%
    magick::image_scale(image_size) %>%
    magick::image_write(path_image)

  # return path to reduced image
  invisible(path_image)
}


#' create legend thumbnail
#'
#' @param r raster layer
#' @param index index name, this will be applied for the output filename
#' @param breaks breaks to pass into the tm_raster function, see ?tm_raster for more info.
#' @param palette colour palette to pass into the tm_raster function, see ?tm_raster for more info.
#' @param labels labels to pass into the tm_raster function, see ?tm_raster for more info.
#' @param outfolder output folder to save the images
#' @param continuous logical, whether to plot on a continuous scale or fixed categories.
#'
#' @return image png of the legend
#' @export
#'
#' @examples
#'
legend_thumbnail <- function(r, index, breaks, palette,labels,outfolder,continuous=F){
  indices<-raster::raster(r)
  if(continuous==T){
    thumbnail_legend <- tmap::tm_shape(indices) +
      tmap::tm_raster(palette =  palette,
                      style = "cont",
                      breaks = breaks,midpoint=0,n=10,
                      labels=labels,
                      legend.is.portrait=FALSE,
                      title="",
                      legend.format=list(text.separator="-",text.to.columns=TRUE))+
      tmap::tm_legend(only=TRUE,
                      position=c("center","center")) +
      tmap::tm_facets(free.scales = FALSE)
    tmap::tmap_save(thumbnail_legend,filename =  paste0(outfolder,index,'_cont.png'),
                    width = 350, height = 55, units="px")
  } else {
    thumbnail_legend <- tmap::tm_shape(indices) +
      tmap::tm_raster(palette =  palette,
                      style = "fixed",
                      breaks = breaks,midpoint=0,n=10,
                      labels=labels,
                      legend.is.portrait=FALSE,
                      title="",
                      legend.format=list(text.separator="-",text.to.columns=TRUE))+
      tmap::tm_legend(only=TRUE,
                      height=1,
                      position=c("center","center"))+
      tmap::tm_facets(free.scales = FALSE)
    tmap::tmap_save(thumbnail_legend,filename =  paste0(outfolder,index,'_fixed.png'),
                    width = 350, height = 55, units="px")
  }

}


