## ############################## ##
##
## Script Name: Function script for analysing the differences between different classes in the imagery determined by points of polygons
##
##
## Contact (if different from above):
##
## Date Created: 2020-06-19
##
## Date Modified: 2020-06-19
##
## Licence: JNCC
##
##
## Abstract:
## If points it will extract every raster cell a point falls into (simple extract method), if a polygon it will take every cell values which falls within a polygon.
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## ggplot2_3.3.0 tidyr_1.0.3   raster_3.0-7  sp_1.3-1      purrr_0.3.4   dplyr_0.8.5
## sf_0.9-2
##
## ############################## ##
#' Extract imagery and indices differences between classes
#'
#' @param trainpath filepath the training shapefile with different classes
#' @param classname field in the training shapefile denoting where the classes are identified
#' @param imagepath path to the imagery rasterfiles, can be a file or folder path. if folder is supplied this will iterate through each file in the folder
#' @param indicespath path to where the indices for each image have been generated
#' @param tcrs the ESPG code of the coordinate reference system of the training shapefile
#'
#' @return
#' @export
#'
#' @examples
#'
#'
ClassProfile <- function(trainpath, classname, imagepath, indicespath, tcrs=27700){

  #test if imagepath is file or folder
  if (file_test("-f",imagepath)==T){
    files <- list(imagepath)
  } else {
    files <- list.files(imagepath, full.names = T, pattern=".tif")
  }

  #test if polygon or point
  training <- sf::st_read(trainpath,quiet=T) %>% sf::st_transform(tcrs)

  # extract all data per imagery tiles
  all <- purrr::map_df(files,function(satpath){
    #get tile
    granule <- raster::brick(satpath)
    #get tile name
    filename <- gsub(basename(satpath), pattern=".tif",replacement="")
    #get indices layers
    ind_files <- list.files(paste0(indicespath,filename), full.names = T, pattern=".tif")
    ind_stack <- purrr::map(ind_files,.f=raster::raster)
    all_stack<- raster::addLayer(granule, ind_stack)
    #edit layer names
    names(all_stack) <- gsub(names(all_stack),pattern=paste0("X",gsub(filename,pattern="-",replacement="."),"_"),replacement="")
    names(all_stack) <- gsub(names(all_stack),pattern=paste0("X",gsub(filename,pattern="-",replacement="."),"."),replacement="Band")

    #find intersecting points
    ext <- raster::extent(granule) %>% as("SpatialPolygons") %>% sf::st_as_sf() %>%  sf::st_set_crs(tcrs)
    #get polygons within the extent of the tile
    train_in_tile <- suppressWarnings(sf::st_intersection(training,ext))
    if(nrow(train_in_tile)!=0){
      #extract sentinel values
      satValues <- raster::extract(all_stack, train_in_tile, df=TRUE,method='simple')
      polyvals <- train_in_tile %>% raster::as.data.frame() %>%
        dplyr::mutate(ID=dplyr::row_number()) %>%
        dplyr::left_join(satValues, by="ID") %>% dplyr::select(-geometry,-ID)
    }
    return(polyvals)
  })

  #render plot of differences
  all <- all %>% dplyr::rename(Class=!!classname)
  longdat <- all %>% tidyr::gather(Variable,Value,names(all)[which(names(all)!="Class")])
  ggplot2::ggplot(data = longdat, ggplot2::aes(x=Class, y=Value)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill=Class), outlier.shape=NA)+
    ggplot2::facet_wrap(. ~ Variable, scales="free",ncol=3) +
    ggplot2::scale_fill_viridis_d(option = "D",alpha=0.7) +
    ggplot2::theme_minimal() + ggplot2::labs(fill='Class') +
    ggplot2::theme(strip.background =ggplot2::element_rect(fill="#3F9C35")) +
    ggplot2::theme(strip.text = ggplot2::element_text(colour = 'white')) +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank())


  return(all)
}
