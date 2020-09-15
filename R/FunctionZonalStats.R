## ############################## ##
##
## Script Name: Zonal statistics function
##
##
## Contact (if different from above):
##
## Date Created: 2020-06-29
##
## Date Modified: 2020-09-11
##
## Licence: JNCC
##
##
## Abstract: Based on the functionality of the NE Living Maps method: takes polygons as shapefiles, rasterizes using the 'fasterize' package, calculates indices from imagery and then uses 'zonal' in the raster package to compute zonal statistics. This iteratees through all the time series imagery adding a row per date per index. Where imagery for a date contains NA values (e.g. cloud masked), the function removes polygon id's which contain any na cells for that date. This is designed to run over small areas, for larger imagery handling then refer to the tiled approch from NE
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
##  fasterize_1.0.2 tmap_3.0        tibble_3.0.1  lubridate_1.7.4 raster_3.0-7    sp_1.3-1
## purrr_0.3.4     stringr_1.4.0   dplyr_0.8.5    sf_0.9-2
##
## ############################## ##



#' Zonal stats
#'
#' @param polys shapefile path of polygons
#' @param polyfield polygon id in shapefile
#' @param s2path path to folder containing the s2 imagery
#' @param s1path path to folder containing the s1 imagery
#' @param outFolder folder to save zonal statistic files in
#' @param sitename name of site,the output zonal stats file will be saved with this name
#'
#' @return
#' @export
#'
#' @examples
zonal_stats <- function(polys,polyfield='id', s2path=NA,s1path=NA, outfolder, sitename="Testsite"){

  #read in polygons and rasterize
  polygons <- sf::st_read(polys)

  #saving outputs
  if(!dir.exists(paste0(outfolder,'ZonalStats'))){
    dir.create(paste0(outfolder,'ZonalStats'))
    dir.create(paste0(outfolder,'ZonalStats/zonal_statistics'))
  }

  #iterate through each of the s2 images
  if(!is.na(s2path)){
    if(!dir.exists(paste0(outfolder,'ZonalStats/s2'))){
      dir.create(paste0(outfolder,'ZonalStats/s2'))
      dir.create(paste0(outfolder,'ZonalStats/s2/masked'))
      dir.create(paste0(outfolder,'ZonalStats/s2/thumbs'))
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
    s2_results <- purrr::map_df(s2_images$date, .f=function(senimg){
      #separate and load files
      datefiles <- s2images %>% dplyr::filter(date==senimg) %>% dplyr::select(files)
      imgfile <- datefiles %>% dplyr::filter(stringr::str_detect(datefiles$files,pattern="stdsref")) %>% as.character()
      #if more than one imgfile per date
      if(length(imgfile)>1){
        imgfile<-imgfile[1]
        print(paste0(senimg, ": has more than 1 img file, first file listed used."))
      }
      cloudfile <- datefiles %>% dplyr::filter(stringr::str_detect(datefiles$files,pattern="cloud")) %>% as.character()
      topofile <- datefiles %>% dplyr::filter(stringr::str_detect(datefiles$files,pattern="topo")) %>% as.character()

      #cloud and shadow masking
      satellite <- raster::stack(paste0(s2path,imgfile))
      cloud <- raster::raster(paste0(s2path,cloudfile))
      shadow <- raster::raster(paste0(s2path,topofile))
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
      raster::writeRaster(satellite_msk,paste0(outfolder,'ZonalStats/s2/masked/',tools::file_path_sans_ext(imgfile),"_msk.tif"),overwrite=T)

      imgname <- paste0(tools::file_path_sans_ext(imgfile),"_msk")

      #get date
      date <- lubridate::ymd(senimg)
      #calculate indices
      runIndices(paste0(outfolder,'ZonalStats/s2/masked/',tools::file_path_sans_ext(imgfile),"_msk.tif"),
                 outpath=paste0(outfolder,'ZonalStats/s2/'),
                 nir=8, r=3, b=1, g=2, swir=9,indices=c("NDVI","NDMI","NDWI"), nf=F)
      #stack indices
      ind_stack <- list(ndvi=raster::raster(paste0(outfolder,'ZonalStats/s2/Indices/',tools::file_path_sans_ext(imgfile),"_msk_NDVI.tif")),
                      ndwi=raster::raster(paste0(outfolder,'ZonalStats/s2/Indices/',tools::file_path_sans_ext(imgfile),"_msk_NDWI.tif")),
                      ndmi=raster::raster(paste0(outfolder,'ZonalStats/s2/Indices/',tools::file_path_sans_ext(imgfile),"_msk_NDMI.tif")))

    ## create thumbnails
      #indices
    purrr::map(ind_stack,.f=function(thumbs){
      strindex <- stringr::str_split(names(thumbs),"_")
      indexname <- strindex[[1]][length(strindex[[1]])]
      #mapping lookup
      lookup <- tibble::tribble(~index,~breaks,~palette,
                                "NDVI", seq(-1,1,by=0.2),terrain.colors(9,rev=T),
                                "NDWI",seq(-1,1,by=0.2),"Blues",
                                "NDMI",seq(-1,1,by=0.2),"Blues")
      indexbreak <- lookup %>% dplyr::filter(index==indexname)
      tmap::tmap_options(show.messages = FALSE)
      image_thumbnail <- tmap::tm_shape(thumbs) +
        tmap::tm_raster(palette = unlist(indexbreak$palette),
                        style = "fixed",
                        breaks = unlist(indexbreak$breaks),midpoint=0,n=10) +
        tmap::tm_credits(paste0(as.character(date),"_",indexname), size = 1, col = "black", fontface = "bold",
                       position = c(0.7, 0.01)) +
      tmap::tm_legend(show = F) +
      tmap::tm_layout(outer.margins = c(0, 0, 0, 0)) +
        tmap::tm_facets(free.scales = FALSE)
      tmap::tmap_save(image_thumbnail,filename =  paste0(outfolder,'ZonalStats/s2/thumbs/',as.character(date),"_",indexname,'.png'),
              width = 6, height = 6)
    })
      #rgb image
    sat_img <- satellite_msk[[1:3]]
    sat_img[sat_img>255]<-255
    rgb_thumb <- suppressWarnings(tmap::tm_shape(sat_img) + tmap::tm_rgb(r=3, b=1, g=2,max.value = 255) +
      tmap::tm_credits(paste0(as.character(date),"_S2"), size = 1, col = "black", fontface = "bold",
                       position = c(0.7, 0.01)) + tmap::tm_layout(outer.margins = c(0, 0, 0, 0)))
    suppressWarnings(tmap::tmap_save(rgb_thumb,filename =  paste0(outfolder,'ZonalStats/s2/thumbs/',as.character(date),'_RGB.png'),
                    width = 6, height = 6))

    #rasterize polygons
    rtemplate <-raster::raster(crs=raster::crs(polygons),res=raster::res(ind_stack$ndvi),ext=raster::extent(ind_stack$ndvi))
    poly_tif <- fasterize::fasterize(polygons,rtemplate, field=polyfield)

    ## find rows with nas and remove poly id
    all_layers <- append(ind_stack, poly_tif)
    all_layers_df <- raster::as.data.frame(raster::stack(all_layers))
    na_row <- all_layers_df %>% dplyr::filter(!is.na(layer) & !complete.cases(all_layers_df))
    #if nas present
    if(nrow(na_row)>0){
      poly_id <- unique(na_row$layer)
      print(paste0("Polygons containing nas: ", length(poly_id)))
      #remove polygon id where polygon contains cells with nas
      poly_tif[poly_tif %in% poly_id] <- NA
      } else {
        print("no nas in polygons")
      }

    #calculate zonal statistics with raster package function 'zonal'
    zonal_df <- purrr::map_df(ind_stack, .f=function(indices){
      ind <- indices
      indexname <- names(indices) %>% stringr::str_replace(pattern=imgname,replacement="") %>% stringr::str_replace("_","")
      stats <- data.frame(raster::zonal(ind, poly_tif,fun = 'mean',na.rm=T)) %>%
        dplyr::mutate(sd = data.frame(raster::zonal(ind, poly_tif, fun = 'sd',na.rm=T))[,2]) %>%
        dplyr::mutate(median = data.frame(raster::zonal(ind, poly_tif, fun = 'median',na.rm=T))[,2]) %>%
        dplyr::mutate(min = data.frame(raster::zonal(ind, poly_tif, fun = 'min',na.rm=T))[,2])   %>%
        dplyr::mutate(max = data.frame(raster::zonal(ind, poly_tif, fun = 'max',na.rm=T))[,2])  %>%
        dplyr::mutate(Q1 = data.frame(raster::zonal(ind, poly_tif, fun = 'quantile',na.rm=T))[,3]) %>%
        dplyr::mutate(Q3 = data.frame(raster::zonal(ind, poly_tif, fun = 'quantile',na.rm=T))[,5]) %>%
        dplyr::rename(ID="zone") %>%
        dplyr::mutate(index=indexname) %>% #add index name
        dplyr::mutate(date= lubridate::ymd(date))  %>% # add date column
        dplyr::mutate(month = lubridate::month(date), year = lubridate::year(date)) %>% #add month and date column
        dplyr::mutate(season = dplyr::case_when(month %in% c(12,1,2) ~ "Winter",month %in% c(3,4,5) ~ "Spring",month %in% c(6,7,8) ~ "Summer",month %in% c(9,10,11) ~ "Autumn",TRUE ~ NA_character_)) %>% #add season column
        dplyr::mutate(seasonyear = ifelse(month==1 | month==2,stringr::str_glue("{season}_{year-1}"),stringr::str_glue("{season}_{year}"))) #add seasonyear column

    })
    #write out result from image date
    write.csv(zonal_df ,paste0(outfolder,"ZonalStats/zonal_statistics/s2_zonal_stats_",as.character(date),".txt"))
    unlink(paste0(outfolder,'ZonalStats/s2/Indices/'))
    return(zonal_df)
  })
  } else { s2_results <- NA}


  #iterate through each of the s1 images
  if(!is.na(s1path)){

    if(!dir.exists(paste0(outfolder,'ZonalStats/s1'))){
      dir.create(paste0(outfolder,'ZonalStats/s1/'))
      dir.create(paste0(outfolder,'ZonalStats/s1/thumbs/'))

    }
    #get imagery
    s1images <- list.files(s1path,full.names=T,pattern='.tif')
    print(paste(length(s1images), " images found."))

    #iterate through imagery
    s1_results <- purrr::map_df(s1images, .f=function(senimg){
      imgname <- gsub(basename(senimg),pattern='.tif',replacement="")
      #get date
      date <- lubridate::ymd(stringr::str_split(basename(senimg),"_")[[1]][2])
      #calculate indices
      runS1Indices(imagepath=senimg,
                 outpath=paste0(outfolder,'ZonalStats/s1/'),
                 vv=1, vh=2,
                 indices=c('RVI','RVIv'))
      #stack indices
      ind_stack <- list(rvi=raster::raster(paste0(outfolder,'ZonalStats/s1/Indices/',gsub(basename(senimg),pattern='.tif',replacement='_RVI.tif'))),
                        rviv=raster::raster(paste0(outfolder,'ZonalStats/s1/Indices/',gsub(basename(senimg),pattern='.tif',replacement='_RVIv.tif'))))

      ## create thumbnails
      purrr::map(ind_stack,.f=function(thumbs){
        strindex <- stringr::str_split(names(thumbs),"_")
        indexname <- strindex[[1]][length(strindex[[1]])]
        #mapping lookup
        lookup <- tibble::tribble(~index,~breaks,
                                  "RVI", seq(0,2,by=0.1),
                                  "RVIv",seq(0,5,by=0.2))
        indexbreak <- lookup %>% dplyr::filter(index==indexname)
        tmap::tmap_options(show.messages = FALSE)
        image_thumbnail <- tmap::tm_shape(thumbs) +
          tmap::tm_raster(palette = "viridis",
                          style = "fixed",
                          breaks = unlist(indexbreak$breaks)) +
          tmap::tm_credits(paste0(as.character(date),"_",indexname), size = 1, col = "black", fontface = "bold",
                           position = c(0.7, 0.01)) +
          tmap::tm_legend(show = FALSE) +
          tmap::tm_layout(outer.margins = c(0, 0, 0, 0))
        tmap::tmap_save(image_thumbnail,filename =  paste0(outfolder,'ZonalStats/s1/thumbs/',as.character(date),"_",indexname,'.png'),
                        width = 6, height = 6)
      })

      ## create s1 false colour thumbnails
      ##false colour image
      image <- raster::raster(senimg, band=1)
      ## plot thumbnail
      image_thumbnail <- tmap::tm_shape(image) +
        tmap::tm_raster(palette = "Greys",
                        style = "fixed",
                        breaks = seq(-30,20,by=5),midpoint=0,n=10) +
        tmap::tm_credits(paste0(as.character(date),"_S1"), size =1, col = "black", fontface = "bold",
                         position = c(0.7, 0.01)) +
        tmap::tm_legend(show = F) +
        tmap::tm_layout(outer.margins = c(0, 0, 0, 0)) +
        tmap::tm_facets(free.scales = FALSE)
      #save out
      suppressWarnings(tmap::tmap_save(image_thumbnail,filename =  paste0(outfolder,'ZonalStats/s1/thumbs/',as.character(date),'_FalseColour.png'),
                                       width = 6, height = 6))

      #rasterize polygons
      rtemplate <-raster::raster(crs=raster::crs(polygons),res=raster::res(ind_stack$rvi),ext=raster::extent(ind_stack$rvi))
      poly_tif <- fasterize::fasterize(polygons,rtemplate, field=polyfield)

      #calculate zonal statistics with raster package function 'zonal'
      zonal_s1 <- purrr::map_df(ind_stack, .f=function(indices){
        ind <- indices
        indexname <- names(indices) %>% stringr::str_replace(pattern=imgname,replacement="") %>% stringr::str_replace("_","")
        stats <- data.frame(raster::zonal(ind, poly_tif,fun = 'mean',na.rm=T)) %>%
          dplyr::mutate(sd = data.frame(raster::zonal(ind, poly_tif, fun = 'sd',na.rm=T))[,2]) %>%
          dplyr::mutate(median = data.frame(raster::zonal(ind, poly_tif, fun = 'median',na.rm=T))[,2]) %>%
          dplyr::mutate(min = data.frame(raster::zonal(ind, poly_tif, fun = 'min',na.rm=T))[,2])   %>%
          dplyr::mutate(max = data.frame(raster::zonal(ind, poly_tif, fun = 'max',na.rm=T))[,2])  %>%
          dplyr::mutate(Q1 = data.frame(raster::zonal(ind, poly_tif, fun = 'quantile',na.rm=T))[,3]) %>%
          dplyr::mutate(Q3 = data.frame(raster::zonal(ind, poly_tif, fun = 'quantile',na.rm=T))[,5]) %>%
          dplyr::rename(ID="zone") %>%
          dplyr::mutate(index=indexname) %>% #add index name
          dplyr::mutate(date= lubridate::ymd(date))  %>% # add date column
          dplyr::mutate(month = lubridate::month(date), year = lubridate::year(date)) %>% #add month and date column
          dplyr::mutate(season = dplyr::case_when(month %in% c(12,1,2) ~ "Winter",month %in% c(3,4,5) ~ "Spring",month %in% c(6,7,8) ~ "Summer",month %in% c(9,10,11) ~ "Autumn",TRUE ~ NA_character_)) %>% #add season column
          dplyr::mutate(seasonyear = ifelse(month==1 | month==2,stringr::str_glue("{season}_{year-1}"),stringr::str_glue("{season}_{year}"))) #add seasonyear column
      })
      #write out result from image date
      write.csv(zonal_s1 ,paste0(outfolder,"ZonalStats/zonal_statistics/s1_zonal_stats_",as.character(date),".txt"))
      return(zonal_s1)
      })
  } else { s1_results <- NA}

  #combine and write out results
  all_img_results <- rbind(s2_results,s1_results)
  write.csv(all_img_results ,paste0(outfolder,"ZonalStats/",sitename,"_zonal_stats",".txt"))
}



