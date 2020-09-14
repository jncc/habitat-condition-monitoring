## ############################## ##
##
## Script Name: change statistics function
##
## Author: Becky Trippier
##
## Contact (if different from above):
##
## Date Created: 2020-08-05
##
## Date Modified:2020-09-11
##
## Licence: JNCC
##
##
## Abstract: calculates monthly, seasonal summaries per polygon, then calculates summary statistics per habitat type.
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
## dplyr_0.8.5 sf_0.9-2
##
## ############################## ##

#' change_stats
#'
#' @param sitefile a text file containing the calculated zonal statistics for a site, output from the 'zonal_stats' function
#' @param outfolder folder path of where to save outputs
#' @param polygons shapefile of the polygons
#' @param polyid field in the polygon shapefile containing a unique identifier
#' @param habclass field in the polygon shapefile containing the habitat class
#'
#' @return
#' @export
#'
#' @examples
change_stats <- function(sitefile,outfolder,polygons,polyid='id',habclass='a_pred'){

  #load in the files
  sitename <- gsub(basename(sitefile),pattern='_zonal_stats.txt',replacement='')
  zonaloutput <- read.csv(sitefile)
  polys <- sf::st_read(polygons,quiet=T) %>% sf::st_drop_geometry() %>% dplyr::select(polyid,habclass)

  #get indices names
  indices <- as.character(unique(zonaloutput$index))

  #iterate through indices
  purrr::map(indices, .f=function(ind){

    ## MONTHLY STATISTICS
    if(!dir.exists(paste0(outfolder,'Monthly_statistics'))){
      dir.create(paste0(outfolder,'Monthly_statistics'))
    }
    # group by month and year
    monthly <- zonaloutput %>% dplyr::filter(index==ind) %>%
      dplyr::group_by(ID,index,month, year) %>%
      dplyr::summarise(mean = mean(mean),sd = mean(sd),median = mean(median),
                       min = min(min),max = max(max),
                       Q1 = mean(Q1),Q3 = mean(Q3),
                       date= paste(date,collapse=','))
    #write out
    write.csv(monthly,paste0(outfolder,"Monthly_statistics/",sitename,'_',ind,"_monthly_stats.txt"))


    ## MONTHLY CHANGE STATISTICS
    month_stat <- dplyr::left_join(monthly,polys,by=c('ID'=polyid)) %>%
      dplyr::ungroup()
    month_year <- month_stat %>%
      dplyr::mutate(month=ifelse(month<10,as.character(paste0("0",month)),as.character(month))) %>%
      dplyr::mutate(year=as.character(year)) %>%
      dplyr::mutate(monthname = stringr::str_glue("{year}-{month}")) %>%
      dplyr::mutate(monthdate = zoo::as.yearmon(monthname))

    mean_hab <- month_year  %>% dplyr::group_by(monthdate,get(habclass)) %>%
      dplyr::summarise(hab_mean=mean(mean),hab_meansd=sd(mean),hab_median=mean(median),hab_mediansd=sd(median),hab_min=mean(min),hab_minsd=sd(min),hab_max=mean(max),hab_maxsd=sd(max),hab_Q1=mean(Q1),hab_Q1sd=sd(Q1),hab_Q3=mean(Q3),hab_Q3sd=sd(Q3))
    write.csv(mean_hab,paste0(outfolder,"Monthly_statistics/",sitename,'_',ind,'_monthly_changestats.txt'))


    ## SEASONAL STATISTICS
    if(!dir.exists(paste0(outfolder,'Seasonal_statistics'))){
      dir.create(paste0(outfolder,'Seasonal_statistics'))
    }
    # group by season
    season <-zonaloutput  %>% dplyr::filter(index==ind) %>%
      dplyr::group_by(ID,index,seasonyear) %>%
      dplyr::summarise(mean = mean(mean),sd = mean(sd),median = mean(median),
                       min = min(min),max = max(max),
                       Q1 = mean(Q1),Q3 = mean(Q3),
                       date= paste(date,collapse=','))
    #write out
    write.csv(season,paste0(outfolder,"Seasonal_statistics/",sitename,'_',ind,"_seasonal_stats.txt"))

    ## SEASONAL CHANGE STATISTICS
    all_stat <- dplyr::left_join(season,polys,by=c('ID'=polyid))
    #calculate mean season stat per year per habitat type
    mean_hab <- all_stat %>%  dplyr::group_by(seasonyear,get(habclass)) %>%
      dplyr::summarise(hab_mean=mean(mean),hab_meansd=sd(mean),hab_median=mean(median),hab_mediansd=sd(median),hab_min=mean(min),hab_minsd=sd(min),hab_max=mean(max),hab_maxsd=sd(max),hab_Q1=mean(Q1),hab_Q1sd=sd(Q1),hab_Q3=mean(Q3),hab_Q3sd=sd(Q3))
    write.csv(mean_hab,paste0(outfolder,"Seasonal_statistics/",sitename,'_',ind,"_seasonal_changestats.txt"))

    print(paste(ind, "done."))
  }) # close iteration

}
