## ############################## ##
##
## Script Name: Various functions used for preparing the change statistics outputs from the 'change_stats' function and flagging change
##
##
##
## Date Created: 2020-08-11
##
## Date Last Modified: 2020-10-09
##
## Author: JNCC
##
## Licence: MIT licence
##
##
## Abstract:
## FileEdit - Define function to edit the zonal stats txt files to remove additional rownames column and add a monthdate column
## FileEdit2 -  Define function to write the habitat names into the zonal stats txt files
## flag_change - Function to flag change using the outputs of the 'change_stats' function
##

##
## R version 3.6.1 (2019-07-05)
## Dependencies:
##   tidyverse 1.3.0
##     ggplot2 3.3.0
##     tibble  3.0.1
##     tidyr   1.0.2
##     readr   1.3.1
##     purrr   0.3.3
##     dplyr   0.8.5
##     stringr 1.4.0
##     forcats 0.4.0
##   pbapply 1.4.2
##   yaml 2.2.0
##   sf 0.9.3
##

## ############################## ##


#' Define function to edit the zonal stats txt files
#'
#' @param filePath list of filepaths within the app data folder
#'
#' @return overwrites files with edited csvs
#' @export
#'
#' @examples
#'
fileEdit <- function(filePath){
  if(grepl(filePath, pattern = "zonal_stats")){

    file <- suppressMessages(readr::read_csv(filePath, progress = F))

    ## Remove the additional rownames column
    if("X1" %in% colnames(file)){
      file <- file %>%
        select(-X1)
    }

    ## Add in the monthdate column if it's missing
    if(("month" %in% colnames(file)) & !("monthdate" %in% colnames(file))){
      file <- file %>%
        dplyr::left_join(tibble(monthname = month.abb, month = 1:12)) %>%
        dplyr::mutate(monthdate = paste0(monthname, " ", year)) %>%
        dplyr::select(-monthname)
    }

    readr::write_csv(file, filePath)
  }

}

#' Define function to write the habitat names into the zonal stats txt files
#'
#' @param filePath list of files in the app data folder
#' @param config yaml config file with app data structure
#'
#' @return overwrites files with edited csvs
#' @export
#'
#' @examples
#'
fileEdit2 <- function(filePath, config){
  if(grepl(filePath, pattern = "zonal_stats/polygon")){
    print(filePath)
    file <- suppressMessages(readr::read_csv(filePath, progress = F))

    if(!("HABITAT" %in% colnames(file))){

      site <- filePath %>%
        stringr::str_remove(pattern = "./data/") %>%
        stringr::str_remove(pattern = "/[A-z1-9/.]+")

      framework <- filePath %>%
        stringr::str_remove(pattern = paste0("./data/", site, "/")) %>%
        stringr::str_remove(pattern = "/[A-z1-9/.]+")

      shp <- sf::st_read(list.files(paste0("./data/", site, "/", framework, "/spatial_framework/"), pattern = "shp$", full.names = T), quiet = T)

      fId <- lapply(config[[site]]$spatial_framework, function(x){
        x$framework_app == framework
      }) %>% unlist %>% which

      habCol <- config[[site]]$spatial_framework[[fId]]$habcol
      idCol <- config[[site]]$spatial_framework[[fId]]$idcol

      shp <- shp %>%
        sf::st_drop_geometry()%>%
        dplyr::select(idCol, habCol) %>%
        dplyr::rename(ID = idCol,
                      HABITAT = habCol)



      file <- suppressMessages(dplyr::left_join(file, shp))

      readr::write_csv(file, filePath)
    }
  }
}

#' Function to flag change using the outputs of the 'change_stats' function
#'
#' @param files vector of zonal stats files to iterate through
#' @param indices vector of indices names
#'
#' @return csv files flagging change
#' @export
#'
#' @examples
#'
flag_change <- function(files, indices){
  ## Create a df to work through
  runList <- expand.grid(files = files, indices = indices, stringsAsFactors = F)

  ## For each row in the new df
  for(i in 1:nrow(runList)){

    ## Read in the polygon level data
    poly <- suppressMessages(readr::read_csv(paste0(runList$files[i], "polygon/", runList$indices[i], ".txt")))

    ## Read in the habitat level data
    hab <- suppressMessages(readr::read_csv(paste0(runList$files[i], "habitat/", runList$indices[i], ".txt"))) %>%
      dplyr::rename(HABITAT = `get(habclass)`)

    ## Check just to make sure there are no negative SD values
    if(any(na.omit(c(hab$hab_meansd, hab$hab_mediansd, hab$hab_Q1sd, hab$hab_Q3sd))<0)){
      stop()
    }

    ## Join the data
    polyHab <- dplyr::left_join(poly, hab)

    ## If the data is monthly
    if(grepl(runList$files[i], pattern = "monthly")){

      ## First flag a polygon if it is more than 1SD different from the habitat mean, calling it "1SD" if it is, and "No change" if it's not
      ## Then flag any polygon more than 2SD different from the habitat mean, calling it "2SD" if it is, and retaining the old entry to the change column if it's not
      polyHab <- polyHab %>%
        dplyr::mutate(meanChange = ifelse(mean < (hab_mean - hab_meansd) | mean > (hab_mean + hab_meansd), "1SD", "No change")) %>%
        dplyr::mutate(meanChange = ifelse(mean < (hab_mean - (hab_meansd*2)) | mean > (hab_mean + (hab_meansd*2)), "2SD", meanChange)) %>%
        dplyr::mutate(medianChange = ifelse(median < (hab_median - hab_mediansd) | median > (hab_median + hab_mediansd), "1SD", "No change")) %>%
        dplyr::mutate(medianChange = ifelse(median < (hab_median - (hab_mediansd*2)) | median > (hab_median + (hab_mediansd*2)), "1SD", medianChange)) %>%
        dplyr::mutate(Q1Change = ifelse(Q1 < (hab_Q1 - hab_Q1sd) | Q1 > (hab_Q1 + hab_Q1sd), "1SD", "No change")) %>%
        dplyr::mutate(Q1Change = ifelse(Q1 < (hab_Q1 - (hab_Q1sd*2)) | Q1 > (hab_Q1 + (hab_Q1sd*2)), "2SD", Q1Change)) %>%
        dplyr::mutate(Q3Change = ifelse(Q3 < (hab_Q3 - hab_Q3sd) | Q3 > (hab_Q3 + hab_Q3sd), "1SD", "No change")) %>%
        dplyr::mutate(Q3Change = ifelse(Q3 < (hab_Q3 - (hab_Q3sd*2)) | Q3 > (hab_Q3 + (hab_Q3sd*2)), "2SD", Q3Change)) %>%
        dplyr::select(ID, index, month, year, monthdate, date, HABITAT, meanChange, medianChange, Q1Change, Q3Change)

      ## Else if the data is seasonal
    } else {

      ## First flag a polygon if it is more than 1SD different from the habitat mean, calling it "1SD" if it is, and "No change" if it's not
      ## Then flag any polygon more than 2SD different from the habitat mean, calling it "2SD" if it is, and retaining the old entry to the change column if it's not
      polyHab <- polyHab %>%
        dplyr::mutate(meanChange = ifelse(mean < (hab_mean - hab_meansd) | mean > (hab_mean + hab_meansd), "1SD", "No change")) %>%
        dplyr::mutate(meanChange = ifelse(mean < (hab_mean - (hab_meansd*2)) | mean > (hab_mean + (hab_meansd*2)), "2SD", meanChange)) %>%
        dplyr::mutate(medianChange = ifelse(median < (hab_median - hab_mediansd) | median > (hab_median + hab_mediansd), "1SD", "No change")) %>%
        dplyr::mutate(medianChange = ifelse(median < (hab_median - (hab_mediansd*2)) | median > (hab_median + (hab_mediansd*2)), "2SD", medianChange)) %>%
        dplyr::mutate(Q1Change = ifelse(Q1 < (hab_Q1 - hab_Q1sd) | Q1 > (hab_Q1 + hab_Q1sd), "1SD", "No change")) %>%
        dplyr::mutate(Q1Change = ifelse(Q1 < (hab_Q1 - (hab_Q1sd*2)) | Q1 > (hab_Q1 + (hab_Q1sd*2)), "2SD", Q1Change)) %>%
        dplyr::mutate(Q3Change = ifelse(Q3 < (hab_Q3 - hab_Q3sd) | Q3 > (hab_Q3 + hab_Q3sd), "1SD", "No change")) %>%
        dplyr::mutate(Q3Change = ifelse(Q3 < (hab_Q3 - (hab_Q3sd*2)) | Q3 > (hab_Q3 + (hab_Q3sd*2)), "2SD", Q3Change)) %>%
        dplyr::select(ID, index, seasonyear, date, HABITAT, meanChange, medianChange, Q1Change, Q3Change)

    }

    ## Write out the new change statistics
    readr::write_csv(polyHab, path = paste0(dirname(runList$files[i]), "/change_stats/", runList$indices[i], ".txt"))

  }
}

