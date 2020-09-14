## ############################## ##
##
## Script Name: Function to extract training data from a variable stack
##
## Author: Becky Trippier
##
## Contact (if different from above):
##
## Date Created: 2020-06-30
##
## Date Modified: 2020-06-30
##
## Licence: JNCC
##
##
## Abstract: Function to  Iterate through and extract points from folder of variable imagery
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
##  raster_3.0-7    sp_1.3-1   dplyr_0.8.5
##
## ############################## ##

#' #### Extract values from variables using matrices ####
#'# Iterate through and extract points from folder of variable imagery
#' @param varstack folder path to the variable layers
#' @param varnames names of variable layers
#' @param trainr rasterlayer of training values
#' @param outfolder output folder to write to
#' @param stratified optional, adds a column to the data which categorises the data based on equal intervals between 0-1
#'
#' @return
#' @export
#'
#' @examples
train_extract<- function(varstack,varnames,trainr,outfolder,stratified = T){

  # load in training raster and varstack
  train <- raster::raster(trainr)
  varlayers <- raster::stack(varstack)

  # crop stack to the training extent
  vars_proj <- raster::projectRaster(varlayers,train,method='bilinear')

  names(train) <- "bare"
  names(vars_proj) <- varnames
  trainvars <- raster::addLayer(vars_proj,train)

  #data frame of values
  train_df <- trainvars %>% raster::as.data.frame() %>% na.omit()

  #bin data into equal categories
  if(stratified==T){
    quant <- data.frame(cat=c(1:5),barepcnt=stats::quantile(0:1))
    train_df$barecat <- raster::cut(train_df$bare, c(-Inf,quant$barepcnt), labels=1:5)
  }

  #write out training values
  write.csv(train_df,paste0(outfolder, gsub('.tif',basename(varstack),replacement='.csv')))
  return(train_df)
}
