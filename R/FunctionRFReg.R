## ############################## ##
##
## Script Name: Regression modelling
##
##
## Contact (if different from above):
##
## Date Created: 2019-11-15
##
## Date Modified:2019-09-11
##
## Licence: JNCC
##
##
## Abstract: function to run through random forest regression modelling with a given set of training data and timeseries of environmental variables. This uses stratified sampling to sample data before splitting out into training and test datasets, the proportion of which is defined in the 'prop.test' argument. This trains the model and predicts across the time series data. 'max_tries' defines how many times to run the model and then the predicted maps are meaned at the end to alleviate some sampling bias. The RMSE, Rsquared and variable importance statistics are reported for each model run.
##
##
##
## R version 3.6.0 (2019-04-26)
## Dependencies:
##  tidyr_1.0.3         stringr_1.4.0
## randomForest_4.6-14 caret_6.0-84
## lattice_0.20-38     ggplot2_3.3.0
## dplyr_0.8.5         raster_3.0-7
## sp_1.3-1            purrr_0.3.4
##
## ############################## ##


#' RFReg
#'
#' @param training csv file containing training data
#' @param varpredict folder path to the time series variable stacks
#' @param varnames names of variable layers
#' @param out.folder folder path to save the outputs
#' @param max_tries number of model runs
#' @param prop.test proportion of data to be held back to evaluate the model performance
#' @param nsamp number of points from the training data to attempt to sample
#' @param resamp numeric, feeds into the caret::trainControl parameter of the RF model. The number of times  K-fold cross-validation is repeated for tuning parameters for the model.
#' @param stratified logical, whether to use stratified sampling to obtain training data.Note: currently sampling is expecting a field called 'bare' for sampling and 'barecat' for stratified sampling.
#' @param fillsamp logical, whether to fill remaining sampling points from other categories where one category doesnt have enough points
#'
#' @return
#' @export
#'
#' @examples
RFReg <- function(training, varpredict,varnames,out.folder,max_tries=5,prop.test=0.25,nsamp=1000,resamp=5,stratified=T,fillsamp=F){

  ##### set up   #####

  all_evals <- NULL
  all_imp <- NULL
  if (!file.exists(paste(out.folder,"Outputs",sep=""))){
    dir.create(paste(out.folder,"Outputs",sep=""))
  }
  if (!file.exists(paste(out.folder,"Outputs/modpredict",sep=""))){
    dir.create(paste(out.folder,"Outputs/modpredict",sep=""))
  }

  #get training data
  trainvals <- read.csv(training, header=TRUE)

  ##### build time series varstacks ####
  varstacks <- list.files(varpredict,full.names =T,pattern='.tif')
  #iterate through loading these in
  vars <- purrr::map(varstacks,.f=function(tif){
    varstacked <- raster::brick(tif)
    names(varstacked) <-varnames
    varstacked
  })
  varstack_names <- gsub('.tif',basename(varstacks),replacement="")
  names(vars) <- varstack_names

  #### iterate over number of max_tries ####
  for (i in 1:max_tries){
    ptm <- proc.time()
    #### split out into training and test data ####
    if (stratified==F){
      trainvals <- trainvals %>% dplyr::select("bare",varnames)
      if(!is.na(nsamp)){ # if a number to sample is given sample that number otherwise all values
        trainno <- sample(nrow(trainvals),nsamp)
        trains <- trainvals[trainno,]
      } else{
        trains <- trainvals
      }
      #split into training and test
      trainssamp <- sample(nrow(trains), ceiling(prop.test * (nrow(trains))))
      training.data <- trains[-trainssamp,] %>% dplyr::rename(response = bare)
      test.data <- trains[trainssamp,] %>% dplyr::rename(response = bare)
      print(paste("Training data:", nrow(training.data),"Test.data",nrow(test.data)))
    } else {
      #sample by category - in the RF reg function
      trainvals <- trainvals %>% dplyr::select("bare","barecat",varnames)
      ntrain <- trainvals %>% dplyr::group_by(barecat) %>% dplyr::summarise(count=n())
      nsampdf <- data.frame(barecat=ntrain$barecat,totn=ntrain$count,nsamp=c(nsamp/length(unique(trainvals$barecat)))) %>% dplyr::mutate(sampled=ifelse(totn>nsamp,nsamp,totn))

      #update sampling numbers if fill in data selected
      if(fillsamp==T ){
        while(sum(nsampdf$sampled) < nsamp){
          remaining <- sum(nsampdf$nsamp-nsampdf$sampled)
          catnotfull<-nsampdf %>% dplyr::filter(nsamp==sampled) %>% dplyr::select(barecat)
          addval <- round(remaining/length(catnotfull$barecat),0) #get number to add on
          nsampdf <- nsampdf %>% dplyr::mutate(nsamp=ifelse(nsampdf$barecat %in% catnotfull$barecat,nsamp+addval,nsamp))
          #see if sample size available
          nsampdf <- nsampdf %>% dplyr::mutate(sampled=ifelse(totn>nsamp,nsamp,totn))

        }
      }

      #sample data
      training.data <- NULL
      test.data <- NULL
      for(s in unique(nsampdf$barecat)){
        traincatdf <- trainvals %>% dplyr::filter(trainvals$barecat==s)
        samp <- nsampdf %>% dplyr::filter(nsampdf$barecat==s) %>% dplyr::select(sampled) %>% as.numeric()
        traincatsamp <- traincatdf[sample(nrow(traincatdf),samp),]
        #split into training and test
        trainssamp <- sample(nrow(traincatsamp), ceiling(prop.test * (nrow(traincatsamp))))
        vals_train <- traincatsamp[-trainssamp,] %>% dplyr::rename(response = bare) %>% dplyr::select(-barecat)
        vals_test <- traincatsamp[trainssamp,] %>% dplyr::rename(response = bare) %>% dplyr::select(-barecat)
        training.data <- rbind(training.data, vals_train)
        test.data <- rbind(test.data, vals_test)
      }
    }

    #reporting on training data
      print(paste("Training data:", nrow(training.data),"Test.data",nrow(test.data)))
      all.data <- rbind(training.data,test.data)
      sink(paste0(out.folder, "Outputs/modpredict/trainingdata_",max_tries,"_",i,".txt"))
      cat("Sampled from binned data:\n")
      print(nsampdf)
      sink()
      #create histogram plot
      hist(all.data$response)
      ggplot2::ggplot(all.data, ggplot2::aes(response)) +
        ggplot2::geom_histogram(bins=10)
      ggplot2::ggsave(paste0(out.folder, "Outputs/modpredict/trainingdata_",max_tries,"_",i,".png"),height=6,width=7)

    #### train Random Forest model ####
      cat("tuning mtrys...                                      ")
      nameRF<- paste("RF",i,sep="")
      mtry <- tryCatch(randomForest::tuneRF(x = training.data[, 2:(ncol(training.data))], y = training.data$response, trace = FALSE,plot = FALSE), error = function(err) NA)
      best.m <- tryCatch(mtry[mtry[, 2] == min(mtry[, 2]), 1],error = function(err) NA)
      tunegrid <- expand.grid(.mtry=best.m)
      cat("Training random forest...                             \n ")
      if (resamp==0){
        RF <- caret::train(response ~ ., training.data,method = "rf",tuneGrid=tunegrid,trControl = trainControl(method = "none",verboseIter = T))
      } else {
        RF <- caret::train(response ~ ., training.data,method = "rf",tuneGrid=tunegrid,trControl = trainControl(method = "repeatedcv", number = resamp,repeats = resamp,verboseIter = T))
      }
      model <- list(RF$finalModel)
      names(model)  <- nameRF
      save(model, file = paste(out.folder, "Outputs/modpredict/RFmodel_",max_tries,"_",i, sep = ""))
      #model evaluation
      RF_pred <- predict(RF, test.data)
      RF_eval<-list(caret::postResample(pred = RF_pred, obs = test.data$response)) #tset rmse
      names(RF_eval) <- nameRF
      utils::write.csv(RF_eval, file = paste(out.folder, "Outputs/modpredict/RFevals_",max_tries,"_",i,".csv", sep = ""))
      #model importance values
      RFimp <-tibble::rownames_to_column(data.frame(randomForest::importance(RF$finalModel)),"var")
      names(RFimp) <- paste(nameRF,names(RFimp),sep="_")
      utils::write.csv(RFimp, file = paste(out.folder, "Outputs/modpredict/RFimps_",max_tries,"_",i,".csv", sep = ""))

      #####  model prediction over time series ####
      cat("Predicting random forest...                            \n ")
      purrr::map(names(vars), .f=function(varstack){
        timevars <- vars[[varstack]]
        RFpredimage <-predict(timevars, RF$finalModel, filename=paste(out.folder, "Outputs/modpredict/",varstack,"_",i,".tif",sep=""), progress='text', format='GTiff', datatype='FLT4S', type='response', overwrite=TRUE)
      })
      all_evals <- append(all_evals, RF_eval)
      all_imp <- append(all_imp, RFimp)
      message("Random forest Regression model run ",i, " completed.")
  }


  # save models and evals before prediction
  utils::write.csv(all_evals, file = paste(out.folder, "Outputs/evals_",max_tries,".csv", sep = ""))
  utils::write.csv(all_imp, file = paste(out.folder, "Outputs/imps_",max_tries,".csv", sep = ""))


  #### mean across the predictions ####
  if(max_tries>1){
    predimages <- list.files(paste(out.folder, "Outputs/modpredict/",sep=""),pattern='tif',full.names = T)
    purrr::map(names(vars),.f=function(timepoint){
      timepoint_pred <- predimages[which(stringr::str_detect(predimages,timepoint))]
      predstack<- purrr::map(timepoint_pred,raster)
      predstacked <- raster::stack(predstack)
      meanpred<- raster::calc(predstacked,mean,progress='text')
      raster::writeRaster(meanpred,paste(out.folder, "Outputs/",timepoint,"_barepeat.tif",sep = ""),overwrite=T)
      print(paste0("mean prediction ",timepoint," done."))
    })
  }

#### evaluation  ####
    #read in data
    evals <- read.csv(paste0(out.folder,"Outputs/evals_",max_tries,".csv"))
    ## RMSE
    #tidy values
    RMSE <- evals %>% dplyr::filter(X=="RMSE") %>% tidyr::gather(mod,RMSE,2:ncol(evals)) %>% dplyr::select(-X) %>% dplyr::mutate(group=stringr::str_extract(mod, "[aA-zZ]+"))
    #create plot
    ggplot2::ggplot(RMSE, ggplot2::aes(x=group, y=RMSE, fill=group)) +
      ggplot2::geom_boxplot(show.legend = FALSE)  +
      ggplot2::scale_fill_brewer(palette="Blues") + ggplot2::theme_classic()
    ggplot2::ggsave(paste0(out.folder,"Outputs/RMSE.PNG"),plot = ggplot2::last_plot())
    #find best model
    best <- RMSE[which(RMSE$RMSE==min(RMSE$RMSE)),]
    bestmods <- RMSE %>% dplyr::group_by(group) %>% dplyr::summarise(meangrp = mean(RMSE))
    print(paste("Best model:",best$mod,round(best$RMSE,3),". Mean RSME values:",bestmods$meangrp))
    ## Rsquared
    #tidy values
    Rsq <- evals %>% dplyr::filter(X=="Rsquared") %>% tidyr::gather(mod,Rsquared,2:ncol(evals)) %>% dplyr::select(-X) %>% dplyr::mutate(group=stringr::str_extract(mod, "[aA-zZ]+"))
    #create plot
    ggplot2::ggplot(Rsq, ggplot2::aes(x=group, y=Rsquared, fill=group)) +
      ggplot2::geom_boxplot(show.legend = FALSE)  +
      ggplot2::scale_fill_brewer(palette="Blues") + ggplot2::theme_classic()
    ggplot2::ggsave(paste0(out.folder,"Outputs/Rsq.PNG"),plot = ggplot2::last_plot())
    #find best model
    bestRsq <- Rsq[which(Rsq$Rsquared==max(Rsq$Rsquared)),]
    bestmodsRsq <- Rsq %>% dplyr::group_by(group) %>% dplyr::summarise(meangrp = mean(Rsquared))
    print(paste("Best model:",bestRsq$mod,round(bestRsq$Rsquared,3),". Mean Rsq values per model:",bestmodsRsq$meangrp))

    ## importance stats ##
    #read in importance sheet
    imp <- read.csv(paste0(out.folder, "Outputs/imps_",max_tries,".csv"))
    imp$X <-imp[,2]
    #format df
    # imp.df <- imp %>% dplyr::select(which(!grepl("var",names(imp)))) %>%
    #   tidyr::gather(Model,IncNodePurity,2:((ncol(imp)+1)/2) ) %>%
    #   dplyr::mutate(Model = stringr::str_replace(Model, pattern="_IncNodePurity", "")) %>%
    #   dplyr::mutate(group=stringr::str_extract(Model, "[aA-zZ]+")) %>%
    #   dplyr::rename(Variable=X)

    ## Error: Can't subset columns that don't exist. x Locations 12, 13, 14, 15, 16, etc. don't exist. i There are only 11 columns.
    ## Alternative code commented out
    imp.df <- imp %>%
      dplyr::select(X, contains("IncNodePurity")) %>%
      tidyr::pivot_longer(cols = contains("IncNodePurity"),
                          names_to = "Model",
                          values_to = "IncNodePurity") %>%
      dplyr::mutate(Model = stringr::str_remove(Model, pattern ="_IncNodePurity")) %>%
      dplyr::mutate(group = stringr::str_extract(Model, "[aA-zZ]+")) %>%
      dplyr::rename(Variable = X)

    #generate plot
    ggplot2::ggplot(imp.df, ggplot2::aes(x=Variable, y=IncNodePurity, fill=group)) +
      ggplot2::geom_boxplot(show.legend = F)  +
      ggplot2::scale_fill_brewer(palette="Greens") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x=element_text(angle=90,size=10))
    ggplot2::ggsave(paste0(out.folder, "Outputs/VarImportance.PNG",sep=""),plot =  ggplot2::last_plot())

    # get most important per model
    mostimp <- imp.df %>% dplyr::group_by(group,Variable) %>%
      dplyr::summarise(maxvar = max(IncNodePurity))
    RFvar <- mostimp %>% dplyr::arrange(desc(maxvar))
    print(RFvar)

}



