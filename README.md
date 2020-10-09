
[<img src="www/logos.png">](https://jncc.gov.uk/our-work/copernicus-project/)

<p> 

# 'habitat-condition-monitoring' Package

This R package was developed by JNCC under the Copernicus User Uptake Work Package 6 projects focussing on Bare Peat Mapping and Habitat Change Detection. It contains various functions involved in the preparation, statistical analysis and modelling with Sentinel-1 and Sentinel-2 data, as well as some designed for analysis of VHR data obtained from Defra's Aerial Photography GB (APGB) service.
<p> 

## Functions
### Sentinel-1 Processing

* <b>runS1Indices</b> -  Function to calculate RVI and RVIv indices from Sentinel 1 imagery
* <b>s1thumbs</b> - Script for creating S1 false colour thumbnails


### Sentinel-2 Processing

* <b>runIndices</b> -  Calculate indices for Sentinel-2 imagery
* <b>s2_processing</b> - Preparing Sentinel-2 imagery by cloud and shadow masking and calculating indices
* <b>sat_varstack</b> - Function for compiling variable stack from sentinel 2 imagery and indices

### APGB Functions

* <b>APGBdate</b> - Function to iterate through imagery and rename with a lookup date.
* <b>comboAPGB</b> - Function for combining the apgb RGB and CIR images


### Change Detection

* <b>zonal_stats</b> - Zonal statistics function
* <b>change_stats</b> - calculates monthly, seasonal summaries per polygon, then calculates summary statistics per habitat type.
* <b>create_square_bounding_box_polygon</b> - This function creates a square bounding box polygon for a site, buffered by 10m so that it includes a margin of single raster pixels
* <b>reduce_image_size</b> - This function reduces the size of an image, saving the original image in an archive folder
* <b>legend_thumbnail</b> - This function creates a legend image for the thumbnails
* <b> fileEdit </b> - Function to edit the zonal stats txt files to remove additional rownames column and add a monthdate column
* <b> fileEdit2 </b> - Function to write the habitat names into the zonal stats txt files
* <b> flag_change </b> - Function to flag change using the outputs of the 'change_stats' function

### Bare Peat Mapping

* <b>barethresh</b> - Thresholding bare peat based on indices
* <b>ClassProfile</b> - Function script for analysing the differences between different classes in the imagery determined by points of polygons
* <b>mask_bare</b> - Will mask  your classified bare peat image given specified areas to include and exclude
* <b>MaskPeatSoils</b> - Function to mask tiles to the peat soils and delete any tiles which do not overlap with peat soils.
* <b>pcov_classify</b> - Preparing the APGB classified images as percentage covers of bare peat.
* <b>train_extract</b> - Function to extract training data from a variable stack
* <b>RFReg</b> - function to run through random forest regression modelling with a given set of training data and timeseries of environmental variables.

<p>

## Vignettes

* <b>Bare_Peat_Detection.Rmd</b> - Walkthrough of the fine-scale mapping processing detecting bare peat in APGB imagery.
* <b>Bare_Peat_Regression.Rmd </b>- Walkthrough of the time series detection of bare peat using Random Forest Regressions with Sentinel-2 imagery.
* <b>Habitat_Change_detection.Rmd</b> - Walkthough of the processes of detecting habitat changes in Sentinel-1 and 2 time series data using a defined spatial framework.
