% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s2_processing.R
\name{s2_processing}
\alias{s2_processing}
\title{Title}
\usage{
s2_processing(s2path, out_folder, nirband = 8, rband = 3, bband = 1,
  gband = 2, swirband = NA, indices, cropAOI = NA)
}
\arguments{
\item{s2path}{folder path to the multispectral Sentinel-2 imagery}

\item{out_folder}{folder path to save output files}

\item{nirband}{near infra-red band}

\item{rband}{numeric, red band}

\item{bband}{numeric, blue band}

\item{gband}{numeric, green band}

\item{swirband}{numeric, optional - Short wave infrared band}

\item{indices}{a vector of indices to retrieve e.g. c("EVI", "GLI","GNDVI","RDVI","SAVI","SBL"). The function will accept EVI,GLI,GNDVI,RDVI,SAVI,SBL,RB,RG,Brightness,NBR}

\item{cropAOI}{optional, a shapefile containing the area of interest to crop the imagery to}
}
\description{
Title
}
