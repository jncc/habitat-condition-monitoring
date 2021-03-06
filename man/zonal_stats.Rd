% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FunctionZonalStats.R
\name{zonal_stats}
\alias{zonal_stats}
\title{Zonal stats}
\usage{
zonal_stats(polys, polyfield = "id", s2path = NA, s1path = NA,
  outfolder, sitename = "Testsite")
}
\arguments{
\item{polys}{shapefile path of polygons}

\item{polyfield}{polygon id in shapefile}

\item{s2path}{path to folder containing the s2 imagery}

\item{s1path}{path to folder containing the s1 imagery}

\item{sitename}{name of site,the output zonal stats file will be saved with this name}

\item{outFolder}{folder to save zonal statistic files in}
}
\description{
Zonal stats
}
