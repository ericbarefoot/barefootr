% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/azimuth_conversions.R
\name{azStringConvert}
\alias{azStringConvert}
\title{Azimuth conversion for 'quadrant' based strike/trend measurements}
\usage{
azStringConvert(string)
}
\arguments{
\item{string}{A string measurement with format [N/S][##][E/W]. Can be either X00X or X0X. Make sure you're using the right hand rule to determine how quadrant measurements are made, otherwise you're screwed.}
}
\value{
A numeric azimuth trend.
}
\description{
Takes measurements of strike or trend like N45E and converts it to an azimuth measurement with N = 0 and E = 90.
}
