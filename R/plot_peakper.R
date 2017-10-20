#' Plots Peak Percentage Forecasts
#'
#' This function allows you to plot Peak Percentage Predictions
#' @param dat Expects a data csv in the form of a CDC fluview submission see \code{FluSight} package for a minimal submission
#' @param region Specifies the region to be plotted
#' @keywords Peak Percentage Prediction Plot
#' @export
#' @examples
#' plotPeakPer()

plot_peakper <- function(dat, region){
  require(ggplot2)
  
  d <- subset(dat, location==region & target=="Season peak percentage" & type=="Bin")
  
  ggplot(data=d, aes(x=as.numeric(as.character(bin_start_incl)), y=value)) + 
    geom_point() + labs(title = "Season Peak Percentage", x="Percent", y="Prob")
}
