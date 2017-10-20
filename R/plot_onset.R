#' Plots Onset Forecasts
#'
#' This function allows you to plot Onset Predictions
#' @param dat Expects a data csv in the form of a CDC fluview submission see \code{FluSight} package for a minimal submission
#' @param region Specifies the region to be plotted
#' @keywords Onset Prediction Plot
#' @export
#' @examples
#' plotOnset()

plot_onset <- function(dat, region){
  require(ggplot2)
  
  d <- subset(dat, location==region & target=="Season onset" & type=="Bin")
  d$Week <- c(1:33)[as.factor(d$bin_start_incl)]
  
  d$bin_start_incl[-length(d$bin_start_incl)] <- substr(d$bin_start_incl[-length(d$bin_start_incl)], 1, nchar(d$bin_start_incl[-length(d$bin_start_incl)])-2)
  d$bin_start_incl <- factor(d$bin_start_incl, levels=c(paste(c(40:52,1:20), sep=""),"none"))
  
  ggplot(data=d, aes(x=bin_start_incl, y=value)) + 
    geom_point() + labs(title = "Season Onset", x="Week", y="Prob")
}
