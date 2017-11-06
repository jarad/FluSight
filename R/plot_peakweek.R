#' Plots Peak Week Forecasts
#'
#' This function allows you to plot Peak Week Predictions
#' @param dat Expects a data csv in the form of a CDC fluview submission see \code{FluSight} package for a minimal submission
#' @param region Specifies the region to be plotted
#' @keywords Peak Week Prediction Plot
#' @export
#' @examples


plot_peakweek <- function(dat, region){
    d <- subset(dat, location == region & target == "Season peak week" & 
                    type == "Bin")
    d$Week <- c(1:33)[as.factor(d$bin_start_incl)]
    d$bin_start_incl <- factor(substr(d$bin_start_incl, 1, nchar(d$bin_start_incl) - 2),
                               levels=paste(c(40:52, 1:20), sep = ""))
    ggplot(data = d, aes(x = bin_start_incl, y = value)) + geom_point() + 
        ylim(0, 1) + 
        labs(title = "Season Peak Week", x = "Week", y = "Prob")
}
