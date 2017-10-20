#' Plots a CDC FluView Submission
#'
#' This function plots a quick grid of plots to view a CDC fluview submission.
#' @param dat Expects a CDC fluview submission (see \code{FluSight} package for a minimal submission)
#' @param ilimx Numeric. Max level of ILI percentage to be plotted
#' @param years Year to retrieve data for
#' @param pdfloc Loction to print out to pdf. If NULL, will show plots in house. 
#' @keywords grid plots
#' @export
#' @examples
#' plot_forecast(dat=full_entry, ilimx=10, years=2016)

plot_forecast <- function(dat, ilimx, years=NA, pdfloc=NULL){
  require(ggplot2)
  require(gridExtra)
  require(cdcfluview)
  
  # ILI Baselines for Regions 1:10, National
  #ILIbaseline <- read.csv("ILIBaselines.csv")

  region <- levels(as.factor(dat$location))

  week <- get_week(Sys.Date())
  week <- formatC(week, width = 2, format="d", flag="0")  
  
  if(is.null(pdfloc)){}else{pdf(paste(pdfloc,"week", week, ".pdf", sep=""), width=18, height=12)}

  for(i in 1:length(region)){
    
    # Plot ILI% predictions for Weeks out
    p1wk <- plot_weekahead(dat, region=region[i], wk=1, ilimax=ilimx, years=years)
    p2wk <- plot_weekahead(dat, region=region[i], wk=2, ilimax=ilimx, years=years)
    p3wk <- plot_weekahead(dat, region=region[i], wk=3, ilimax=ilimx, years=years)
    p4wk <- plot_weekahead(dat, region=region[i], wk=4, ilimax=ilimx, years=years)
    
    # Plot Onset Week Predictions
    onst  <- plot_onset(dat, region[i])
    
    # Plot Peak Percentage Predictions
    pkper <- plot_peakper(dat, region[i])
      
    # Plot Peak Week Predictions
    pkwk  <- plot_peakweek(dat, region[i])

    # Plot Actual % ILI up to current week
    cdcdat <- plot_cdc(region[i], years=years)
    
    # Arranges various plots onto a grid
    
    grid.arrange(p1wk,onst,p2wk,pkper,p3wk,pkwk,p4wk,cdcdat, nrow=4, ncol=2, top=paste(region[i]))
    par(ask=TRUE)
    # readline("press enter to see next plot")
  }
  
  
  # Plots the probabilities of %ILI by region colored by week
  sbdat <- subset(dat, as.numeric(as.character(bin_start_incl)) <= ilimx & 
                    target %in% c("1 wk ahead","2 wk ahead","3 wk ahead","4 wk ahead"))
  
  natplot <- ggplot(data=sbdat, aes(x=as.numeric(as.character(bin_start_incl)), y=value, color=target)) +
    geom_point(size = 1) + facet_grid(location~.) + labs(x="ILI%", y="Prob", title="ILI by Region")
    
    print(natplot)
    dev.off()
}







#' Get the surveillance week
#'
#' @param date A Date object or one that can be coerced to a Date object
#' @return The numeric week number
#' @importFrom MMWRweek MMWRweek
#' @seealso \code{\link{write_entry}}, \code{\link{as.Date}}
#' @keywords internal
get_week <- function(date) {
  warning("Getting a week automatically is an experimental feature. ",
          "Please check the week in the filename.")
  
  if (!is(date, "Date")) date = as.Date(date)
  
  MMWRweek::MMWRweek(date-9)$MMWRweek
}
