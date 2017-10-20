#' Plots CDC Data
#'
#' This function allows you to plot current and past CDC data.
#' @param region Specifies the region to be plotted.
#' @param years Specifies years to retrieve data for (i.e. 2014 for CDC flu season 2014-2015). Default value is the current year and all years values should be > 1997
#' @keywords CDC FluView
#' @export
#' @examples
#' plotcdcdata()


plot_cdc <- function(region,years=NA){
  require(ggplot2)
  require(cdcfluview)
  
  if(region=="US National"){
    if(is.na(years)){
      d <- get_flu_data("national", NA, "ilinet") 
    }else{
      d <- get_flu_data("national", NA, "ilinet", years=years) 
    }
  }else{
    if(is.na(years)){
      d <- get_flu_data("hhs", as.numeric(unlist(strsplit(region, " "))[3]), "ilinet")
    }else{
      d <- get_flu_data("hhs", as.numeric(unlist(strsplit(region, " "))[3]), "ilinet", years=years)
    }
  }
  
  if(is.na(years)){
    baselines <- read.csv(system.file("extdata", "wILI_Baseline.csv", package="FluSight"), header=TRUE)
    ILIbaseline <- subset(baselines, year==tail(baselines$year, 1))
  }else{
    baselines <- read.csv(system.file("extdata", "wILI_Baseline.csv", package="FluSight"), header=TRUE)
    ILIbaseline <- subset(baselines, year==years)
  }

  
  
  d$WEEK <- factor(factor(as.factor(d$WEEK)), levels=c(c(40:52,1:20),"none"))
  
  ggplot(data=d[!is.na(d$WEEK),], aes(x=WEEK, y=`% WEIGHTED ILI`)) + 
    geom_line(aes(group=1)) + geom_point() + ylab("ILI%") +
    geom_hline(yintercept = ILIbaseline$value[ILIbaseline$location==region])
}
