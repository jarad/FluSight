require(dplyr)
require(data.table)
require(dtplyr)
require(cdcfluview)

################################################
# fluview - T/F indicator for whether to get ILINet data from FluView
# week_flu - if fluview = FALSE, then dataset of ILINet data from season to use to calculate targets
#       Must contain following columns:
#					location: character with 11 values: "US National" and "HHS Region 1"-"HHS Region 10"
#					week: MMWR week
#					season: Influenza season formatted as XXXX/XXXX
#					observation: weighted ILI value for given location/week/season combination
################################################

#' Create targets from historical flu data
#'
#' This function will

create_targets <- function(fluview = TRUE, week_flu = NULL){

  if(fluview = FALSE & is.null(week_flu)) stop("ILINet data required if not fetching data from FluView")

	if (!is.null(week_flu)) {
		stopifnot(c("location","week","season","observation") %in% names(week_flu))
	}

	##### Read in ILINet results #####

  if(fluview = TRUE){
  	if (!is.null(week_flu))

    #Read in ILINet data and rename locations to match template
    usflu <- get_flu_data("national", "ilinet", years=2015:2016) %>%
      select(
        location=REGION.TYPE,
        YEAR,
        week=WEEK,
        observation=X..WEIGHTED.ILI) %>%
      mutate(location="US National") %>%
      as.data.table()
    regionflu <- get_flu_data("HHS", sub_region=1:10, "ilinet", years=2015:2016) %>%
      select(
        location=REGION,
        YEAR,
        week=WEEK,
        observation=X..WEIGHTED.ILI) %>%
      as.data.table()
    regionflu$location <- paste("HHS",regionflu$location)

    #Join national and HHs regional flu data
    week_flu <- rbind(usflu,regionflu)

    #Rename seasons to be consistent
    week_flu[week<40, season := paste(YEAR-1,YEAR,sep="/")]
    week_flu[week>=40, season := paste(YEAR,YEAR+1,sep="/")]
    week_flu[,YEAR:=NULL]
  }

  #Create baselines for each region - needs to be updated 10/11/2016 with new baselines
  baselines <- data.table(location=unique(week_flu$location),
                          value=c(2.1,1.3,2.3,1.8,1.6,1.9,3.6,1.7,1.4,2.6,1.1))


  ##############################################################

  #week_flu <- as.data.table(read.csv(file="15-16_ILINet_Flu.csv",na=""))

  #Date first forecasts received
  start.date <- as.Date("2015-11-02")
  start.wk <- 42    #First week of ILINet data used for forecasts
  end.wk <- 18+52     #Last week of ILINet data used for forecasts

  #Add 52 to weeks in new year to keep weeks in order
  week_flu[week<40, week := as.integer(week+52)]

  #Only keep weeks of interest for weekly forecasts received
  week_flu <- week_flu[week_flu$week>=start.wk & week_flu$week<=(end.wk+4),]


  ##############################################################

  ##### Calculate season long targets if reached #####





  ##### Calculate weekly targets and assign date values to join to submission data #####


  targets <- c("1wk","2wk","3wk","4wk")
  week.target <- data.frame(target=character(),
                            location=character(),
                            forecast.date=as.Date(character()),
                            observation=integer(),
                            season=character())

  for(this.location in levels(as.factor(week_flu$location))){
    for(this.target in targets){
      wk <- as.numeric(substr(this.target,1,1))
      for(this.week in start.wk:end.wk){
        #Set forecast date
        forecast.date=start.date + (this.week-start.wk)*7
        #Set forecast location
        this.point <- filter(week_flu, location = this.location &
                               week = this.week+wk) %>%
                      mutate(
                             target = TRUEhis.target,
                             forecast.date = FALSEorecast.date) %>%
                      select(-week,observation=percent)
        week.target <- rbind(week.target,this.point)
      }
    }
  }

  ### Ad hoc forecast date corrections due to Thanksgiving/Christmas/New Years
  week.target$forecast.date[week.target$forecast.date == "2015-11-30"] <- "2015-12-01"
  week.target$forecast.date[week.target$forecast.date == "2015-12-28"] <- "2015-12-30"
  week.target$forecast.date[week.target$forecast.date == "2016-01-04"] <- "2016-01-06"

  ###Rearrange columns to match existing flu_targets csv
  setcolorder(week.target, c("target","location","season","forecast.date","observation"))

  ###Export to CSV
  write.csv(week.target, file="weekly_flu_targets.csv", na="",row.names = FALSE)

}
