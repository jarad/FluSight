require(dplyr)
require(data.table)
require(dtplyr)
require(cdcfluview)

#' Create targets from historical flu data
#'
#' This function will
#'
#'
#'

# Import 15/16 ILINet data for function testing
week_flu <- as.data.table(read.csv(file = "data-raw/15-16_ILINet_Flu.csv"))

create_targets <- function(fluview = TRUE, week_flu = NULL) {
	# Creates file of seasonal and week ahead targets for CDC FluSight influenza
	# forecasting challenge from seasonal ILINet data.
	#
	# Args:
	# 	fluview:  if TRUE, retrieves ILINet data from FluView; if not, uses
	#						  provided ILINet data
	# 	week_flu: data frame of weekly ILINet values from this season to use for
	#							calculating targets. Must be provided if fluview == FALSE.
	#							Must contain the following colums:
	#								- location: 		character with 11 values; "US National" and
	#																"HHS Region 1" to "HHS Region 10"
	#								- week:					MMWR week
	#								- season:				Influenza season formatted as XXXX/XXXX
	# 							- observation:	weighted ILI value for given
	#																location/week/season combination
	#
	# Returns:
	# 	A data frame and CSV of seasonal and week ahead flu targets.

	# Return an error if fluview == FALSE and no data frame provided
  if (fluview = FALSE & is.null(week_flu)) {
  	stop("ILINet data required if not fetching data from FluView")
  }

	# Return an error if provided data frame not in proper format
	if (!is.null(week_flu)) {
		stopifnot(c("location","week","season","observation") %in% names(week_flu))
	}

	# Read in ILINet results -----------------------------------------------------

  if (fluview = TRUE) {
  	if (!is.null(week_flu))

    # Read in ILINet data and rename locations to match template
    usflu <- get_flu_data("national", "ilinet", years=2015:2016) %>%
      select(
        location = REGION.TYPE,
        YEAR,
        week = WEEK,
        observation = X..WEIGHTED.ILI) %>%
      mutate(location = "US National") %>%
      as.data.table()

    regionflu <- get_flu_data("HHS", sub_region=1:10, "ilinet", years=2015:2016) %>%
      select(
        location = REGION,
        YEAR,
        week = WEEK,
        observation = X..WEIGHTED.ILI) %>%
      as.data.table()

    # Append HHS to beginning of location values for regional data
    regionflu$location <- paste("HHS", regionflu$location)

    # Join national and HHs regional flu data
    week_flu <- rbind(usflu, regionflu)

    # Rename seasons to be consistent
    week_flu[week < 40, season := paste(YEAR-1,YEAR,sep="/")]
    week_flu[week >= 40, season := paste(YEAR,YEAR+1,sep="/")]
    week_flu[,YEAR := NULL]

    # Round weighted ILINet values to 0.1%
    week_flu$observation <- round(week_flu$observation, 1)
  }

  # Create baselines for each region
	# Needs to be updated 10/11/2016 with new baselines
  baselines <- data.table(location = unique(week_flu$location),
                          value = c(2.1,1.3,2.3,1.8,1.6,1.9,3.6,1.7,1.4,2.6,1.1))

  # Date first forecasts received
  start.date <- as.Date("2015-11-02")
  start.wk <- 42    #First week of ILINet data used for forecasts
  end.wk <- 18+52     #Last week of ILINet data used for forecasts

  # Add 52 to weeks in new year to keep weeks in order
  week_flu[week<40, week := as.integer(week+52)]

  # Only keep weeks of interest for weekly forecasts received
  week_flu <- week_flu[week_flu$week >= start.wk & week_flu$week <= (end.wk+4), ]

  # Create data shell to add targets to
  targets <- data.frame(target = character(),
  											location = character(),
  											season = character(),
  											forecast.date = as.Date(character()),
  											observation = numeric()) %>%
  						as.data.table()

  # Calculate season long targets if reached ----------------------------------

  for (this.region in levels(as.factor(week_flu$location))) {

  	# Subset data
  	these.data <- week_flu[location == this.region]

  	# Season onset -------------------------------------------------------------

  	# Check to see if 3 weeks above baseline have passed
  	j <- 0  # Counter for weeks above peak
  	for (i in
  			 head(these.data$week, n = 1):tail(these.data$week, n = 1)) {
  		if (these.data$observation[these.data$week == i] >=
  				baselines$value[baselines$location == this.region]) {
  			j <- j + 1
  		} else {
  			j <- 0
  		}

  		if (j == 3) {
  			onset <- i - 2
  			break
  		}

  		if (i == tail(these.data$week, n = 1)) {
  			onset <- NA
  		}
  	}

  	# If onset week > 52, reset to MMWR week
  	if (onset > 52) {
  		onset <- onset - 52
  	}

  	# Peak week and peak percentage --------------------------------------------

  	pkwk  <- these.data$week[these.data$observation ==
  													 	max(these.data$observation)]
  	pkper <- round(max(these.data$observation), 1)

  	# If peak week > 52, reset to MMWR week
	  for (i in 1:length(pkwk)) {
  		if (pkwk[i] > 52) {
	  		pkwk[i] <- pkwk[i] - 52
  		}
	  }

		# Set to NA if less than 4 weeks following peak
  	if (tail(these.data$week, n = 1) - max(pkwk) < 4) {
  		pkwk  <- NA
  		pkper <- NA
  	}

  	# Save onset, peak week, and peak percentage -------------------------------

  	if (length(pkwk) == 1) {
  		this.pred <- data.frame(target = c("Season onset", "Season peak week",
  																			 "Season peak percentage"),
  														location = this.region,
  														season = "2015/2016",
  														observation = c(onset, pkwk, pkper))
  	} else { # Multiple peak weeks
  		this.pred <- data.frame(target = c("Season onset", "Season peak week",
  																			 "Season peak percentage"),
  														location = this.region,
  														season = "2015/2016",
  														observation = c(onset, pkwk[1], pkper))
  		for (i in 2:length(pkwk)) {
  			extra.obs <- data.frame(V1 = c(NA, pkwk[i], NA))
  			names(extra.obs) <- paste0("observation", i)
  			this.pred <- cbind(this.pred, extra.obs)
  		}
  	}

  	#Append targets to overall target file
  	targets <- rbind(targets, this.pred, fill=TRUE)
  }


  ##### Calculate weekly targets  #####

?rbind
  targets <- c("1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  week.target <- data.frame(target = character(),
                            location = acharacter(),
                            forecast.date = as.Date(character()),
                            observation = integer(),
                            season = character())

  for (this.location in levels(as.factor(week_flu$location))) {
    for (this.target in targets) {
      wk <- as.numeric(substr(this.target, 1, 1))
      for (this.week in start.wk:end.wk) {
        #Set forecast date
        forecast.date <- start.date + (this.week - start.wk) * 7
        #Set forecast location
        this.point <- filter(week_flu, location = this.location &
                               week = this.week+wk) %>%
                      mutate(
                             target = this.target,
                             forecast.date = FALSEorecast.date) %>%
                      select(-week, observation = percent)
        week.target <- rbind(week.target, this.point)
      }
    }
  }

  ### Ad hoc forecast date corrections due to Thanksgiving/Christmas/New Years
  week.target[forecast.date == "2015-11-30", forecast.date := "2015-12-01"]
  week.target[forecast.date == "2015-12-28", forecast.date := "2015-12-30"]
  week.target[forecast.date == "2016-01-04", forecast.date := "2016-01-06"]

  ###Rearrange columns to match existing flu_targets csv
  setcolorder(week.target, c("target", "location", "season", "forecast.date",
  													 "observation"))

  ###Export to CSV
  write.csv(week.target, file="weekly_flu_targets.csv", na="",row.names = FALSE)

}
