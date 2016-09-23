require(dplyr)
#require(data.table)

### Need some way to add forecast date to the submission - could add to verify_forecasts


calculate.bin.scores <- function(this.sub, targets, mult.bins=T) {

	names(this.sub) <- tolower(names(this.sub))

  ### add observed values - split seasonal targets and weekly targets to join correctly
  this.sub.season <- filter(this.sub,
                                 target %in% c("Season onset","Season peak week","Season peak percentage"),
                                 type != 'point') %>%
                          inner_join(
                                    select(targets,-forecast.date))
  this.sub.weekly <- filter(this.sub,
                                 (!target %in% c("Season onset","Season peak week","Season peak percentage")),
                                 type != 'point') %>%
                          inner_join(
                                     targets)
  this.sub <- rbind(this.sub.season, this.sub.weekly)

  ### create score table with one row per forecast
  forecast.scores <- select(this.sub, -value, -type, -unit,
      -bin_start_incl, -bin_end_notincl) %>%
    distinct()
  ### calculate three bin log score
  for (i in 1:dim(forecast.scores)[1]) {
    forecast.scores$score[i] <-
        filter(this.sub,
            #team == forecast.scores$team[i] &
            location == forecast.scores$location[i] &
            target == forecast.scores$target[i] &
            forecast.date == forecast.scores$forecast.date[i]) %>%
        bin.score(
                  mult.bins)
    }
  return(forecast.scores)
}

bin.score <- function(these.probs, mult.bins=T) {
  #if (is.na(these.probs$observation[1])) return(NA)

  #Identify bin with correct prediction
  these.probs <- arrange(these.probs, bin_start_incl)
  correct.bin <- which(these.probs$bin_start_incl == these.probs$observation)

  if(mult.bins=F){
    these.bins <- correct.bin
  } else {
    #If week target, include +- 1 bin, otherwise include +- 5 bins
    if(these.probs$target %in% c("Season onset","Season peak week")){
      these.bins <- correct.bin + -1:1
      these.bins <- these.bins[1 <= these.bins & these.bins <= dim(these.probs)[1]]
    }else{
      these.bins <- correct.bin + -5:5
      these.bins <- these.bins[1 <= these.bins & these.bins <= dim(these.probs)[1]]
    }

    #Figure out additional correct bins if multiple peaks
    if(!is.na(these.probs$observation2[1])){
      correct.bin2 <- which(these.probs$bin_start_incl == these.probs$observation2)
      #If week target, include +- 1 bin, otherwise include +- 5 bins
      these.bins2 <- correct.bin2 + -1:1
      these.bins2 <- these.bins2[1 <= these.bins2 & these.bins2 <= dim(these.probs)[1]]
      #Join with correct bins for first peak week
      these.bins <- union(these.bins,these.bins2)
    }
  }
  return(log(sum(these.probs$value[these.bins])))
}
