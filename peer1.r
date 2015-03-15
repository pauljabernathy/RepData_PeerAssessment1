stepsHist <- function(activity) {
  totals <- rep(0,288);
  intervals <- activity$interval;
  steps <- activity$steps;
  print(head(intervals));
  print(length(intervals));
  print(length(steps));
  for(i in 1:length(steps)) {
    if(!is.na(activity$steps[i])) {
      index <- i %% 288;# != 0 ? i %% 288 : 288;
      if(index == 0) {
        index <- 288;
      }
      totals[index] <- totals[index] + steps[i];
    }
  }
  totals;
}

#does the same as above but with compact code, using R's OTB sum()
stepsTotals <- function(activity) {
  totals <- rep(0,288);
  steps <- activity$steps;
  for(i in 1:287) {
    totals[i] <- sum(steps[indices[which(indices %% 288 == i)]], na.rm=TRUE);
  }
  totals[288] <- sum(steps[indices[which(indices %% 288 == 0)]], na.rm=TRUE);
  totals;
}

stepsHistByDate <- function(activity) {
  dayTotals <- vector();#rep(0, length(date));
  date <- activity$date
  for(currentDate in levels(date)) {
    dayTotals <- c(dayTotals, sum(steps[date == currentDate], na.rm=TRUE));
  }
  dayTotals;
}

fillInNAs <- function(activity) {
  totals <- stepsTotals(activity);
  adjustedSteps <- activity$steps
  numDays <- length(as.factor(activity$days));
  for(i in 1:length(adjustedSteps)) {
    if(is.na(steps[i])) {
      if(i %% 288 == 0) {
        adjustedSteps[i] <- totals[288] / 61;
      } else {
        adjustedSteps[i] <- totals[ i %% 288] / 61;
      }
    }
  }
  adjustedSteps;
}

getAdjustedTotals <- function(activity) {
  adjustedSteps <- fillInNAs(activity);
  adjustedActivity <- data.frame(adjustedSteps, date, interval);
  names(adjustedActivity) <- names(activity);
  head(adjustedActivity);
  adjustedTotals <- stepsTotals(adjustedActivity);
  adjustedTotals;
}

isWeekend <- function(date) {
  day <- weekdays(date);
  day == "Saturday" | day == "Sunday"
}