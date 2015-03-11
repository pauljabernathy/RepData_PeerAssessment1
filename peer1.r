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
stepsHist2 <- function(activity) {
  totals <- rep(0,288);
  steps <- activity$steps;
  for(i in 1:287) {
    totals[i] <- sum(steps[indices[which(indices %% 288 == i)]], na.rm=TRUE);
  }
  totals[288] <- sum(steps[indices[which(indices %% 288 == 0)]], na.rm=TRUE);
  totals;
}

stepsHistByDate <- function(activity) {
  #do something 
}