# Data editing for idtracker trajectories
  # Converts NaN's to NA's
  # Removes ProbID1 column
  # Assigns trial numbers
  # Removes strings of NA's greater than a specified "noise_allowance" length
  # Returns finished and labeled data frame
  # Retains frame numbers from the original csv

idtracker_data_fix <- function(df, noise_allowance = 3, searchcolumn = 1){
  # Change NaN's to NA's
  for(i in 1:ncol(df)){
    df[,i][is.nan(df[,i])] <- NA
  }
  
  # Remove ProbId1 column
  df$ProbId1 <- NULL
  
  # Assign frame numbers
  df$framenum <- 1:nrow(df)
  
  # Function to identify runs of NA's
  consecna <- function(x, noise_allowance){
    # function to identify rows that are part of a run of n or more NA's
    # https://stackoverflow.com/questions/16842163/consecutive-nas-in-a-column
    y <- rle(is.na(x))
    y$values <- y$lengths > (noise_allowance - 0.5) & y$values
    return(inverse.rle(y))
  }
  
  # Filter out rows that are part of a run of NA's
  df <- df[!consecna(df[,searchcolumn], noise_allowance),] # look in the specified column for runs of NA's
  
  # Find ends of trials
  ends <- which(diff(df$framenum) > 1)
  
  # Add blank Trial column
  df$trial <- NA
  
  # Assign trial ID's to the first and last trials
  df$trial[1:ends[1]] <- 1 # first trial
  df$trial[(max(ends)+1):nrow(df)] <- length(ends) + 1 # last trial
  
  # Fill in the ID's in between
  if(length(ends) > 1){
    for(i in 2:length(ends)){
      df$trial[(ends[i-1]+1):ends[i]] <- i
    }
  }
  
  return(df)
}
