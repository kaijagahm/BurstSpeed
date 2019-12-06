# Process coordinates functions
# Define some functions
# Function to capitalize words
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Function to tally frames by trial, and name the data frames
name_and_number <- function(df){
  colnames_vec <- c("x", "y", "angle", "px", "trial") # create column names vector
  names(df) <- colnames_vec
  df <- tally_by_group(df, group_col = "trial")
  return(df)
}

# Function to change NaN's to NA's
nantona <- function(df){
  for(i in 1:ncol(df)){
    df[,i][is.nan(df[,i])] <- NA
  }
  return(df)
}

# Function to make and scale trajectories from coordinates, given x and y scaling parameters.
# Returns a list of trajectories, one for each trial
trajMakeScale <- function(df, xscale, yscale){
  ntrials <- length(unique(df$trial))
  trajectories <- vector("list", ntrials)
  for(i in 1:ntrials){
    coords <- df %>% filter(trial == i) %>% select(x:px) %>% na.omit()
    trj <- TrajFromCoords(coords, spatialUnits = "pixels", fps = 30, timeUnits = 's')
    trjscaled <- TrajScale(trj, xscale, "m", yscale)
    trajectories[[i]] <- trjscaled
  }
  return(trajectories)
}

# Function to get speed data frames
getSpeedDFs <- function(onetadpole_trials_list){
  ntrials <- length(onetadpole_trials_list)
  speedDFs <- vector("list", ntrials)
  for(i in 1:ntrials){
    derivs <- TrajDerivatives(onetadpole_trials_list[[i]])
    s <- data.frame(
      time = c(0, derivs$speedTimes),
      speed = c(NA, derivs$speed),
      angle = onetadpole_trials_list[[i]]$angle,
      px = onetadpole_trials_list[[i]]$px
    )
    speedDFs[[i]] <- s
  }
  return(speedDFs)
}

# Function to name the frames df's
renameFrames <- function(df){
  names(df) <- c("frame", "trial")
  return(df)
}

# Function to transform angles so they all start at 0
angleTransform <- function(angles){
  newangles <- rep(NA, length(angles))
  newangles[1] <- 0
  factor = 0 - angles[1]
  newangles[2:length(newangles)] <- angles[2:length(newangles)] + factor
  return(newangles)
}

# Function to find the beginning from speed
findBeginningSpeed <- function(df, speedvar){
  numpoints <- nrow(df)
  minspeed <- min(df[,speedvar], na.rm = T)
  maxspeed <- max(df[,speedvar], na.rm = T)
  firstfast <- min(which(df[,speedvar] > maxspeed/4), na.rm = T)
  upperthresh <- minspeed + maxspeed/25 # somewhat arbitrary division by 25
  slows <- which(df[,speedvar] < upperthresh) # upper threshold for what we're considering "slow"
  if(firstfast <= min(slows, na.rm = T)){ # if the first fast point happens almost immediately, begin at frame 1
    beginning_ind <- 1}
  else{beginning_ind <- max(slows[slows < firstfast], na.rm = T)}
  return(beginning_ind)
}

# Function to find the beginning from pixel differences
findBeginningPx <- function(df, pxvar){
  numpoints <- nrow(df)
  minpx <- min(df[,pxvar], na.rm = T)
  maxpx <- max(df[,pxvar], na.rm = T)
  firstpxhigh <- min(which(df[,pxvar] > maxpx/4), na.rm = T)
  upperthresh <- minpx + maxpx/25 # somewhat arbitrary division by 25
  lowpxs <- which(df[,pxvar] < upperthresh) # upper threshold for what we're considering "slow"
  if(firstpxhigh <= min(lowpxs, na.rm = T)){ # if the first fast point happens almost immediately, begin at frame 1
    beginning_ind <- 1}
  else{beginning_ind <- max(lowpxs[lowpxs < firstpxhigh], na.rm = T)}
  return(beginning_ind)
}

# Function to find the beginning from angles
findBeginningAngle <- function(df, anglevar){
  diff <- abs(df[,anglevar])
  firstup <- min(which(diff > 5))
  return(firstup-1)
}

