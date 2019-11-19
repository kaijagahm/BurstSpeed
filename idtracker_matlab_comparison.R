# Comparison of tracks from IDtracker and Matlab
source("libraries.R")
source("idtracker_data_fix.R")
source("loess_wrapper_extrapolate.R")

# Read in the idtracker data
idtracker_files <- list.files(path = "test_tracking_idtracker/", pattern = "txt", full.names = T)
idtracker <- lapply(idtracker_files, read.table, header = T)
idtracker <- lapply(idtracker, idtracker_data_fix, noise_allowance = 3, searchcolumn = 1)

# Read in the matlab data
matlab_files <- list.files(path = "test_tracking_matlab/", pattern = "csv", full.names = T)
matlab <- lapply(matlab_files, read.csv, header = F)

# Regex to find tadpole names, and apply them
pattern = "(?<=\\_)[[:alnum:]]+(?=\\.)"
idtracker_names <- str_extract(idtracker_files, pattern = pattern)
names(idtracker) <- idtracker_names
matlab_names <- str_extract(matlab_files, pattern = pattern)
names(matlab) <- matlab_names

# Remove the frame numbers from the idtracker files
removeframe <- function(df){
  df$framenum <- NULL
  return(df)
}

idtracker <- lapply(idtracker, removeframe)

# Standardize column names
namesvec <- c("x", "y", "trial")
makenames <- function(df){
  colnames(df) <- namesvec
  return(df)
}

idtracker <- lapply(idtracker, makenames)
matlab <- lapply(matlab, makenames)

# Understand trial numbers as factors
trial2factor <- function(df){
  df$trial <- factor(df$trial)
  return(df)
}

idtracker <- lapply(idtracker, trial2factor)
matlab <- lapply(matlab, trial2factor)

frames_times <- function(df){
  df$frame <- NA
  for(i in 1:length(unique(df$trial))){
    df$frame[df$trial==i] <- 1:nrow(df[df$trial==i,])
  }
  df$time <- df$frame/60
  return(df)
}
idtracker <- lapply(idtracker, frames_times)
matlab <- lapply(matlab, frames_times)

# Test plotting
ggplot(aes(x = x, y = y, col = trial), data = matlab[[2]]) +
  geom_point()+
  geom_point(aes(x = x, y = y), data = idtracker[[2]], color = "black", alpha = 0.3)
# This looks great!

# Test smoothing
test <-  matlab[[2]] %>% filter(trial == 2)
test$num <- 1:nrow(test)
test <- na.omit(test)

test$loessx <- loess_wrapper_extrapolate(x = test$num, y = test$x, span.vals = seq(0.15, 1, by = 0.05))$fitted
test$loessy <- loess_wrapper_extrapolate(x = test$num, y = test$y, span.vals = seq(0.15, 1, by = 0.05))$fitted

ggplot(test)+
  geom_point(aes(x, y), color = "black")+
  geom_line(aes(loessx, loessy), color = "blue")

ggplot(test, aes(x, y))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~s(x))

a <- loess_bootstrap_partial(x = test$num, y = test$x)
ggplot(a)+
  geom_point(aes(x = span, y = error))

# Testing smoothing with SG in the trajr package
library(trajr)
coords <- test[,c(1:2, 4)]
trj <- TrajFromCoords(coords, spatialUnits = "pixels")
plot(trj, lwd = 1, lty = 1)

derivs <- TrajDerivatives(trj)
s <- data.frame(
  speedTime = derivs$speedTimes,
  speed = derivs$speed
)

a <- data.frame(
  accelTime = derivs$accelerationTimes,
  accel = derivs$acceleration
)

# plot speed: this is actually what we want to be smoothing
s$loess_25 <- loess(speed~speedTime, data = s, span = 0.25)$fitted
s$loess_50 <- loess(speed~speedTime, data = s, span = 0.5)$fitted
s$loess_75 <- loess(speed~speedTime, data = s, span = 0.75)$fitted

p <- ggplot(s)+
  geom_point(aes(x = speedTime, y = speed), color = "black")+
  geom_line(aes(x = speedTime, y = loess_25), color = "green", alpha = 0.75, lwd = 1)+
  geom_line(aes(x = speedTime, y = loess_50), color = "blue", alpha = 0.75, lwd = 1)+
  geom_line(aes(x = speedTime, y = loess_75), color = "red", alpha = 0.75, lwd = 1)
p


speedloess <- function(df){
  coords <- df[,c(1:2, 5)]
  trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeCol = "time", fps = 60)
  derivs <- TrajDerivatives(trj)
  s <- data.frame(
    speedTime = derivs$speedTimes,
    speed = derivs$speed
  )
  
  s$loess_25 <- loess(speed~speedTime, data = s, span = 0.25)$fitted
  s$loess_50 <- loess(speed~speedTime, data = s, span = 0.5)$fitted
  s$loess_75 <- loess(speed~speedTime, data = s, span = 0.75)$fitted
  
  p <- ggplot(s)+
    geom_point(aes(x = speedTime, y = speed), color = "black")+
    geom_line(aes(x = speedTime, y = loess_25), color = "green", alpha = 1, lwd = 1)+
    geom_line(aes(x = speedTime, y = loess_50), color = "blue", alpha = 0.75, lwd = 1)+
    geom_line(aes(x = speedTime, y = loess_75), color = "red", alpha = 0.5, lwd = 1)+
    xlab("Time (seconds)")+
    ylab("Speed (pixels/second)")
  
  return(p)
}

speedmovavg <- function(df){
  coords <- df[,c(1:2, 5)]
  trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeCol = "time", fps = 60)
  derivs <- TrajDerivatives(trj)
  s <- data.frame(
    speedTime = derivs$speedTimes,
    speed = derivs$speed
  )
  
  n_test <- round(nrow(s)/10)
  if(n_test < 2){
    n_test <- 2
  }
  
  
  s$mov_t <- movavg(s$speed, n = n, type = "t") #i like this one best
  s$mov_s <- movavg(s$speed, n = n, type = "s") # second choice
  s$mov_m <- movavg(s$speed, n = n, type = "m") #nonono
  s$mov_e <- movavg(s$speed, n = n, type = "e") # also no
  s$mov_w <- movavg(s$speed, n = n, type = "w") #  nope
  
  p <- ggplot(s)+
    geom_point(aes(x = speedTime, y = speed), color = "black")+
    geom_line(aes(x = speedTime, y = mov_s), color = "red", alpha = 0.50, lwd = 1)+
    geom_line(aes(x = speedTime, y = mov_m), color = "green", alpha = 0.25, lwd = 1)+
    geom_line(aes(x = speedTime, y = mov_e), color = "orange", alpha = 0.25, lwd = 1)+
    geom_line(aes(x = speedTime, y = mov_w), color = "purple", alpha = 0.25, lwd = 1)+
    geom_line(aes(x = speedTime, y = mov_t), color = "blue", alpha = 0.75, lwd = 1)+
    xlab("Time (seconds)")+
    ylab("Speed (pixels/second)")
  p
  
  return(p)
}


matlab2 <- matlab
for(i in 1:length(matlab)){
  matlab2[[i]] <- split(matlab[[i]], matlab[[i]]$trial)
}

ploteachtrial_loess <- function(list_of_trials){
  list_of_plots <- list_of_trials # define a new list to hold the plots
  for(i in 1:length(list_of_trials)){
    list_of_plots[[i]] <- speedloess(list_of_trials[[i]])
  }
  return(list_of_plots)
}

matlabplots_loess <- vector("list", length = length(matlab2))
for(i in 1:length(matlabplots_loess)){
  matlabplots_loess[[i]] <- ploteachtrial_loess(matlab2[[i]])
}

save(matlabplots_loess, file = "matlabplots_loess.Rda")

ploteachtrial_movavg <- function(list_of_trials){
  list_of_plots <- list_of_trials # define a new list to hold the plots
  for(i in 1:length(list_of_trials)){
    list_of_plots[[i]] <- speedmovavg(list_of_trials[[i]])
  }
  return(list_of_plots)
}

matlabplots_movavg <- vector("list", length = length(matlab2))
for(i in 1:length(matlabplots_movavg)){
  matlabplots_movavg[[i]] <- ploteachtrial_movavg(matlab2[[i]])
}

save(matlabplots_movavg, file = "matlabplots_movavg.Rda")
# this throws an error, but it doesn't matter, because we want to cut it off at the tap anyway
