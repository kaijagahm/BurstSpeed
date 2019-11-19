# Batch process coordinates data from matlab output files
source("libraries.R")
source("tally_by_group.R")
source("process_coordinates_funs.R")
###############################################

###########################################################################
########################### COORDINATE DATA PREPARATION ###################
###########################################################################

# Read in the files
files <- list.files(path = "coordinates", full.names = T) # list files
files <- files[!grepl('error', files, ignore.case = T)] # ignore error files
files <- files[!grepl('test', files, ignore.case = T)] #ignore the "test" file
coords <- lapply(files, read.table, sep = ",") # read in files

# Find tadpole names
tadids <- str_extract(files, "(?<=coordinates_).*(?=\\.txt)")
tadids <- gsub("_edit", "", tadids)
names(coords) <- tadids # name by tadpole id

# Name and number coordinates; change NaN's to NA's.
coords <- lapply(coords, name_and_number)
coords <- lapply(coords, nantona)

# Sort in alphabetical order, for consistency
coords <- coords[sort(names(coords))]

# Make a data frame of video info
vidinfo <- data.frame(
  fullname = tadids,
  pond = str_extract(tadids, "^[[:alnum:]]{2,3}"),
  clutch = str_extract(tadids, "(?<=^[[:alnum:]]{2,3}_)[0-9]{2}(?=_)"),
  indiv = NA,
  treatment = str_extract(tadids, "Low|High|W"),
  date = str_extract(tadids, "2019[0-9]{4}"),
  year = 2019,
  month = str_extract(tadids, "(?<=2019)[0-9]{2}"),
  day = str_extract(tadids, "(?<=2019[0-9]{2})[0-9]{2}"),
  camera = str_extract(tadids, "[0-9]$"),
  trynum = str_extract(tadids, "(?<=[0-9]{2}_)[0-9](?=_2019)"),
  stringsAsFactors = FALSE
)
vidinfo$treatment[vidinfo$treatment == "W"] <- "Wild" # update the "wild" tag
vidinfo$date <- as.numeric(vidinfo$date)
vidinfo$fullname <- as.character(vidinfo$fullname)
vidinfo$pond <- factor(vidinfo$pond)
vidinfo$clutch <- factor(vidinfo$clutch)
vidinfo$treatment <- factor(vidinfo$treatment)
vidinfo$month <- as.numeric(vidinfo$month)
vidinfo$day <- as.numeric(vidinfo$day)
vidinfo$camera <- factor(vidinfo$camera)
vidinfo$trynum <- as.numeric(vidinfo$trynum)

for(i in 1:nrow(vidinfo)){
  if(vidinfo$treatment[i] == "Wild"){
    vidinfo$indiv[i] <- str_extract(vidinfo$fullname[i], "(?<=W_)[0-9]{2}") # if wild, find the number
  }
  else{
    vidinfo$indiv[i] <- str_extract(vidinfo$fullname[i], "[[:upper:]](?=_Low|_High)") # if not wild, find the letter
  }
}
sample_n(vidinfo, 15) # sanity check

###########################################################################
########################### SPATIAL SCALING ###############################
###########################################################################

# We have two main scalings
x_main = 0.65/1497.5
y_main = 0.45/1036.7
x_alt = 0.65/1398.5
y_alt = 0.45/968.2

# The dates for the alternate scalings are as follows:
# 5/29 Camera 2
# 5/30 Camera 2
# 6/5 Camera 2
# 6/6 Camera 2
# 6/18 Camera 2

alternates <- vidinfo$fullname[vidinfo$date %in% c("20190529", "20190530", 
                                                   "20190605", "20190606", 
                                                   "20190618")]
regulars <- vidinfo$fullname[!vidinfo$fullname %in% alternates]

# Split `coords` by whether each video needs alternate scaling or not.
alternates_coords <- coords[names(coords) %in% alternates]
regular_coords <- coords[names(coords) %in% regulars]

alternates_trajs <- lapply(alternates_coords, trajMakeScale, x_alt, y_alt) # make and scale trajectories
regular_trajs <- lapply(regular_coords, trajMakeScale, x_main, y_main) # make and scale trajectories

#regular_trajs <- vector("list", length(regular_coords)) # this does the same thing as the lapply line above, but provides syntax for iterating through the files, in case there are errors and this is necessary.
#names(regular_trajs) <- names(regular_coords)
#for(i in 1:length(regular_trajs)){
  #tryCatch(
    #expr = {
      #trj <- trajMakeScale(regular_coords[[i]], x_main, y_main)
      #regular_trajs[[i]] <- trj
      #message("Iteration ", i, " successful.")
    #},
    #error = function(e){ 
      #message("* Caught an error on iteration", i)
      #print(e)
    #}
  #)
#}

(problems <- regulars[lengths(regular_trajs) == 0])
# went back and manually re-ran these two videos in matlab. The coordinates look fine! No idea what was wrong...
# now no more problems. Yay!

###########################################################################
######################## CALCULATE SPEED ##################################
###########################################################################

# Merge the regulars and the alternates back together
alltrajs <- c(alternates_trajs, regular_trajs) # this just binds them in the order specified
  # Restore alphabetical order
  alltrajs <- alltrajs[sort(names(alltrajs))] # all trajectories
  
length(coords) == length(alltrajs) # check that it's the same length as the original coords list. Should be TRUE.
sum(names(coords) != names(alltrajs)) # check that the names line up with the original coords list. Should be 0.

# Get speeds for each trial, keeping it as a nested list
speeds <- lapply(alltrajs, getSpeedDFs)


#speeds <- vector("list", length(alltrajs)) # this would be the non-lapply version of this function, as above.
#names(speeds) <- names(alltrajs)
#for(i in 1:length(alltrajs)){
  #tryCatch(
    #expr = {
      #speedDFs <- getSpeedDFs(alltrajs[[i]])
      #speeds[[i]] <- speedDFs
      #message("Iteration ", i, " successful.")
    #},
    #error = function(e){ 
      #message("* Caught an error on iteration", i)
      #print(e)
    #}
  #)
#}

(problems <- names(alltrajs)[lengths(speeds) == 0])
# Fixed two problem coordinates by hand by re-running in matlab. Possibly this was an issue with an incorrect size threshold? Not sure why that would be the case, though.
# Yay!

# Assign trial numbers and bind individual data frames into one for each tadpole
speeds <- lapply(speeds, rbindlist, idcol = "trial")

# Bind all tadpoles together into one list
speedsDF <- rbindlist(speeds, idcol = "fullname", use.names = T)

# Join more info from vidinfo
speedsDF <- left_join(speedsDF, vidinfo, by = "fullname") # the vidinfo data frame is useful!

###########################################################################
##################### SMOOTHING SPEEDS ####################################
###########################################################################

# We will use three different methods to automatically smooth the tadpole trajectories, in order to find the burst beginnings.
# 1. Moving averages of speeds
# 2. Moving averages of turning angles between frames
# 3. Moving averages of the number of pixels different between successive frames (black to white or white to black)
#     - for a reminder of how this was calculated, have a look back at the matlab scripts.

# Take moving averages of the speeds
speedsDF <- speedsDF %>% group_by(fullname, trial) %>% 
  mutate(speedma5 = rollapplyr(speed, 5, mean, fill = NA))

# Standardize the angles (center around 0)
speedsDF <- speedsDF %>% group_by(fullname, trial) %>%
  mutate(anglet = angleTransform(angle))

# Take moving averages of the angles
speedsDF <- speedsDF %>% group_by(fullname, trial) %>%
  mutate(atma5 = rollapplyr(anglet, 5, mean, fill = NA))

# Taking moving averages of the pixel differences
speedsDF <- speedsDF %>% group_by(fullname, trial) %>%
  mutate(pxma5 = rollapplyr(px, 5, mean, fill = NA))

##############################################################################################
##################### AUTOMATICALLY FIND BURST BEGINNINGS ####################################
##############################################################################################
# Load true burst beginnings
# We went through and found "true" frame numbers for the first trial of 100 randomly selected videos.

trueframes <- read.csv("getframenums.csv")
trueframes <- trueframes[,c(1, 5)]
names(trueframes) <- c("fullname", "frame")
trueframes$fullname <- gsub("_edit|.avi", "", trueframes$fullname)

# calculate beginnings using speed and angle and pixels
trueframes <- trueframes[trueframes$fullname %in% speedsDF$fullname,]
for(i in 1:nrow(trueframes)){
    df <- speedsDF %>% filter(fullname == trueframes$fullname[i], trial == "1") # pick out the video and trial
    trueframes$beg_sp[i] <- findBeginningSpeed(df, "speedma5") # find beginning using speed
    trueframes$beg_ag[i] <- findBeginningAngle(df, "atma5") # find beginning using angle
    trueframes$beg_px[i] <- findBeginningPx(df, "pxma5") # find beginning using pixels
}

# Calculate differences between each method and the actual frame
trueframes$spdiff <- trueframes$beg_sp - trueframes$frame
trueframes$agdiff <- trueframes$beg_ag - trueframes$frame
trueframes$pxdiff <- trueframes$beg_px - trueframes$frame

# Calculate z-scores for the differences. I still don't quite get this.
trueframes$spz <- (trueframes$spdiff - mean(trueframes$spdiff))/std(trueframes$spdiff)
trueframes$agz <- (trueframes$agdiff - mean(trueframes$agdiff))/std(trueframes$agdiff)
trueframes$pxz <- (trueframes$pxdiff - mean(trueframes$pxdiff))/std(trueframes$pxdiff)

# Visualize z-scores for each method
ggplot(trueframes, aes(x = fullname))+
  geom_point(aes(y = spz), col = "blue")+
  geom_point(aes(y = agz), col = "red", alpha = 0.9)+
  geom_point(aes(y = pxz), col = "green", alpha = 0.7) +
  ylab("Z-score")

# Make individual models for each method
spm <- lm(frame~beg_sp, data = trueframes)
pxm <- lm(frame~beg_px, data = trueframes)
agm <- lm(frame~beg_ag, data = trueframes)
# View the model results
summary(spm)
summary(pxm)
summary(agm)

# Combined model for all three predictors
allm <- lm(frame~beg_sp+beg_ag+beg_px, data = trueframes)
summary(allm)
# notice that the sp predictor isn't significant; what if we remove it?

# Model with angle and pixels
agpxm <- lm(frame~beg_ag+beg_px, data = trueframes)
summary(agpxm)
#this does marginally better than using any of the three predictors alone, and it doesn't do worse than before we removed sp. 
plot(fitted(agpxm)~trueframes$frame)

# Add the fitted model values onto the trueframes data frame
trueframes$fitted <- fitted(agpxm)
trueframes$resid <- trueframes$fitted - trueframes$frame

############################################################################
############### REMOVING OUTLIERS BEFORE FITTING MODEL #####################
############################################################################

trueframes$estimates_avg <- rowMeans(trueframes[,c("beg_sp", "beg_ag", "beg_px")], na.rm = T) # for each trial, get the average of the three estimates
trueframes$estimates_var <- rowVars(as.matrix(trueframes[,c("beg_sp", "beg_ag", "beg_px")]), na.rm = T)
trueframes$problem <- ifelse(abs(trueframes$agdiff) > 3, T, F) # For testing purposes, identify the ones in this dataset that are really wrong

# Make a data frame of thresholds to test
threshes <- data.frame(
  thresh = seq(1, 30, by = 0.1),
  fnr = NA,
  fpr = NA,
  prop_byhand = NA,
  total_fn_prop = NA
)

# Calculate false negative and false positive rates for each threshold
for(i in 1:nrow(threshes)){
  namesflagged <- trueframes$fullname[trueframes$estimates_var > threshes$thresh[i]]
  namesnotflagged <- trueframes$fullname[trueframes$estimates_var <= threshes$thresh[i]]
  namespos <- trueframes$fullname[trueframes$problem == T]
  namesneg <- trueframes$fullname[trueframes$problem == F]
  npos = length(namespos)
  nneg = length(namesneg)
  nflagged <- length(namesflagged)
  nnotflagged <- length(namesnotflagged)

  names_fn <- namespos[namespos %in% namesnotflagged]
  names_fp <- namesneg[namesneg %in% namesflagged]
  nfn <- length(names_fn) # number of false negatives 
  nfp <- length(names_fp) # number of false positives
  
  fnr <- nfn/npos # false negative rate
  threshes$fnr[i] <- fnr
  total_fn_prop <- nfn/99
  
  threshes$total_fn_prop[i] <- total_fn_prop # proportion of the total data that will be false negatives
  
  fpr <- nfp/nneg # false positive rate
  threshes$fpr[i] <- fpr
  
  prop_byhand = nflagged/99 #what proportion of the data will we have to do by hand
  threshes$prop_byhand[i] <- prop_byhand
}

# Convert threshes to a long data frame for plotting
threshes_long <- melt(threshes, id.vars = "thresh", variable.name = "rate")

# Plot thresholds vs. how many it flags
ggplot(threshes_long, aes(x = thresh, y = value, col = rate))+
  geom_line()+
  xlab("Variance threshold")+
  ylab("Proportion")

# 5 seems to be a good threshold

# find the false negatives in this test data set
trueframes$flagged <- ifelse(trueframes$estimates_var > 5, T, F)
fns <- trueframes[trueframes$flagged == F & trueframes$problem == T,]
fns_names <- fns$fullname

#nothing obvious is going on, which is too bad, bc that means we can't filter it out
summary(fns$agdiff) # how different is the angle estimate from the true frame?
summary(fns$resid) # it's even better with the residuals.
# this isn't bad

################################################################################
################ FOLLOWING THAT PROTOCOL #######################################
################################################################################

################## TEST #######################
trueframes$byhand <- ifelse(trueframes$estimates_var > 5, T, F) # which ones will we have to do by hand

trueframes_auto <- trueframes %>% filter(byhand == F)
model <- lm(frame~beg_ag+beg_px, data = trueframes_auto)
summary(model) # this is crazy good, which makes sense

################# FULL DATASET ################
# How many total trials do we have?
a <- length(unique(speedsDF[,c("fullname", "trial")])$fullname)
message(paste('we have', a, 'individual tadpole/trials'))

# Find all the unique trials
starts <- data.frame(
  fullname = unique(speedsDF[,c("fullname", "trial")])$fullname,
  trial = unique(speedsDF[,c("fullname", "trial")])$trial
)

# Compute beginnings using speed, angle, and pixels for each tadpole/trial
# Must find a way to speed this up.
#for(i in 1:nrow(starts)){
#  tryCatch(
#    expr = {
#      df <- speedsDF %>% filter(fullname == starts$fullname[i], trial == starts$trial[i])
#      starts$beg_sp[i] <- findBeginningSpeed(df, "speedma5")
#      starts$beg_ag[i] <- findBeginningAngle(df, "atma5")
#      starts$beg_px[i] <- findBeginningPx(df, "pxma5") + 1
#      message("Iteration ", i, " successful.")
#    },
#    error = function(e){
#      message("* Caught an error on iteration", i)
#      print(e)
#    }
#  )
#}

#save(starts, file = "starts.Rda")
load("starts.Rda")

# Function to replace Inf values with NA
inf2NA <- function(vec){
  vec[is.infinite(vec)] <- NA
  return(vec)
}

# Replace Inf with NA
starts2 <- starts %>% mutate_each(funs(inf2NA))

# Compute estimates variance
starts2$estimates_var <- rowVars(as.matrix(starts2[,c("beg_sp", "beg_ag", "beg_px")]), na.rm = T)

# Flag observations with var > 5
starts2$byhand <- ifelse(starts2$estimates_var >= 5, T, F)
# What percent of the observations were flagged as "by hand"?
(pct_byhand <- sum(starts2$byhand, na.rm = T)/nrow(starts2))

# Which ones do we need to do by hand
todobyhand <- starts2 %>% filter(byhand == TRUE)

#write.csv(todobyhand[,1:2], "todobyhand.csv", row.names = F)
#check100 <- todobyhand[sample(1:nrow(todobyhand)),]
#write.csv(check100[,1:2], "check100.csv", row.names = F)
  # whoops this just sampled them in a random order

################################################################################
######### AUTOMATIC BEGINNING IDENTIFICATION FOR NON-FLAGGED TRIALS ############
################################################################################

automatics <- starts2 %>% filter(byhand == FALSE)
nrow(automatics)
automatics$fitted <- predict.lm(model, newdata = automatics)

################################################################################################
################## VERIFY AUTOMATIC AND MANUAL METHODS #########################################
################################################################################################

# Aggregate the documents involving manual identification of trial beginnings
todobyhand <- read.csv("todobyhand_kaija.csv", header = T)
todobyhand %<>% rename(kaija_frame = frame_indexed_0) %>%
  mutate(kaija_frame = kaija_frame + 1) # frames were indexed from 0 in quicktime; now they're indexed from 1
joaquin <- read.csv("joaquin_check100.csv", header = T)
joaquin %<>% mutate(joaquin_frame = joaquin_frame + 1) # frames were indexed from 0 in quicktime; now they're indexed from 1.

check100 <- left_join(joaquin, todobyhand, by = c("fullname", "trial"))

check100 %<>% mutate(diff = joaquin_frame - kaija_frame)

mistakes <- check100 %>% filter(abs(diff) > 10)
# I checked these manually. In the three cases with huge differences, Joaquin accidentally recorded the wrong trial (my estimate was correct when double-checked). In the last case, the tadpole wiggled by itself before or after being poked, and there was a difference of about 20 frames. This is ok to just build into the error.

# Let's calculate some accuracy stats
check100$mistake <- ifelse(abs(check100$diff) > 10, T, F)
(summary(check100$diff[check100$mistake == F]))
check100 %>% filter(mistake == F) %>%
  ggplot()+
  geom_boxplot(aes(y = diff))

# Compare to the automatics
trueframes %>% filter(byhand == F) %>%
  ggplot()+
  geom_boxplot(aes(y = agdiff))

# Compare these side by side
trueframes$trial <- 1
autos <- trueframes %>% filter(byhand == F) %>% select(fullname, trial, resid) %>% rename(diff = resid) %>% 
  mutate(Method = "Automatic")
mans <- check100 %>% filter(mistake == F) %>% select(fullname, trial, diff) %>% mutate(Method = "Manual")

# Comparison density plot
comparison <- rbind(autos, mans)
ggplot(comparison)+
  geom_density(aes(x = diff, fill = Method, col = Method), alpha = 0.4)+
  ggtitle("Error from automatic vs. manual frame selection")+
  xlab("Difference between estimates (frames at 60fps)")+
  ylab("")

# Comparison boxplot
ggplot(comparison) + 
  geom_boxplot(aes(y = diff, fill = Method, col = Method), alpha = 0.4)+
  ggtitle("Error from automatic vs. manual frame selection")+
  ylab("Difference between estimates (frames at 60fps)")+
  xlab("")
# this looks great, we have the same error between automatic and manual methods. 

################################################################################################
#################### COMBINE AUTOMATIC AND MANUAL FRAME DATA ###################################
################################################################################################

# gather automatic and manual data and prepare it for merge.
head(todobyhand)
todobyhand_1 <- todobyhand %>% select(-comments) %>% rename(frame_start_absolute = kaija_frame)
head(automatics)
automatics_1 <- automatics %>% select(fullname, trial, fitted) %>% rename(frame_start = fitted)

# compare the starting frames
hist(todobyhand_1$frame_start_absolute)
hist(automatics_1$frame_start)
# WE HAVE A PROBLEM! The frames that I did by hand are in reference to the beginning of the entire video, not to the beginning of the trial. 

# Read in the frames files 
#frames <- list.files(path = "frames", full.names = T) # list frames files
#frames_errors <- frames[grepl('error', frames, ignore.case = T)]
#frames <- frames[!grepl('error', frames, ignore.case = T)] # ignore error frames files
#frameslist <- lapply(frames, read.table, sep = ",") # read in frames files

# Find tadpole names
#tadids_frames <- str_extract(frames, "(?<=frames_).*(?=\\.txt)")
#tadids_frames <- gsub("_edit", "", tadids_frames)
#names(frameslist) <- tadids_frames # name by tadpole id

#sum(todobyhand_1$fullname %in% names(frameslist)) == nrow(todobyhand) # are all the byhand trials included in the frameslist? They are!

# Because MATLAB weirdly downsampled the data by half, we need to double the number of rows in each data frame.
#insertrows <- function(df){ # function to repeat rows
  #df2 <- df[rep(1:nrow(df),1,each=2),]
  #df2$newabsolute <- 1:nrow(df2)
  #return(df2)
#}

# apply the function to all of the data frames
#frameslist <- lapply(frameslist, insertrows)

# now go in and get the relative frames
# this part isn't working at all. I think there's something really messed up about the frame numbering in the matlab files. This problem persists even after I've interpolated frames.
# We know we have pretty good confidence in the automatic trial assignment, because we cross-checked that with a manual method
#   manual method used quicktime
#   manual method included only first trials. So unless the frame count is not constant throughout the video in quicktime, then it should apply broadly.
#   we ended up with a very low error rate, so we know that manual and automatic assignment agree
#   matlab, not quicktime, seems to be the problem here
#   Feel pretty confident, therefore, that our manually-assigned absolute frame numbers are correct
#   what if we go back in and manually write down the first frame of each trial in quicktime?
    # this might actually be a pretty good manual fix. then we can dispense with the weird matlab bullshit once and for all.
#for(i in 1:nrow(todobyhand_1)){
  #tryCatch(
    #expr = {
     # tadpole <- todobyhand_1$fullname[i] # which tadpole are we dealing with?
     # trial <- todobyhand_1$trial[i] # which trial are we dealing with?
     # abs <- todobyhand_1$frame_start_absolute[i] # absolute start frame number
     # df <- frameslist[[tadpole]] # pick out only the correct tadpole
     # df_subset <- df[df$V2 == trial,] # pick out only the frames included in the specified trial
     # df_subset[,"index"] <- 1:nrow(df_subset) # number the frames from the beginning of the trial
     # rel <- df_subset$index[df_subset$newabsolute == abs] # the index corresponding to the absolute start frame
     # todobyhand_1$frame_start[i] <- rel
     # message("Iteration ", i, " successful.")
   # },
   # error = function(e){
     # message("* Caught an error on iteration", i)
      #print(e)
   # }
 # )
#}

# iterate through the absolute frames and return relative frames
#for(i in 1:nrow(todobyhand_1)){
 # tadpole <- todobyhand_1$fullname[i] # which tadpole are we dealing with?
 # trial <- todobyhand_1$trial[i] # which trial are we dealing with?
#  abs <- todobyhand_1$frame_start[i] # absolute start frame number
#  df <- frameslist[[tadpole]] # pick out only the correct tadpole
#  df_subset <- df[df$V2 == trial,] # pick out only the frames included in the specified trial
#  df_subset[,"index"] <- 1:nrow(df_subset) # number the frames from the beginning of the trial
#  rel <- df_subset$index[df_subset$V1 == abs] # the index corresponding to the absolute start frame
 # todobyhand_1$frame_start[i] <- rel
#}



# get all trial start frames together in one place
# Provisional: change the names
names(todobyhand_1)[3] <- "frame_start"
todobyhand_1$frame_start <- NA
trial_start_frames <- rbind(todobyhand_1, automatics_1) %>% as.data.frame()
trial_start_frames <- trial_start_frames[order(trial_start_frames$fullname),] %>% mutate(frame_start = ceiling(frame_start))


################################################################################################
################################## EDIT BURSTS #################################################
################################################################################################

# assign frame numbers to the speeds DF, using the tally_by_group function.
trial_start_frames <- trial_start_frames %>% mutate(unique_trial = paste(fullname, trial))
speedsDF <- speedsDF %>% mutate(unique_trial = paste(fullname, trial)) %>% as.data.frame()
speedsDF1 <- tally_by_group(speedsDF, group_col = "unique_trial")

# Remove any frames that occur before the beginning of the burst trial
speedsDF2 <- left_join(speedsDF1, trial_start_frames[,c("unique_trial", "frame_start")], by = "unique_trial")
speedsDF3 <- speedsDF2 %>% filter(frame >= frame_start)
speedsDF3 <- speedsDF3 %>% rename(frame_absolute = frame)
speedsDF4 <- tally_by_group(speedsDF3, group_col = "unique_trial") %>% rename(frame_relative = frame)

# Clip the bursts to different lengths
bursts <- speedsDF4 %>% select(-c(time, angle, px, date, anglet, atma5, pxma5, px, frame_absolute, frame_start)) %>% mutate(crop = "Full burst")
bursts_sec <- bursts %>% filter(frame_relative <= 60) %>% mutate(crop = "First second")
bursts_halfsec <- bursts %>% filter(frame_relative <= 30) %>% mutate(crop = "First half second")
bursts_allcrops <- rbind(bursts, bursts_sec, bursts_halfsec)

tadpoles <- unique(bursts$fullname)

# Exploratory plotting
bursts_halfsec %>% filter(fullname == tadpoles[130]) %>% ggplot(aes(x = frame_relative, y = speedma5, col = factor(trial)))+
  geom_line()

# Compute statistics
full_stats <- bursts %>% 
  group_by(fullname, trial, pond, clutch, indiv, treatment, year, month, day, 
           camera, trynum, unique_trial) %>% 
  summarize(avgspeed = mean(speed),
            maxspeed = max(speed, na.rm = T),
            maxspeedma = max(speedma5, na.rm = T)) %>%
  mutate(crop = "Full burst") %>% as.data.frame()

sec_stats <- bursts_sec %>% 
  group_by(fullname, trial, pond, clutch, indiv, treatment, year, month, day, 
           camera, trynum, unique_trial) %>% 
  summarize(avgspeed = mean(speed),
            maxspeed = max(speed, na.rm = T),
            maxspeedma = max(speedma5, na.rm = T)) %>%
  mutate(crop = "First second") %>% as.data.frame()

halfsec_stats <- bursts_halfsec %>% 
  group_by(fullname, trial, pond, clutch, indiv, treatment, year, month, day, 
           camera, trynum, unique_trial) %>% 
  summarize(avgspeed = mean(speed),
            maxspeed = max(speed, na.rm = T),
            maxspeedma = max(speedma5, na.rm = T)) %>% 
  mutate(crop = "First half second") %>% as.data.frame()

# Bind the three stats data frames together into long format
stats <- rbind(full_stats, sec_stats, halfsec_stats) %>% as.data.frame()

############################################################################
######################## EXPLORATORY PLOTTING ##############################
############################################################################

# Average speed over the interval
stats %>% ggplot(aes(x = treatment))+
  geom_boxplot(aes(y = avgspeed, fill = treatment))+
  facet_wrap(~ crop) + 
  ggtitle("Average speed over the burst interval")

# Maximum speed
stats %>% ggplot(aes(x = treatment))+
  geom_boxplot(aes(y = maxspeed, fill = treatment))+
  facet_wrap(~ crop)+
  ggtitle("Maximum speed")

# Log-transformed maximum speed
stats %>% ggplot(aes(x = treatment))+
  geom_boxplot(aes(y = log(maxspeed), fill = treatment))+
  facet_wrap(~ crop) +
  ggtitle("Maximum speed (log-transformed)")

# Maximum speed (moving average)
stats %>% ggplot(aes(x = treatment))+
  geom_boxplot(aes(y = maxspeedma, fill = treatment))+
  facet_wrap(~ crop)+
  ggtitle("Maximum speed (after moving average)")

# Log-transformed maximum speed (moving average)
stats %>% ggplot(aes(x = treatment))+
  geom_boxplot(aes(y = log(maxspeedma), fill = treatment))+
  facet_wrap(~ crop)+
  ggtitle("Maximum speed (after moving average, log-transformed)")

##### Does speed decrease by trial number? #####
stats %>% filter(trial < 5, crop == "First second") %>%
  ggplot(aes(x = jitter(trial), y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y ~ x, se = T)

stats %>% filter(trial < 5, crop == "First second") %>%
  ggplot(aes(x = jitter(trial), y = avgspeed))+
  geom_point()+
  geom_smooth(method = 'lm', formulat = y ~ x, se = T)

tiring_model <- lm(avgspeed ~ trial + treatment, data =  stats)
summary(tiring_model)

########## Make some plots of trajectories ############
# trajectory plot
tadplot <-  coords[[24]] %>% ggplot(aes(x, y, col = factor(trial)))+
  geom_point(size = 2, alpha = 0.4)+
  guides(colour = guide_legend(override.aes = list(size=10, alpha = 1)))+
  coord_fixed()+
  scale_color_viridis(discrete = T)+
  theme_minimal()+
  labs(color = "Trial")+
  xlab("")+
  ylab("")+
  theme(axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20))
tadplot
#ggsave(file = "graphics/tadplot.svg", plot = tadplot)

# speed plot
speedplot <- bursts_allcrops %>% filter(fullname == names(coords)[24]) %>% ggplot()+
  geom_line(aes(x = frame_relative/60, y = speedma5, col = factor(trial)))+
  facet_wrap(~crop, scales = "free")+
  scale_color_viridis(discrete = T)+
  xlab("Time (s)")+
  ylab("Speed (m/s)")+
  labs(color = "Trial")+
  theme_minimal()
speedplot

#ggsave(file = "graphics/speedplot.svg", plot = speedplot)

firsthalfsec <- bursts_halfsec %>% filter(fullname == names(coords)[24]) %>% ggplot(aes(x = frame_relative/60, y = speedma5, col = factor(trial)))+
  geom_line(size = 1.5)+
  scale_color_viridis(discrete = T)+
  xlab("Time (s)")+
  ylab("Speed (m/s)")+
  labs(color = "Trial")+
  theme_minimal() +
  ggtitle("Speed profiles of burst trials")

firsthalfsec
ggsave(file = "graphics/firsthalfsec.svg", plot = firsthalfsec, width = 6, height = 4)

# The tadpole above happens to not have any trials that we had to do by hand. 


#ggsave("graphics/tadplot.svg", plot = tadplot, device = "svg")

####### Read in manual morphometric data
morpho <- read.csv("Combined_2019RespData_20190729.csv", as.is = T)
morpho <- morpho %>% mutate(tadpole = paste(ID, tolower(Treat), sep = "-"))
morpho$tadpole <- gsub("LOW-", "", morpho$tadpole)
morpho$tadpole <- gsub("_", "-", morpho$tadpole)
stats <- stats %>% mutate(tadpole = paste(pond, clutch, indiv, tolower(treatment), sep = "-"))
stats$tadpole <- gsub("-NA-", "-W-", stats$tadpole)
a <- unique(stats$tadpole) %in% unique(morpho$tadpole)
b <- unique(morpho$tadpole) %in% unique(stats$tadpole)
# not sure why there are some that don't match, but oh well

nrow(morpho)
length(unique(morpho$tadpole))
# There is one tadpole that was measured twice, on days 163 and 165. Respirometry for that tadpole was run on 164, so for consistency with the other tadpoles, we will take the measurement from 165.
morpho <- morpho %>% filter(!(tadpole == "CPS-W-11-wild" & Resp.DOY == "165"))
nrow(morpho) == length(unique(morpho$tadpole)) # ok, we can proceed. 


colorscale <- scale_color_manual(name = "Treatment", values = c("darkorange", "cyan3", "black"))
fillscale <- scale_fill_manual(name = "Treatment", values = c("darkorange", "cyan3", "black"))


# Join morphological data to stats
stats2 <- left_join(stats, morpho, by = "tadpole") %>% select(-c(Treat, Pond, Clutch, ID, Run, CH, AZAtag, Coll.DOY, Indiv))

# Explore linear regression
# GS vs. speed by treatment
stats2 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = jitter(GS), y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  ggtitle("Avg speed vs. Gosner Stage by treatment")+
  xlab("Gosner stage")+
  ylab("Average Speed (first half second)")+
  labs(col = "Treatment")+
  colorscale + theme_minimal()

# Mass vs. speed by treatment
stats2 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = Mass, y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  ggtitle("Avg speed vs. mass by treatment")+
  xlab("Mass")+
  ylab("Average Speed (first half second)")+
  labs(col = "Treatment")+
  colorscale + theme_minimal()

# SVL vs. speed by treatment
stats2 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = SVL, y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  ggtitle("Avg speed vs. SVL by treatment")+
  xlab("SVL")+
  ylab("Average Speed (first half second)")+
  labs(col = "Treatment")+
  colorscale + theme_minimal()

# Total length vs. speed by treatment
stats2 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = Tot.L, y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  ggtitle("Avg speed vs. total length by treatment")+
  xlab("Total length")+
  ylab("Average Speed (first half second)")+
  labs(col = "Treatment") +
  colorscale + theme_minimal

# Tail depth vs. speed by treatment
stats2 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = Tail.D, y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  ggtitle("Avg speed vs. tail depth by treatment")+
  xlab("Tail depth")+
  ylab("Average Speed (first half second)")+
  labs(col = "Treatment")+
  colorscale + theme_minimal()

# Resp DOY vs. speed by treatment
stats2 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = Resp.DOY, y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  ggtitle("Avg speed vs. day of year by treatment")+
  xlab("Day of year")+
  ylab("Average Speed (first half second)")+
  labs(col = "Treatment") + colorscale +
  theme_minimal()

# Do PCA on the morphometric data
morphodata <- stats2[,19:22] %>% na.omit()
morphopca <- princomp(morphodata, cor = TRUE, scores = TRUE)
morphodata[,5:8] <- morphopca$scores
names(morphodata)[5:8] <- c("PC1", "PC2", "PC3", "PC4")

stats3 <- left_join(stats2, morphodata, by = c("Mass", "SVL", "Tot.L", "Tail.D"))
stats3 <- stats3[!duplicated(stats3),]

# Morphometric body size ellipses
stats3 %>% ggplot(aes(x = PC1, y = PC2, col = treatment))+
  geom_point()+
  stat_ellipse()+
  ggtitle("Morphometrics PCA plot: separation of tadpoles by treatment")+
  labs(col = "Treatment") + 
  colorscale + theme_minimal()

# Plot the PC axes against some other predictors
stats3 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = PC1, y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  ggtitle("Avg speed vs. PC1 by treatment")+
  xlab("PC1")+
  ylab("Average Speed (first half second)")+
  labs(col = "Treatment") + 
  colorscale + theme_minimal()

stats3 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = PC2, y = avgspeed, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  ggtitle("Avg speed vs. PC2 by treatment")+
  xlab("PC2")+
  ylab("Average Speed (first half second)")+
  labs(col = "Treatment") + colorscale + theme_minimal()

# PC1 vs. GS
stats3 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = jitter(GS), y = PC1, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  labs(col = "Treatment") + colorscale + theme_minimal()

# PC2 vs. GS
stats3 %>% filter(crop == "First half second") %>% 
  ggplot(aes(x = jitter(GS), y = PC2, col = treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  labs(col = "Treatment") + colorscale + theme_minimal()

# PC1 is highly correlated with GS, so we shouldn't use those together in a model
dat <- stats3 %>% filter(crop == "First half second")

mod1 <- lm(avgspeed ~ treatment + GS + PC2 + trial + pond, data = dat)
summary(mod1) # this seems to be the best model I can get

library(lme4)
mixed <- lmer(avgspeed ~ treatment + GS + PC2 + trial + pond + (1|clutch) + (1|indiv), data = dat)
summary(mixed)
coef(mixed)

# Dev rate and growth rate proxies
dat <- dat %>% mutate(devrateproxy = GS/Resp.DOY,
                      growthrateproxy = PC1/Resp.DOY)
devrates <- dat %>% ggplot(aes(x = devrateproxy, y = avgspeed, col = treatment))+
  geom_point(size = 0.5)+
  geom_smooth(method = 'lm', se = F) + colorscale + theme_minimal()+
  ylab("Burst speed (m/s)") +
  xlab("Developmental rate (GS/day)") +
  ggtitle("Relationship of burst speed to developmental rate by treatment")
devrates
#ggsave(devrates, file = "graphics/devrates.svg", width = 6, height = 4)

devratedensity <- dat %>% ggplot(aes(x = devrateproxy, fill = treatment, col = treatment))+
  geom_density(alpha = 0.5) +
  colorscale + fillscale +
  theme_minimal()+
  ylab("") +
  xlab("Developmental rate")+
  ggtitle("Developmental rate by treatment")
devratedensity
#ggsave(devratedensity, file = "graphics/devratedensity.svg", width = 7, height = 4)
  
pondeffects <- dat %>% ggplot(aes(x = pond, y = avgspeed))+
  geom_boxplot(aes(fill = treatment, col = treatment), alpha = 0.5, show.legend = F)+
  colorscale + fillscale +
  theme_minimal() +
  xlab("Pond") +
  ylab("Burst speed (m/s)")
  
pondeffects
#ggsave(pondeffects, file = "graphics/pondeffects.svg", width = 6.5, height = 4)
