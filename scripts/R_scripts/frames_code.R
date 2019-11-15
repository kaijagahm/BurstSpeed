# Code for dealing with video frames, given information about which frames belong to which trial. 

########################################################
# Identifying the beginnings of trials

# Read in frames files
frames_files <- list.files(path = "frames", full.names = T) # list frames files
frames_files <- frames_files[!grepl('error', frames_files, ignore.case = T)]
frames <- lapply(frames_files, read.table, sep = ",") # read in frames files

# Find tadpole names
tadids <- str_extract(frames_files, "(?<=frames_).*(?=\\.txt)")
tadids <- gsub("_edit", "", tadids)
names(frames) <- tadids # name by tadpole id

# Name and number coordinates; change NaN's to NA's.
frames <- lapply(frames, renameFrames)
frames <- lapply(frames, nantona)
for(i in 1:length(frames)){
  frames[[i]][,"fullname"] <- tadids[i]
}