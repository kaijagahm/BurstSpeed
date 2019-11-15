#Random rename image files in preparation for digitizing landmarks
library(stringr)

setwd("2019_PostResp_photos/Exposure_Edit/for_landmarks/")

# Get a list of the files in the directory
files <- list.files()

# remove the four-digit numbers that photoshop appended
newnames <- files
pattern <- "(?<=straightened)\\_\\d{4}"
newnames <- str_replace(newnames, pattern, "")

file.rename(from = files, to = newnames) # that worked for most of them, but inexplicably some of them have a double hyphen.

newnames2 <- list.files()
pattern2 <- "(?<=straightened)\\_\\_\\d{4}"
newnames2 <- str_replace(newnames2, pattern2, "")

file.rename(from = list.files(), to = newnames2)

###
# Append random integers
# get list of files, excluding tps files
files <- list.files()
tpsfiles <- which(grepl(".tps", files))
files <- files[-tpsfiles]

set.seed(3)
numbers <- sample(1:length(files), length(files), replace = FALSE) %>% as.character()

# add leading zeroes as necessary
numbers <- str_pad(numbers, width = 3, side = "left", pad = "0")

#make new names and assign them
names_with_randints <- paste0(numbers, "_", files)
file.rename(from = files, to = names_with_randints)

