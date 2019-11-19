# Detect which job runs gave errors
coordfiles <- list.files("coordinates/")
successtadpoles <- str_extract(coordfiles, pattern = "(?<=coordinates\\_).*(?=\\.txt)")

videofiles <- read.table("videofiles.txt")
videofiles$V1 <- as.character(videofiles$V1)
alltadpoles <- str_extract(videofiles$V1, pattern = "^.*(?=\\_edit)")

failedtadpoles <- alltadpoles[!(alltadpoles %in% successtadpoles)]
failedtadpoles <- failedtadpoles[failedtadpoles != "BO_01_A"]
failedtadpoles

# need to delete these videos from the cluster too (priority), and from their respective folders (less imp.)