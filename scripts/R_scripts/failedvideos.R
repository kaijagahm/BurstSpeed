# Figure out which videos failed to run on the cluster
status <- read.table("job_36315155_status.tsv")
status2 <- status[order(status$V1), c(1:8, 17)]
names(status2) <- c("index", "exitcode", "node", "startdate", "starttime", "enddate", "endtime", "time", "command")
failed <- status2[status2$exitcode != 0,]

jobs <- read.table("jobs.txt")
nrow(jobs)
jobs$number <- 0:(nrow(jobs)-1)

jobs_failed <- jobs[jobs$number %in% failed$index,]

jobs_failed2 <- jobs_failed[,1:11] # without the number column
# write this to a table: ## write.table(jobs_failed2, file = "tryagain_jobs.txt", sep = " ", col.names = F, quote = F, row.names = F)


# Which videos still failed to run after trying with more memory?
status <- read.table("job_36316463_status.tsv")
status2 <- status[order(status$V1), c(1:8, 17)]
names(status2) <- c("index", "exitcode", "node", "startdate", "starttime", "enddate", "endtime", "time", "command")
failed <- status2[status2$exitcode != 0,]
failed