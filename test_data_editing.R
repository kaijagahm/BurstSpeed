library(tidyverse)
library(sf)
pythag <- function(x,y){
  diagdist <- sqrt(x^2 + y^2)
  return(diagdist)
}

diag <- function(x1, x2, y1, y2){
  deltax <- x2-x1
  deltay <- y2-y1
  diag <- pythag(deltax, deltay)
  return(diag)
}
  
  
# Load files
a_files <- list.files(path = "trajectories/A/", full.names = T)
e_files <- list.files(path = "trajectories/E/", full.names = T)
f_files <- list.files(path = "trajectories/F/", full.names = T)
g_files <- list.files(path = "trajectories/G/", full.names = T)
a <- lapply(a_files, function(x) read.table(x, header = T)[,1:2])
e <- lapply(e_files, function(x) read.table(x, header = T)[,1:2])
f <- lapply(f_files, function(x) read.table(x, header = T)[,1:2])
g <- lapply(g_files, function(x) read.table(x, header = T)[,1:2])
# remove NA rows
removenas <- function(x){
  for(i in 1:length(x)){
    x[[i]] <- x[[i]][complete.cases(x[[i]]),]
  }
  return(x)
}

a <- removenas(a)
e <- removenas(e)
f <- removenas(f)
g <- removenas(g)

distfromstart <- function(df){
  df$distfromstart <- NA
  df$frame <- row.names(df)
  for(i in 1:nrow(df)){
    df$distfromstart[i] <- pythag(df$X1[i]-df$X1[1], df$Y1[i]-df$Y1[1])
  }
  return(df)
}

a <- lapply(a, distfromstart)
e <- lapply(e, distfromstart)
f <- lapply(f, distfromstart)
g <- lapply(g, distfromstart)

onlyspeed <- function(df){
  lowest <- which(df$distfromstart > 10)[1]
  df <- df[lowest:nrow(df),]
  return(df)
}

a <- lapply(a, onlyspeed)
e <- lapply(e, onlyspeed)
f <- lapply(f, onlyspeed)
g <- lapply(g, onlyspeed)

data <- data.frame(time = c(0.5, 1.0),
                   xdist = rep(NA, 2),
                   ydist = rep(NA, 2),
                   diagdist = rep(NA, 2))

datalist_a <- list(data, data, data, data, data, data, data, data, data)
datalist_e <- list(data, data, data, data, data, data, data, data, data)
datalist_f <- list(data, data, data, data, data, data)
datalist_g <- list(data, data, data, data, data)

for(i in 1:length(a)){
  df <- a[[i]]
  refx <- df$X1[1]
  refy <- df$Y1[1]
  x05 <- df$X1[30]-refx
  y05 <- df$Y1[30]-refy
  x1 <- df$X1[60]-refx
  y1 <- df$Y1[60]-refy
  diag05 <- pythag(x05, y05)
  diag1 <- pythag(x1, y1)
  datalist_a[[i]][1,2:4] <- c(x05, y05, diag05)
  datalist_a[[i]][2,2:4] <- c(x1, y1, diag1)
}
a_data <- bind_rows(datalist_a, .id = "trial")

for(i in 1:length(e)){
  df <- e[[i]]
  refx <- df$X1[1]
  refy <- df$Y1[1]
  x05 <- df$X1[30]-refx
  y05 <- df$Y1[30]-refy
  x1 <- df$X1[60]-refx
  y1 <- df$Y1[60]-refy
  diag05 <- pythag(x05, y05)
  diag1 <- pythag(x1, y1)
  datalist_e[[i]][1,2:4] <- c(x05, y05, diag05)
  datalist_e[[i]][2,2:4] <- c(x1, y1, diag1)
}
e_data <- bind_rows(datalist_e, .id = "trial")

for(i in 1:length(f)){
  df <- f[[i]]
  refx <- df$X1[1]
  refy <- df$Y1[1]
  x05 <- df$X1[30]-refx
  y05 <- df$Y1[30]-refy
  x1 <- df$X1[60]-refx
  y1 <- df$Y1[60]-refy
  diag05 <- pythag(x05, y05)
  diag1 <- pythag(x1, y1)
  datalist_f[[i]][1,2:4] <- c(x05, y05, diag05)
  datalist_f[[i]][2,2:4] <- c(x1, y1, diag1)
}
f_data <- bind_rows(datalist_f, .id = "trial")

for(i in 1:length(g)){
  df <- g[[i]]
  refx <- df$X1[1]
  refy <- df$Y1[1]
  x05 <- df$X1[30]-refx
  y05 <- df$Y1[30]-refy
  x1 <- df$X1[60]-refx
  y1 <- df$Y1[60]-refy
  diag05 <- pythag(x05, y05)
  diag1 <- pythag(x1, y1)
  datalist_g[[i]][1,2:4] <- c(x05, y05, diag05)
  datalist_g[[i]][2,2:4] <- c(x1, y1, diag1)
}
g_data <- bind_rows(datalist_g, .id = "trial")

# combine all four
all_data <- list("a" = a_data, "e" = e_data, "f" = f_data, "g" = g_data)

alldata_df <- bind_rows(all_data, .id = "tadpole") %>% mutate(trial = as.numeric(trial))

alldata_df %>% filter(time == 0.5) %>% 
  ggplot(aes(x = trial, y = diagdist, col = tadpole))+
  geom_point() +
  stat_smooth(method = "lm", se = T)+
  ggtitle("0.5s")

alldata_df %>% filter(time == 1) %>% 
  ggplot(aes(x = trial, y = diagdist, col = tadpole))+
  geom_point() +
  stat_smooth(method = "lm", se = T)+
  ggtitle("1s")

a05 <- lm(diagdist~trial, data = alldata_df %>% filter(tadpole == "a", time == 0.5))
e05 <- lm(diagdist~trial, data = alldata_df %>% filter(tadpole == "e", time == 0.5))
f05 <- lm(diagdist~trial, data = alldata_df %>% filter(tadpole == "f", time == 0.5))
g05 <- lm(diagdist~trial, data = alldata_df %>% filter(tadpole == "g", time == 0.5))
models <- list(a05, e05, f05, g05)
lapply(models, summary)



# Test function to calculate velocity, moving window, variable smoothing parameter
test <- a[[1]]

velwindow <- function(df, smoothing_parameter = 1, xcol = "X1", ycol = "Y1"){
  par <- smoothing_parameter
  start <- par+1
  dists <- rep(NA, nrow(df)-start)
  for(i in start:nrow(df)){
    dists[i] <- diag(df[i-par, xcol], df[i, xcol], df[i-par, ycol], df[i, ycol])
  }
  vels <- dists/par
  return(vels)
}

test$vel1 <- velwindow(test)
test$vel2 <- velwindow(test, 2)
test$vel3 <- velwindow(test, 3)
test$vel4 <- velwindow(test, 4)
test$vel5 <- velwindow(test, 5)
test$vel6 <- velwindow(test, 6)
test$vel7 <- velwindow(test, 7)
test$vel8 <- velwindow(test, 8)
test$vel9 <- velwindow(test, 9)
test$vel10 <- velwindow(test, 10)
test$vel11 <- velwindow(test, 11)
test$vel12 <- velwindow(test, 12)
test$vel13 <- velwindow(test, 13)
test$vel14 <- velwindow(test, 14)
test$vel15 <- velwindow(test, 15)
test$vel16 <- velwindow(test, 16)
test$vel17 <- velwindow(test, 17)
test$vel18 <- velwindow(test, 18)
test$vel19 <- velwindow(test, 19)
test$vel20 <- velwindow(test, 20)