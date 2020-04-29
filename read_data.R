library(tidyverse)
library(data.table)
library(dplyr)
library(tidyr)

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rbindlist_fread <- function(path, pattern = "*.csv") {
  files = list.files(path, pattern, full.names = TRUE)
  rbindlist(lapply(files, function(x) read_plus(x)))
}
path<-"C:/Users/adamj/Desktop/S&P/data"
all<-rbindlist_fread(path)

all$filename<-substr(all$filename, 33, nchar(all$filename)-4)
unique(all$filename)

all_fresh<-all[all$Data>'2020-01-01']

all_fresh_cov<-as.data.frame(cbind(all_fresh$Zamkniecie[all_fresh$filename==unique(all$filename)[1]],
                     all_fresh$Zamkniecie[all_fresh$filename==unique(all$filename)[2]]))

names(all_fresh_cov)<-unique(all$filename)[1:2]

for(i in 3:length(unique(all$filename))){
  
  if(length(all_fresh$Zamkniecie[all_fresh$filename==unique(all$filename)[i]])==79){
    all_fresh_cov[,paste0(unique(all$filename)[i])]<-all_fresh$Zamkniecie[all_fresh$filename==unique(all$filename)[i]]
  }

}

corr<-cor(all_fresh_cov)
heatmap(corr, symm = TRUE, col = terrain.colors(256))



