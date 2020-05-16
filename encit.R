# Set environment ----
rm(list = ls())
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep="/")
FiguresDir    <- paste(BaseDir, "Figures", sep="/")
ResultsDir    <- paste(BaseDir, "Results", sep="/")
DataDir       <- paste(BaseDir, "Data",sep="/")

# load Packages
setwd(CodesDir)
source("checkpackages.R")
source("vmd_pred.R")
source("single_pred.R")

packages<-c('vmd','dplyr','tidyverse','magrittr', 'caret', 'reshape2',
            'TTR', 'forecast', 'Metrics', 'e1071', "cowplot", "elmNNRcpp", 
            "tcltk", "foreach", "iterators","doParallel","lmtest","wmtsa","magrittr")

sapply(packages,packs)

rm(packages)

library(extrafont)
# windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)

# Data treatment ----
setwd(DataDir)

raw_data <- read.csv('dataset.csv')

raw_data$PCTimeStamp <- raw_data$PCTimeStamp %>% as.Date()

dates <- paste0('2017-08-',c(22:26))

wind_data <- list()
vmd_results <- list()
single_results <- list()

model_list <- c(
  'knn',
  'svmRadial',
  'cubist',
  'brnn',
  'qrf'
)

for (date in seq(length(dates)-1)) {
  wind_data[[date]] <- raw_data %>% 
    filter(PCTimeStamp >= dates[date] & PCTimeStamp < dates[date+1])
  
  vmd_results[[date]] <- vmd_pred(wind_data[[date]], model_list)
  single_results[[date]] <- single_pred(wind_data[[date]], model_list)
}

setwd(ResultsDir)
FH <- c('ODA','TDA','SDA')
for (dataset in seq(vmd_results)) {
  filename_vmd <- paste0('dataset_',dates[dataset],'_vmd_metrics.csv') # vmd file name
  filename_single <- paste0('dataset_',dates[dataset],'_single_metrics.csv') # single file name
  
  file.create(filename_vmd) # create file vmd
  file.create(filename_single) # create file single
  
  # append header in csv files
  data.frame('model','FH','sMAPE','RRMSE','R2') %>%
    write.table(file = filename_vmd,
                append = TRUE,
                sep = ',',
                col.names = FALSE,
                row.names = FALSE)
  data.frame('model','FH','sMAPE','RRMSE','R2') %>%
    write.table(file = filename_single,
                append = TRUE,
                sep = ',',
                col.names = FALSE,
                row.names = FALSE)

  for (metric in seq(vmd_results[[dataset]]$Metrics)) {
    # save vmd metrics in csv
    data.frame(
      FH = rep(FH[metric]),
      vmd_results[[dataset]]$Metrics[[metric]][,-1] %>% 
        .[order(as.character(rownames(.))),,drop=FALSE]
    ) %>%
      write.table(file = filename_vmd,
                  append = TRUE,
                  sep = ',',
                  col.names = FALSE,
                  row.names = TRUE)
    
    # save single metrics in csv
    data.frame(
      FH = rep(FH[metric]),
      single_results[[dataset]]$Metrics[[metric]][,-1] %>% 
        .[order(as.character(rownames(.))),,drop=FALSE]
    ) %>%
      write.table(file = filename_single,
                  append = TRUE,
                  sep = ',',
                  col.names = FALSE,
                  row.names = TRUE)
  }
}
