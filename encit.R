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

packages<-c('vmd','dplyr','tidyverse','magrittr', 'caret', 'reshape2', 'gghighlight',
            'TTR', 'forecast', 'Metrics', 'e1071', "cowplot", "elmNNRcpp", 
            "tcltk", "foreach", "iterators","doParallel","lmtest","wmtsa","magrittr")

sapply(packages,packs)

rm(packages)

library(extrafont)
# font_import(pattern = 'CM')
library(ggplot2)
library(Cairo)

# Data treatment ----
# set working directory
setwd(DataDir) 

# load data
raw_data <- read.csv('dataset.csv')

# format date column
raw_data$PCTimeStamp <- raw_data$PCTimeStamp %>% as.Date()

# set a list of dates to analyze
dates <- paste0('2017-08-',c(23:27))

# create empty lists
wind_data <- list()
vmd_results <- list()
single_results <- list()

# list of models
model_list <- c(
  'knn',
  'svmRadial',
  'cubist',
  'brnn',
  'qrf'
) %>% sort

for (date in seq(length(dates)-1)) {
  # filtering the data according to date list
  wind_data[[date]] <- raw_data %>% 
    filter(PCTimeStamp >= dates[date] & PCTimeStamp < dates[date+1])
  
  # training using vmd
  vmd_results[[date]] <- vmd_pred(wind_data[[date]], model_list)

  # training using single models
  single_results[[date]] <- single_pred(wind_data[[date]], model_list)
}

# Save results ----
# set working directory
setwd(ResultsDir)

# loop to save RDS
for (dataset in seq(vmd_results)) {
  saveRDS(
    object = vmd_results[[dataset]],
    file = paste0('results_',dates[dataset],'_vmd.rds')
  )
  
  saveRDS(
    object = single_results[[dataset]],
    file = paste0('results_',dates[dataset],'_single.rds')
  )
}


# loop to save metrics results
FH <- c('Three-steps','Six-steps','Twelve-steps') # aux to create forecasting horizon column

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


# Plot ----
setwd(ResultsDir)

## load data
file_list <- list.files(pattern = '.rds') # list the .rds files

vmd_results <- list()
single_results <- list()

count_single <- 1 # single models aux counter
count_vmd <- 1 # vmd models aux counter 

# loop to read data
for (dataset in seq(file_list)) {
  if (dataset%%2 != 0) {
    single_results[[count_single]] <- readRDS(file = file_list[dataset])
    count_single <- count_single + 1
  } else {
    vmd_results[[count_vmd]] <- readRDS(file = file_list[dataset])
    count_vmd <- count_vmd + 1
  }
}

## Plot Predict x Observed
setwd(FiguresDir)
datasets <- list()

for (ii in seq(4)) {
  datasets[[ii]] <- data.frame(
    'Observed'       = vmd_results[[ii]]$Predictions$`three-steps`[,'Obs'],
    'TSA.VMD.CUBIST' = vmd_results[[ii]]$Predictions$`three-steps`[,'cubist'],
    'Observed'       = vmd_results[[ii]]$Predictions$`three-steps`[,'Obs'],
    'SSA.VMD.CUBIST' = vmd_results[[ii]]$Predictions$`six-steps`[,'cubist'],
    'Observed'       = vmd_results[[ii]]$Predictions$`three-steps`[,'Obs'],
    'TWA.VMD.CUBIST' = vmd_results[[ii]]$Predictions$`twelve-steps`[,'cubist']
  ) %>% melt() %>% data.frame(
    .,
    rep(c('Observed','Predicted'), each = nrow(vmd_results[[ii]]$Predictions$`three-steps`)),
    rep(c("Three-steps","Six-steps","Twelve-steps"), each= 2*nrow(vmd_results[[ii]]$Predictions$`three-steps`))
  )
  
  datasets[[ii]]$variable <- NULL
  colnames(datasets[[ii]]) <- c('value', 'type', 'FH')
}

count <- 1

for (dataset in datasets) {
  n <- table(dataset$FH)[1]/2
  
  dataset$FH <- dataset$FH %>% factor(levels = c("Three-steps","Six-steps","Twelve-steps"))
  
  plot <- dataset %>% as.data.frame %>% 
    ggplot(aes(x = rep(seq(n),6), y = value, colour = type)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = 'bottom',
          legend.background = element_blank(),
          legend.text = element_text(size = 20),
          text = element_text(family = "CM Roman", size = 20),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          ) +
    ylab('Wind Power (KW)') + xlab('Samples (10 minutes)') +
    facet_grid(rows = vars(FH)) +
    scale_x_continuous(breaks = seq(0,n,35), limits = c(0,n)) +
    scale_y_continuous(breaks = c(1000, 1500, 2000)) +
    scale_color_manual(values = c("#377EB8","#E41A1C")) +
    geom_vline(xintercept = round(n*0.8), color = 'black', size = 0.5) +
    annotate(geom = 'text', x = min(seq(n)), y = min(dataset$value), hjust = .2, vjust = -.4, 
             label = 'Training', color = 'black', family = 'CM Roman', size = 6) +
    annotate(geom = 'text', x = round(n*0.85), min(dataset$value), hjust = .2, vjust = -.4,
             label = 'Test', color = 'black', family = 'CM Roman', size = 6)
    
  plot %>% 
    ggsave(
      filename = paste0('PO_dataset_',dates[count],'.pdf'),
      device = 'pdf',
      width = 12,
      height = 6.75,
      units = "in",
      dpi = 300
    )   
  
  count <- count + 1
}

## Plot IMFs
setwd(FiguresDir)

IMFs <- data.frame()

for (dataset in seq(length(vmd_results))) {
  vmd_results[[dataset]]$IMF$Dataset <- rep(dates[dataset])
  vmd_results[[dataset]]$IMF$n <- seq(nrow(vmd_results[[dataset]]$IMF))
  IMFs <- rbind(IMFs,vmd_results[[dataset]]$IMF %>% melt(id.vars = c('Dataset','n')))
}

IMFs <- IMFs %>% 
  filter(Dataset != '2017-08-25')

IMFs$Dataset <- IMFs$Dataset %>% 
  factor(levels = paste0('2017-08-',c(23,24,26)),
         labels = paste0('Dataset~', seq(3)))

imf_labels <- 
  c(
    expression(paste(IMF[1])),
    expression(paste(IMF[2])),
    expression(paste(IMF[3])),
    expression(paste(IMF[4])),
    expression(paste(IMF[5]))
  )

IMFs$variable <- IMFs$variable %>% 
  factor(
    levels = c('Obs', paste0('IMF',seq(5))),
    labels = c('Obs',imf_labels)
  )

imf_plot <- IMFs %>% 
  filter(variable != 'Obs') %>%
  ggplot(aes(x = n, y = value, colour = variable)) +
  geom_line(size = 1, colour='#377EB8') +
  theme_bw() +
  theme(
    text = element_text(family = "CM Roman", size = 16),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  ylab('') + xlab('Samples(10 minutes)') +
  facet_grid(
    variable ~ Dataset,
    scales = 'free',
    switch = 'y',
    labeller = "label_parsed",
  ) +
  scale_x_continuous(breaks = seq(0,max(IMFs$n),35)) +
  scale_y_continuous(breaks = c(-200,0,200,1200,1600,2000))

imf_plot

imf_plot %>% 
  ggsave(
    filename = 'imf_plot.pdf',
    device = 'pdf',
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 300
  ) 

## Plot datasets
setwd(FiguresDir)

wind_data[[3]] <- NULL

obs_dataset <- data.frame(
  'n' = seq(144),
  'type' = c(rep('Training', times = 101), rep('Test', times = 43))
)

for (dataset in seq(length(wind_data))) {
  obs_dataset <- cbind(obs_dataset, wind_data[[dataset]][,'Power'])
}

colnames(obs_dataset) <- c('n', 'type', paste('Dataset', seq(3)))

obs_dataset <- obs_dataset %>% melt(id.vars = c('n','type'))

dataplot <- obs_dataset %>% 
  ggplot(aes(x = n, y = value)) +
  geom_line(size = 1, colour='#377EB8') +
  facet_grid(vars(variable), scales = 'free', switch = 'y') +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.background = element_blank(),
        legend.text = element_text(size = 20),
        text = element_text(family = "CM Roman", size = 20),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 20),
  ) +
  ylab('') + xlab('Samples (10 minutes)') +
  scale_x_continuous(breaks = seq(0,max(obs_dataset$n),35), limits = c(0,max(obs_dataset$n))) +
  scale_y_continuous(breaks = scales::pretty_breaks(4))

dataplot %>% 
  ggsave(
    filename = 'datasets_plot.pdf',
    device = 'pdf',
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 300
  ) 

## Summary table

wind_data[[3]] <- NULL

summaries_table <- data.frame(
  'Variable' = rep(names(wind_data[[1]])[-1], times = 3),
  'Samples' = rep(c('Whole', 'Training', 'Test'), each = ncol(wind_data[[1]][-1]))
)

for (dataset in seq(length(wind_data))) {
  #Descriptives
  n <- nrow(wind_data[[dataset]])
  cut <- round(0.7*n)
  
  #Whole
  Whole <- t(apply(wind_data[[dataset]][,-1],2,function(x){c(mean(x),sd(x),min(x),max(x))}))
  colnames(Whole) <- paste0(c('Mean.', 'Std.', 'Min.', 'Max.'), dataset)
  #Train Descriptives
  Train <- t(apply(wind_data[[dataset]][1:cut,-1],2,function(x){c(mean(x),sd(x),min(x),max(x))}))
  colnames(Train) <- names(Whole)
  #Test Descriptives
  Test <- t(apply(tail(wind_data[[dataset]][,-1],n-cut),2,function(x){c(mean(x),sd(x),min(x),max(x))}))
  colnames(Test) <- names(Whole)
  
  #Merge
  summaries_table <- cbind(summaries_table, rbind(Whole, Train, Test))
  row.names(summaries_table) <- NULL # reset row index
}

# Reorder rows
summaries_table <- summaries_table %>% 
  arrange(factor(Variable, levels = names(wind_data[[1]][-1])))

print(xtable::xtable(summaries_table, digits = 2), include.rownames=FALSE)












