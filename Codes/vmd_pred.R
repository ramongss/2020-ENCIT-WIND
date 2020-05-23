vmd_pred <- function(data, model_list){
  cat('\n\n######### VMD Prediction #########\n###########',
      as.character(format.Date(data$PCTimeStamp[1])),'###########\n\n')
  # Data preprocessing ----
  # vmd decomposition
  vmd_decomp <- vmd(
    signal = data$Power,
    tol = 1e-6,
    DC = FALSE,
    K = 5
  ) %>% data.frame()
  
  # create dataframes
  lag <- 2 # number of lags
  
  IMF <- list() # list to save dfs
  
  # create df for each imf with lags and inputs
  for (ii in seq(5)) {
    IMF[[ii]] <- data.frame(
      lags(vmd_decomp[,ii+2], lag),
      data[,3][(lag-0):(dim(data)[1]-lag+1)],
      data[,4][(lag-0):(dim(data)[1]-lag+1)],
      data[,5][(lag-0):(dim(data)[1]-lag+1)],
      data[,6][(lag-0):(dim(data)[1]-lag+1)],
      data[,7][(lag-0):(dim(data)[1]-lag+1)],
      data[,8][(lag-0):(dim(data)[1]-lag+1)],
      data[,9][(lag-0):(dim(data)[1]-lag+1)]
    )
    
    # rename columns
    names(IMF[[ii]]) <- c(
      'y',
      paste0('lag',seq(lag)),
      names(data[,c(3:ncol(data))])
    )
  }
  
  # rename the list
  names(IMF) <- paste0('IMF', seq(5))
  
  # create training and test sets
  IMF_train  <- list()
  IMF_test   <- list()
  IMF_xtrain <- list()
  IMF_ytrain <- list()
  IMF_xtest  <- list()
  IMF_ytest  <- list()
  
  for (i in seq(IMF)) {
    n <- dim(IMF[[i]])[1]
    cut <- round(n * 0.8)
    
    IMF_train[[i]] <- IMF[[i]][1:cut,]
    IMF_test[[i]]  <- tail(IMF[[i]],n-cut)
    
    IMF_xtrain[[i]] <- IMF_train[[i]][,-1]
    IMF_ytrain[[i]] <- IMF_train[[i]][,1]
    
    IMF_xtest[[i]] <- IMF_test[[i]][,-1]
    IMF_ytest[[i]] <- IMF_test[[i]][,1]
  }
  
  # create Observed dfs
  Obs   <- data$Power[(lag+1):(dim(data)[1]-lag+2)]
  Obs_train <- Obs[1:cut]
  Obs_test  <- tail(Obs,n-cut)
  
  IMF_df <- data.frame(vmd_decomp[,c(2:7)])
  colnames(IMF_df) <- c('Obs', paste0('IMF', seq(5)))
  
  # Training phase ----
  # set random seed
  set.seed(1234)
  
  # set traincontrol
  control <- trainControl(
    method = "timeslice",
    initialWindow = 0.8*dim(IMF_train[[1]])[1],
    horizon = 0.1*dim(IMF_train[[1]])[1],
    fixedWindow = FALSE,
    allowParallel = TRUE,
    savePredictions = 'final',
    verboseIter = FALSE
  )
  
  # list of training models
  model_list <- model_list
  
  # define objects
  IMF_model <- list()
  IMF_pred <- list()
  Params <- list()
  Importance <- list()
  k <- 1 # aux
  
  # training and predicting each IMF with each model
  for (imf in seq(IMF)) {
    Importance[[imf]] <- matrix(nrow = ncol(IMF_xtrain[[imf]]), ncol = length(model_list))
    colnames(Importance[[imf]]) <- model_list
    rownames(Importance[[imf]]) <- colnames(IMF_xtrain[[imf]])
    for (model in seq(model_list)) {
      # fitting
      IMF_model[[k]] <- train(
        y~., data = IMF_train[[imf]],
        method = model_list[model],
        trControl = control,
        preProcess = c('center','scale'),
        tuneLength = 5,
        trace = FALSE
      )
      
      # save hyperparameters
      Params[[k]] <- IMF_model[[k]]$bestTune
      
      # save variables importance
      Importance[[imf]][,model] <- varImp(IMF_model[[k]], 
                                          scale = FALSE)$importance$Overall
      
      # prediction
      IMF_train_pred <- predict(IMF_model[[k]], IMF_train[[imf]])
      IMF_test_pred <- predict(IMF_model[[k]], IMF_test[[imf]])
      IMF_pred[[k]] <- data.frame(c(IMF_train_pred, IMF_test_pred))
      
      # print steps
      cat("\nModel: ", model_list[model], "\tIMF", imf, "\t",
          as.character(format.Date(Sys.time(), '%H:%M:%S')),
          sep = '')
      
      # update k aux
      k <- k + 1
    }
  }
  
  names(Importance) <- paste0('IMF',seq(5))
  
  # create a matrix combination
  combs <- matrix(nrow = length(model_list), ncol = length(IMF))
  colnames(combs) <- names(IMF)

  for (i in seq(model_list)) {
    for (j in seq(IMF)) {
      combs[i,j] <- i
    }
  }
  # # count <- seq(model_list) # aux
  # # combs <- expand.grid(count,count,count,count,count) # create combination
  # # colnames(combs) <- names(IMF) # rename columns
  # 
  # 
  # # define objects
  # vmd_metrics_train <- matrix(nrow = dim(combs)[1], ncol = 4)
  # vmd_metrics_test <- matrix(nrow = dim(combs)[1], ncol = 4)
  # colnames(vmd_metrics_train) <- c("comb","sMAPE","RRMSE","R2")
  # colnames(vmd_metrics_test) <- colnames(vmd_metrics_train)
  # vmd_prediction <- matrix(nrow = n, ncol = dim(combs)[1]) %>% data.frame
  # 
  # # IMF_pred to a matrix of results
  # pred_matrix <- data.frame(matrix(IMF_pred, ncol=5, byrow=FALSE))
  # names(pred_matrix) <- names(IMF)
  # 
  # for (comb in seq(dim(combs)[1])) {
  #   # auxiliar to pull the index from pred_matrix
  #   a <- combs[comb, 'IMF1']
  #   b <- combs[comb, 'IMF2']
  #   c <- combs[comb, 'IMF3']
  #   d <- combs[comb, 'IMF4']
  #   e <- combs[comb, 'IMF5']
  #   
  #   # summing each IMF according to combs
  #   vmd_prediction[,comb] <- (
  #     pred_matrix[[a,'IMF1']] +
  #       pred_matrix[[b,'IMF2']] + 
  #       pred_matrix[[c,'IMF3']] +
  #       pred_matrix[[d,'IMF4']] +
  #       pred_matrix[[e,'IMF5']]
  #   )
  #   
  #   # avoiding negative values
  #   for (line in 1:dim(vmd_prediction)[1]) {
  #     if (vmd_prediction[line,comb] < 0) {
  #       vmd_prediction[line,comb] <- 0
  #     }
  #   }
  #   
  #   # split into train-test
  #   vmd_train <- vmd_prediction[,comb][1:cut]
  #   vmd_test <- tail(vmd_prediction[,comb],n-cut)
  #   
  #   # metrics
  #   smape_train <- smape(vmd_train, Obs_train)
  #   rrmse_train <- RMSE(vmd_train, Obs_train)/mean(vmd_train)
  #   r2_train <- cor(vmd_train, Obs_train)^2
  #   
  #   smape_test <- smape(vmd_test, Obs_test)
  #   rrmse_test <- RMSE(vmd_test, Obs_test)/mean(vmd_test)
  #   r2_test <- cor(vmd_test, Obs_test)^2
  #   
  #   vmd_metrics_train[comb,] <- c(comb, smape_train, rrmse_train, r2_train)
  #   vmd_metrics_test[comb,] <- c(comb, smape_test, rrmse_test, r2_test)
  # }
  # 
  # # smaller sMAPE
  # vmd_best_metric <- vmd_metrics_test[which.min(vmd_metrics_test[,2]),]
  # best_comb <- combs[which.min(vmd_metrics_test[,2]),] # combination
  
  
  # Multi-step predictions ----
  
  # save IMF_model into a matrix
  model_matrix <- data.frame(matrix(IMF_model, ncol=5, byrow = FALSE))
  names(model_matrix) <- names(IMF)
  
  ## Recursive prediction
  
  # define objects
  step_VMD_pred <- list()
  metrics_VMD_train <- list()
  metrics_VMD_test <- list()
  errors <- list()
  horizon <- c(3,6,12)
  for (h in seq(length(horizon))) {
    {
      hrz <- horizon[h]
      PTRmo <- list()
      PTEmo <- list()
      Comp_train <- list()
      Comp_test <- list()
      k <- 1
    }
    
    cat('\nHorizon: ', hrz, 'steps\n')
    
    for (m in 1:length(model_list)) {
      
      IMF1_trainm <- as.data.frame(IMF_xtrain[[1]])
      IMF2_trainm <- as.data.frame(IMF_xtrain[[2]])
      IMF3_trainm <- as.data.frame(IMF_xtrain[[3]])
      IMF4_trainm <- as.data.frame(IMF_xtrain[[4]])
      IMF5_trainm <- as.data.frame(IMF_xtrain[[5]])
      
      Comp_train[[m]] <- list(IMF1_trainm,
                              IMF2_trainm,
                              IMF3_trainm,
                              IMF4_trainm,
                              IMF5_trainm)
      
      IMF1_testm <- as.data.frame(IMF_xtest[[1]])
      IMF2_testm <- as.data.frame(IMF_xtest[[2]])
      IMF3_testm <- as.data.frame(IMF_xtest[[3]])
      IMF4_testm <- as.data.frame(IMF_xtest[[4]])
      IMF5_testm <- as.data.frame(IMF_xtest[[5]])
      
      Comp_test[[m]] <- list(IMF1_testm,
                             IMF2_testm,
                             IMF3_testm,
                             IMF4_testm,
                             IMF5_testm)
      
      PTRmo[[m]] <- matrix(ncol = length(IMF), nrow = dim(IMF1_trainm)[1])
      PTEmo[[m]] <- matrix(ncol = length(IMF), nrow = dim(IMF1_testm)[1])
      
      for (c in 1:length(Comp_train[[m]])) {
        if (h == 1) {
          # train
          PTRmo[[m]][,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]]))
          
          # test
          PTEmo[[m]][,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]]))
        } else {
          # train
          for(p in 1:cut) {
            if(p%%hrz !=1) {
              PTRmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]][p,]))
              Comp_train[[m]][[c]][p+1,1] <- PTRmo[[m]][p,c]
              Comp_train[[m]][[c]][p+2,2] <- PTRmo[[m]][p,c]
            } else {
              Comp_train[[m]][[c]][p:cut,] <- IMF_xtrain[[c]][p:cut,]
              PTRmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]][p,]))
              Comp_train[[m]][[c]][p+1,1] <- PTRmo[[m]][p,c]
              Comp_train[[m]][[c]][p+2,2] <- PTRmo[[m]][p,c]
            }
          }
          
          # test
          for(p in 1:(n-cut)) {
            if(p%%hrz !=1) {
              PTEmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]][p,]))
              Comp_test[[m]][[c]][p+1,1] <- PTEmo[[m]][p,c]
              Comp_test[[m]][[c]][p+2,2] <- PTEmo[[m]][p,c]
            } else {
              Comp_test[[m]][[c]][p:(n-cut),] <- IMF_xtest[[c]][p:(n-cut),]
              PTEmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]][p,]))
              Comp_test[[m]][[c]][p+1,1] <- PTEmo[[m]][p,c]
              Comp_test[[m]][[c]][p+2,2] <- PTEmo[[m]][p,c]
            }
          }
        }
        
        cat("Model: ", model_list[m], "\tIMF: ", c , "\t", 
            (k/(length(model_list)*length(Comp_train[[m]])))*100,"%\n", sep = "")
        
        k <- k + 1
      }
    }
    
    {
      metrics_VMD_train[[h]] <- matrix(nrow = dim(combs)[1],ncol = 4)
      metrics_VMD_test[[h]] <- matrix(nrow = dim(combs)[1],ncol = 4)
      colnames(metrics_VMD_train[[h]]) <- c("i","sMAPE","RRMSE","R2")
      colnames(metrics_VMD_test[[h]]) <- colnames(metrics_VMD_train[[h]])
      rownames(metrics_VMD_train[[h]]) <- model_list
      rownames(metrics_VMD_test[[h]]) <- rownames(metrics_VMD_train[[h]])
      
      
      step_VMD_pred_train <- matrix(nrow = dim(IMF_xtrain[[1]])[1], ncol = dim(combs)[1])
      step_VMD_pred_test <- matrix(nrow = dim(IMF_xtest[[1]])[1], ncol = dim(combs)[1])
      step_VMD_pred[[h]] <- matrix(nrow = n, ncol = dim(combs)[1])
    }
    
    for (i in 1:dim(combs)[1]) {
      step_VMD_pred_train[,i] <- rowSums(PTRmo[[i]])
      step_VMD_pred_test[,i] <- rowSums(PTEmo[[i]])
      
      ### Avoiding negative values
      for (j in 1:dim(step_VMD_pred_train)[1]) {
        if (step_VMD_pred_train[j,i] < 0) {
          step_VMD_pred_train[j,i] <- 0
        }
      }
      for (j in 1:dim(step_VMD_pred_test)[1]) {
        if (step_VMD_pred_test[j,i] < 0) {
          step_VMD_pred_test[j,i] <- 0
        }
      }
      
      step_VMD_pred[[h]][,i] <- c(step_VMD_pred_train[,i],
                                  step_VMD_pred_test[,i])
      
      
      # metrics
      
      vmd_step_smape_train <- smape(step_VMD_pred_train[,i], Obs_train)
      vmd_step_rrmse_train <- RMSE(step_VMD_pred_train[,i], Obs_train)/mean(step_VMD_pred_train[,i])
      vmd_step_r2_train    <- cor(step_VMD_pred_train[,i], Obs_train)^2
      
      vmd_step_smape_test <- smape(step_VMD_pred_test[,i], Obs_test)
      vmd_step_rrmse_test <- RMSE(step_VMD_pred_test[,i], Obs_test)/mean(step_VMD_pred_test[,i])
      vmd_step_r2_test    <- cor(step_VMD_pred_test[,i], Obs_test)^2
      
      metrics_VMD_train[[h]][i,] <- c(i,
                                      vmd_step_smape_train,
                                      vmd_step_rrmse_train,
                                      vmd_step_r2_train)
      metrics_VMD_test[[h]][i,] <- c(i,
                                     vmd_step_smape_test,
                                     vmd_step_rrmse_test,
                                     vmd_step_r2_test)
    }
    step_VMD_pred[[h]] <- cbind(Obs, step_VMD_pred[[h]]) 
    colnames(step_VMD_pred[[h]]) <- c('Obs',model_list)
    
    errors[[h]] <- matrix(ncol = length(model_list), nrow = n)
    colnames(errors[[h]]) <- model_list
    
    for (error in seq(dim(errors[[h]])[2])) {
      errors[[h]][,error] <- (step_VMD_pred[[h]][,1] - step_VMD_pred[[h]][,error+1])
    }
    
  }
  names(step_VMD_pred) <- c('three-steps','six-steps','twelve-steps')
  names(metrics_VMD_train) <- names(step_VMD_pred)
  names(metrics_VMD_test) <- names(step_VMD_pred)
  names(errors) <- names(step_VMD_pred)
  
  vmd_results <- list(Predictions = step_VMD_pred,
                      Metrics = metrics_VMD_test,
                      Hyperparameters = Params,
                      Var_Importance = Importance,
                      Errors = errors,
                      IMF = IMF_df)
  
  return(vmd_results)
}





















