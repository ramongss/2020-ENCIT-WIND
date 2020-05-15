single_pred <- function(data, model_list){
  
  # Data Preprocessing ----
  # number of lags
  lag <- 2
  
  # create lags and inputs
  data_lag <- data.frame(
    lags(data[,-1], lag)
  )
  
  # keep and reorder the columns
  data_lag <- data_lag[,c(1,9,17,10:16)]
  
  # rename columns
  colnames(data_lag) <- c('y',paste0('lag', seq(2)), colnames(data)[3:9])
  
  # create train and test sets
  n <- nrow(data_lag)
  cut <- n - 6
  
  train <- data_lag[1:cut,]
  test <- tail(data_lag, n-cut)
  
  x_train <- train[,-1]
  y_train <- train[,1]
  
  x_test <- test[,-1]
  y_test <- test[,1]
  
  # create Obs
  Obs <- data_lag$y
  Obs_train <- Obs[1:cut]
  Obs_test  <- tail(Obs,n-cut)
  
  # Training phase ----
  # set random seed
  set.seed(1234)
  
  # set trainControl
  control <- trainControl(
    method = "timeslice",
    initialWindow = 0.8*nrow(train),
    horizon = 0.1*nrow(train),
    fixedWindow = FALSE,
    allowParallel = TRUE,
    savePredictions = 'final',
    verboseIter = FALSE
  )
  
  # list of training models
  model_list <- model_list
  
  # define objects
  models <- list()
  Params <- list()
  Importance <- matrix(nrow = ncol(x_train), ncol = length(model_list))
  colnames(Importance) <- model_list ; rownames(Importance) <- colnames(x_train)
  
  # training each model
  for (model in seq(model_list)) {
    # fitting
    models[[model]] <- train(
      y~., data = train,
      method = model_list[model],
      trControl = control,
      preProcess = c('center','scale'),
      tuneLength = 5,
      importance = TRUE,
      trace = FALSE
    )
    
    # save hyperparameters
    Params[[model]] <- models[[model]]$bestTune
    
    # save variables importance
    Importance[,model] <- varImp(models[[model]], scale = FALSE)$importance$Overall
    
    # print steps
    cat("\nModel: ", model_list[model], "\t",
        as.character(format.Date(Sys.time(), '%H:%M:%S')),
        sep = '')
  }
  
  # Multi-step predictions ----
  
  ## Recursive prediction
  
  # define objects
  PTRmo <- list()
  PTEmo <- list()
  single_step_pred <- list()
  metrics_train <- list()
  metrics_test <- list()
  errors <- list()
  horizon <- c(1,3,6)
  
  for (h in seq(horizon)) {
    hrz <- horizon[h]
    PTRmo[[h]] <- matrix(ncol = length(model_list), nrow = nrow(train))
    PTEmo[[h]] <- matrix(ncol = length(model_list), nrow = nrow(test))
    metrics_train[[h]] <- matrix(nrow = length(model_list), ncol = 4)
    metrics_test[[h]] <- matrix(nrow = length(model_list), ncol = 4)
    colnames(metrics_train[[h]]) <- c("i","sMAPE","RRMSE","R2")
    colnames(metrics_test[[h]]) <- colnames(metrics_train[[h]])
    rownames(metrics_train[[h]]) <- model_list
    rownames(metrics_test[[h]]) <- rownames(metrics_train[[h]])
    
    single_step_pred[[h]] <- matrix(nrow = n, ncol = length(model_list))
    colnames(single_step_pred[[h]]) <- model_list
    
    cat('\nHorizon: ', hrz, '\n')
    
    for (m in seq(model_list)) {
      x_trainm <- as.data.frame(x_train)
      x_testm <- as.data.frame(x_test)
      
      if (h == 1) {
        # train
        PTRmo[[h]][,m] <- (predict(models[[m]], x_trainm))
        
        # test
        PTEmo[[h]][,m] <- (predict(models[[m]], x_testm))
      } else {
        # train
        for(p in seq(cut)) {
          if(p%%hrz != 1) {
            non_zero <- (predict(models[[m]], x_trainm[p,]))
            if(non_zero < 0){non_zero <- 0}
            PTRmo[[h]][p,m] <- non_zero
            x_trainm[p+1,1] <- PTRmo[[h]][p,m]
            x_trainm[p+2,2] <- PTRmo[[h]][p,m]
          } else {
            x_trainm[p:cut,] <- x_train[p:cut,]
            non_zero <- (predict(models[[m]], x_trainm[p,]))
            if(non_zero < 0){non_zero <- 0}
            PTRmo[[h]][p,m] <- non_zero
            x_trainm[p+1,1] <- PTRmo[[h]][p,m]
            x_trainm[p+2,2] <- PTRmo[[h]][p,m]
          }
        }
        
        # test
        for(p in seq(n-cut)) {
          if(p%%hrz !=1) {
            non_zero <- (predict(models[[m]], x_testm[p,]))
            if(non_zero < 0){non_zero <- 0}
            PTEmo[[h]][p,m] <- non_zero
            x_testm[p+1,1] <- PTEmo[[h]][p,m]
            x_testm[p+2,2] <- PTEmo[[h]][p,m]
          } else {
            x_testm[p:(n-cut),] <- x_test[p:(n-cut),]
            non_zero <- (predict(models[[m]], x_testm[p,]))
            if(non_zero < 0){non_zero <- 0}
            PTEmo[[h]][p,m] <- non_zero
            x_testm[p+1,1] <- PTEmo[[h]][p,m]
            x_testm[p+2,2] <- PTEmo[[h]][p,m]
          }
        }
      }
      
      # avoiding negative values
      for (j in seq(nrow(PTRmo[[h]]))) {
        if (PTRmo[[h]][j,m] < 0) {
          PTRmo[[h]][j,m] <- 0
        }
      }
      for (j in seq(nrow(PTEmo[[h]]))) {
        if (PTEmo[[h]][j,m] < 0) {
          PTEmo[[h]][j,m] <- 0
        }
      }
      
      single_step_pred[[h]][,m] <- c(PTRmo[[h]][,m], PTEmo[[h]][,m])
      
      # metrics
      step_smape_train <- smape(PTRmo[[h]][,m], Obs_train)
      step_rrmse_train <- RMSE(PTRmo[[h]][,m], Obs_train)/mean(PTRmo[[h]][,m])
      step_r2_train    <- cor(PTRmo[[h]][,m], Obs_train)^2
      
      step_smape_test <- smape(PTEmo[[h]][,m], Obs_test)
      step_rrmse_test <- RMSE(PTEmo[[h]][,m], Obs_test)/mean(PTEmo[[h]][,m])
      step_r2_test    <- cor(PTEmo[[h]][,m], Obs_test)^2
      
      metrics_train[[h]][m,] <- c(m,
                                  step_smape_train,
                                  step_rrmse_train,
                                  step_r2_train)
      metrics_test[[h]][m,] <- c(m,
                                 step_smape_test,
                                 step_rrmse_test,
                                 step_r2_test)
      
      
      cat("Model: ", model_list[m], "\t", 
          (m/(length(model_list)))*100,"%\n", sep = "")
    }
    
    # add Obs column
    single_step_pred[[h]] <- cbind(Obs, single_step_pred[[h]]) 
    colnames(single_step_pred[[h]]) <- c('Obs',model_list)
    
    # calculte errors
    errors[[h]] <- matrix(ncol = length(model_list), nrow = n)
    colnames(errors[[h]]) <- model_list
    for (error in seq(ncol(errors[[h]]))) {
      errors[[h]][,error] <- (single_step_pred[[h]][,1] - single_step_pred[[h]][,error+1])
    }
  }
  
  names(single_step_pred) <- c('one-step','three-steps','six-steps')
  names(metrics_train) <- names(single_step_pred)
  names(metrics_test) <- names(single_step_pred)
  names(errors) <- names(single_step_pred)
  
  single_results <- list(Predictions = single_step_pred,
                         Metrics = metrics_test,
                         Hyperparameters = Params,
                         Var_Importance = Importance,
                         Errors = errors)
  
  return(single_results)
}