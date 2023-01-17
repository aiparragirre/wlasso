
f.table <- function(res, obj.name, method.ind, method.names){
  
  df <- matrix(NA, nrow = length(method.ind) + 1, ncol = 3*4)
  colnames(df) <- c("error_min", "error_max", "error_mean(sd)", "error_median(Q1-Q3)",
                    "loglambda_min", "loglambda_max", "loglambda_mean(sd)", "loglambda_median(Q1-Q3)",
                    "nvar_min", "nvar_max", "nvar_mean(sd)", "nvar_median(Q1-Q3)")
  df <- as.data.frame(df)
  rownames(df) <- c("True (pop)", method.names)
  
  # Error
  # ----------------------------------------------------------------------------
  
  # Data
  l.error <- list()
  for(m in method.ind){
    lambda.id <- l.error[[paste0("method_",m)]] <- rep(NA, length(res[["lambda.grid"]]))
    for(r in 1:length(res[["lambda.grid"]])){
      lambda.id[r] <- which(res[["lambda.grid"]][[paste0("sample_",r)]] %in% res[[paste0("method_",m)]][["lambda.min"]][r])
      l.error[[paste0("method_",m)]][r] <- res[["population.error"]][[paste0("sample_",r)]][lambda.id[r]]
    }
  }
  names(l.error) <- method.names
  l.error[["True (pop)"]] <- rep(NA, length(res[["lambda.grid"]]))
  for(r in 1:length(res[["lambda.grid"]])){
    l.error[["True (pop)"]][r] <- min(res[["population.error"]][[paste0("sample_",r)]])
  }
  
  # Table
  df[1,1:4] <- c(round(min(l.error[["True (pop)"]]), digits = 4), 
                 round(max(l.error[["True (pop)"]]), digits = 4), 
                 paste0(round(mean(l.error[["True (pop)"]]), digits = 4), " (", round(sd(l.error[["True (pop)"]]), digits = 4),")"),
                 paste0(round(quantile(l.error[["True (pop)"]], prob=0.5), digits = 4), " (", round(quantile(l.error[["True (pop)"]], prob = 0.25), digits = 4),", ", round(quantile(l.error[["True (pop)"]], prob = 0.75), digits = 4),")"))
  
  for(m in 1:length(method.ind)){
    df[m+1, 1:4] <- c(round(min(l.error[[m]]), digits = 4), 
                    round(max(l.error[[m]]), digits = 4), 
                    paste0(round(mean(l.error[[m]]), digits = 4), " (", round(sd(l.error[[m]]), digits = 4),")"),
                    paste0(round(quantile(l.error[[m]], prob=0.5), digits = 4), " (", round(quantile(l.error[[m]], prob = 0.25), digits = 4),", ", round(quantile(l.error[[m]], prob = 0.75), digits = 4),")"))
  }
  
  # log(lambda)
  # ----------------------------------------------------------------------------
  
  # Data
  l.loglambda.bias <- list()
  for(m in method.ind){
    l.loglambda.bias[[paste0("method_",m)]] <- log(res[[paste0("method_",m)]][["lambda.min"]])
  }
  names(l.loglambda.bias) <- method.names
  l.loglambda.bias[["True (pop)"]] <- log(res[["pop.lambda.min"]])
  
  # Table
  df[1,5:8] <- c(round(min(l.loglambda.bias[["True (pop)"]]), digits = 4), 
                 round(max(l.loglambda.bias[["True (pop)"]]), digits = 4), 
                 paste0(round(mean(l.loglambda.bias[["True (pop)"]]), digits = 4), " (", round(sd(l.loglambda.bias[["True (pop)"]]), digits = 4),")"),
                 paste0(round(quantile(l.loglambda.bias[["True (pop)"]], prob=0.5), digits = 4), " (", round(quantile(l.loglambda.bias[["True (pop)"]], prob = 0.25), digits = 4),", ", round(quantile(l.loglambda.bias[["True (pop)"]], prob = 0.75), digits = 4),")"))
  
  for(m in 1:length(method.ind)){
    df[m+1, 5:8] <- c(round(min(l.loglambda.bias[[m]]), digits = 4), 
                      round(max(l.loglambda.bias[[m]]), digits = 4), 
                      paste0(round(mean(l.loglambda.bias[[m]]), digits = 4), " (", round(sd(l.loglambda.bias[[m]]), digits = 4),")"),
                      paste0(round(quantile(l.loglambda.bias[[m]], prob=0.5), digits = 4), " (", round(quantile(l.loglambda.bias[[m]], prob = 0.25), digits = 4),", ", round(quantile(l.loglambda.bias[[m]], prob = 0.75), digits = 4),")"))
  }
  
  # Number of variables
  # ----------------------------------------------------------------------------
  
  # Data
  l.zeros <- list()
  for(m in method.ind){
    l.zeros[[paste0("method_",m)]] <- res[[paste0("method_",m)]][["estimated.nonzeros"]]
  }
  names(l.zeros) <- method.names
  l.zeros[["True (pop)"]] <- res[["pop.estimated.nonzeros"]]
  
  # Table
  df[1,9:12] <- c(round(min(l.zeros[["True (pop)"]]), digits = 4), 
                 round(max(l.zeros[["True (pop)"]]), digits = 4), 
                 paste0(round(mean(l.zeros[["True (pop)"]]), digits = 4), " (", round(sd(l.zeros[["True (pop)"]]), digits = 4),")"),
                 paste0(round(quantile(l.zeros[["True (pop)"]], prob=0.5), digits = 4), " (", round(quantile(l.zeros[["True (pop)"]], prob = 0.25), digits = 4),", ", round(quantile(l.zeros[["True (pop)"]], prob = 0.75), digits = 4),")"))
  
  for(m in 1:length(method.ind)){
    df[m+1, 9:12] <- c(round(min(l.zeros[[m]]), digits = 4), 
                      round(max(l.zeros[[m]]), digits = 4), 
                      paste0(round(mean(l.zeros[[m]]), digits = 4), " (", round(sd(l.zeros[[m]]), digits = 4),")"),
                      paste0(round(quantile(l.zeros[[m]], prob=0.5), digits = 4), " (", round(quantile(l.zeros[[m]], prob = 0.25), digits = 4),", ", round(quantile(l.zeros[[m]], prob = 0.75), digits = 4),")"))
  }
  
  write.csv(df,file = paste0("Tables/table-", obj.name,".csv"))
  
}