#table <- rse_combined(formula = 'S_W_INDEX ~ WEIGHT + AGE + SEX', datasources = datasources, data = "D")

robust_combined_HC1_dev <- function(formula, datasources, data) {
  start.time <- proc.time()[3]

  variables <- sapply(strsplit(formula, split = "[~+]"), function(x) gsub(" ","",as.character(x)))
  numVar <- length(variables)

  #converting categorical to numeric

  vector <- c()
  for (i in 1:numVar){
    if (sum(ds.class(paste0(data, "$", variables[i]), datasources = datasources)=="factor") == length(datasources)){
      ds.asNumeric(paste0(data, "$", variables[i]), datasources = datasources, newobj=variables[i])
      vector[i] <-  variables[i]
    }
    else{
      vector[i]<- paste0(data, "$", variables[i])
    }
  }


  ds.dataFrame(vector, completeCases = TRUE, newobj="Dcompl", datasources = datasources, stringsAsFactors = FALSE)

  mod <- ds.glm(formula = formula, data=data, family='gaussian', datasources = datasources)

  ds.make(toAssign = paste0(mod$coefficients[,1][1],
                            paste0("+((",mod$coefficients[,1][-1],")*Dcompl$",variables[-1],")", collapse=""  ) ),
          newobj='fv',
          datasources = datasources)

  ds.make(toAssign = paste0("Dcompl$",variables[1],'-fv'),
          newobj = 'resid',
          datasources = datasources)

  for (i in 1:length(datasources)){

    dim <- print(ds.dim('Dcompl', datasources = datasources[i],type = "split")[[1]][1])
    number <- (dim/(dim-numVar))
    ds.make(toAssign = paste0(number,'*(resid * resid)'), newobj='hc1omega', datasources=datasources[i])

  }


  variables_expansion <- expand.grid(variables,
                                     variables)

  variables_expansion$Var1 <- as.character(variables_expansion$Var1)
  variables_expansion$Var2 <- as.character(variables_expansion$Var2)

  variable_combinations_unique <- variables_expansion[!duplicated(t(apply(variables_expansion, 1, sort))),]
  variable_combinations_unique$ID <- 1:length(variable_combinations_unique$Var1)
  variable_combinations_unique <- variable_combinations_unique[,c(3,1,2)]

  for (i in 1:length(variable_combinations_unique$ID)){

    if (variable_combinations_unique$Var1[i] == variables[1] && variable_combinations_unique$Var2[i] == variables[1]){

      #### XTX
      variable_combinations_unique$XTXValue[i] <- ds.dim("Dcompl", datasources = datasources)[[length(datasources)+1]][1]

      #### XTOmegaX
      ds.dataFrame(rep(paste0("hc1omega"), 10), newobj = "hc1omega_df", stringsAsFactors = FALSE, datasources = datasources)
      ds.rowColCalc(x="hc1omega_df", operation='colSums', newobj="hc1omega_sum", datasources = datasources)
      var_mean <- ds.mean("hc1omega_sum", datasources = datasources)
      variable_combinations_unique$XTOmegaXValue[i] <- sum(var_mean[[1]][,1])

    } else if ((variable_combinations_unique$Var1[i] == variables[1]) | (variable_combinations_unique$Var2[i] == variables[1])){

      #### XTX
      vars_pair <- c(variable_combinations_unique$Var1[i], variable_combinations_unique$Var2[i])
      var_for_comp <- vars_pair[!(vars_pair %in% variables[1])]
      ds.dataFrame(rep(paste0("Dcompl$", var_for_comp), 10), newobj = paste0(var_for_comp,"_df"), stringsAsFactors = FALSE, datasources = datasources)
      ds.rowColCalc(x=paste0(var_for_comp,"_df"), operation='colSums', newobj=paste0(var_for_comp,"_sum"), datasources = datasources)
      var_mean <- ds.mean(paste0(var_for_comp,"_sum"), datasources = datasources)
      variable_combinations_unique$XTXValue[i] <- sum(var_mean[[1]][,1])

      #### XTOmegaX
      ds.make(toAssign = paste0('Dcompl$', var_for_comp,'*hc1omega'), newobj=paste0(var_for_comp,"_Xhc1"), datasources = datasources)
      ds.dataFrame(rep(paste0(var_for_comp,"_Xhc1"), 10), newobj = paste0(var_for_comp,"_Xhc1_df"), stringsAsFactors = FALSE, datasources = datasources)
      ds.rowColCalc(x=paste0(var_for_comp,"_Xhc1_df"), operation='colSums', newobj=paste0(var_for_comp,"_hc1sum"), datasources = datasources)
      var_mean <- ds.mean(paste0(var_for_comp,"_hc1sum"), datasources = datasources)
      variable_combinations_unique$XTOmegaXValue[i] <- sum(var_mean[[1]][,1])


    } else if (!(variable_combinations_unique$Var1[i] == variables[1]) && !(variable_combinations_unique$Var2[i] == variables[1])){

      #### XTX
      ds.make(toAssign = paste0("Dcompl$",variable_combinations_unique$Var1[i], "*Dcompl$", variable_combinations_unique$Var2[i]), newobj=paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i]), datasources = datasources)
      ds.dataFrame(rep(paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i]), 10), newobj = paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_df"), stringsAsFactors = FALSE, datasources = datasources)
      ds.rowColCalc(x=paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_df"), operation='colSums', newobj=paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_sum"), datasources = datasources)
      var_mean <- ds.mean(paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_sum"), datasources = datasources)
      variable_combinations_unique$XTXValue[i] <- sum(var_mean[[1]][,1])

      ####XTOmegaX
      ds.make(toAssign = paste0("Dcompl$",variable_combinations_unique$Var1[i], "*Dcompl$", variable_combinations_unique$Var2[i],"*hc1omega"), newobj=paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_Xhc1"), datasources = datasources)
      ds.dataFrame(rep(paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i],"_Xhc1"), 10), newobj = paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_Xhc1df"), stringsAsFactors = FALSE, datasources = datasources)
      ds.rowColCalc(x=paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_Xhc1df"), operation='colSums', newobj=paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_Xhc1sum"), datasources = datasources)
      var_mean <- ds.mean(paste0(variable_combinations_unique$Var1[i],"_x_", variable_combinations_unique$Var2[i], "_Xhc1sum"), datasources = datasources)
      variable_combinations_unique$XTOmegaXValue[i] <- sum(var_mean[[1]][,1])

    }

  }

  for (z in 1:length(variable_combinations_unique$Var1)){
    variable_combinations_unique$Cell[z] <- list(sort(c(variable_combinations_unique$Var1[z], variable_combinations_unique$Var2[z])))
  }


  for (k in 1:length(variables_expansion$Var1)){
    variables_expansion$Cell[k] <- list(sort(c(variables_expansion$Var1[k], variables_expansion$Var2[k])))
  }

  XTX_Match <- dplyr::left_join(variables_expansion, variable_combinations_unique, by = "Cell")

  XTX <- matrix(XTX_Match$XTXValue, nrow=numVar, ncol=numVar)
  XTOmegaX <- matrix(XTX_Match$XTOmegaXValue, nrow=numVar, ncol=numVar)


  #### end of change for TXOmegaX

  vce_hc1 <- solve(XTX) %*% XTOmegaX %*% solve(XTX)
  robust_SE <- sqrt(diag(vce_hc1))

  # Obtain the new 95% confidence intervals
  beta <- mod$coefficients[,1]
  robust_LCI <- beta - (robust_SE*qnorm(0.975))
  robust_UCI <- beta + (robust_SE*qnorm(0.975))

  # Obtain test statistic z (= beta/SE)
  robust_z <- beta/robust_SE

  # Obtain new p value P = exp(−0.717×z − 0.416×z2)
  robust_p <- 2*pnorm(q=abs(robust_z),lower.tail=FALSE)

  robust_table <- data.frame(unlist(beta),unlist(robust_SE),unlist(robust_z),unlist(robust_p),unlist(robust_LCI),unlist(robust_UCI), rep(mod$Nvalid, numVar))

  colnames(robust_table)<- c("Beta", "Robust SE", "Robust Z", "Robust P", "Robust LCI", "Robust UCI", "Nvalid")

  totalTimeMins2 <- (proc.time()[3] - start.time) / 60
  message("The entire analysis took ", round(totalTimeMins2, 2), " minutes")

  return(robust_table)
}
