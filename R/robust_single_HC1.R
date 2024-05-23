rse_single_HC1 <- function(formula, datasources, type = "split", data) {
  
  
  if (type =="split"){
    
    allStudies <- list()
    
    for (p in 1:length(datasources)){
      
      modDS1 <- ds.glm(formula = formula, 
                       data=data,
                       family='gaussian', 
                       datasources = datasources[p])
      
      numVar <- length(rownames(modDS1$coefficients))
      
      variables <- sapply(strsplit(formula, split = "[~+]"), function(x) gsub(" ","",as.character(x)))
      
      IPD_list<- list()
      
      for (i in 1:numVar){
        
        IPD_list[paste0(rownames(modDS1$coefficients))[i]] <- modDS1[["coefficients"]][i]
        
      }
      
      ds.cbind(paste0(data,"$", variables),newobj="vars",datasources = datasources[p])# df only with vars of interest
      
      
      ds.completeCases("vars", newobj="vars.complete", datasources = datasources[p]) # df wout missings
      
      
      newFormula <- list()
      
      for (i in 1:numVar){
        
        if(i==1){
          
          newFormula[i]<-IPD_list[i]
        }
        
        else if(ds.class(paste0("vars.complete$", gsub('[0-9]+', '', names(IPD_list)[i])), datasources = datasources[p])=="factor"){
          
          ds.asInteger(paste0("vars.complete$",gsub('[0-9]+', '', names(IPD_list)[i])),
                       paste0(gsub('[0-9]+', '', names(IPD_list)[i]),".int"),datasources=datasources[p])
          
          newFormula[i]<- paste0("+(",IPD_list[i],"*",(paste0(gsub('[0-9]+', '', names(IPD_list)[i]),".int")),")")
        }
        
        else {
        
            newFormula[i]<- paste0("+(",IPD_list[i],"*",(paste0("vars.complete$",gsub('[0-9]+', '', names(IPD_list)[i]))),")")
        }
        
      }
      
      newFormula2 <- (paste(newFormula, collapse = ""))
      
      ds.make(toAssign = newFormula2,
              
              newobj = "predicted.values.IPD",
              
              datasources = datasources[p]) #modify accord.to incl. studies
      
      
      ds.make(toAssign = paste0("vars.complete$",variables[1], " - predicted.values.IPD"), 
              
              newobj = "residuals.IPD", 
              
              datasources = datasources[p]) #modify accord.to incl. studies
      
      #N complete cases 
      n_list <- ds.dim('vars.complete', datasources = datasources[p]) # list with number of observations (participants) and number of variables
      n <- n_list[[1]][[1]] # this exctracts only the number of participants (length of dataset with complete cases)
      
      # creating diagonal matrix
      ds.rep(1, times = n, source.x1 = 'c', source.times = 'c', datasources = datasources[p], newobj = 'intercept') #times = complete cases from all studies
      
      ds.dataFrame(c('intercept',
                     paste0("vars.complete$",variables[-1] )), 
                   newobj= 'Xmat' , 
                  stringsAsFactors = FALSE,
                   datasources = datasources[p])
      
      ds.asDataMatrix(x.name='Xmat', newobj='Xmat', datasources = datasources[p])
      
      #ds.dim('Xmat', datasources = datasources[p])
      
      # transposing matrix
      ds.matrixTranspose(M1='Xmat', newobj='tXmat', datasources = datasources[p])
      
      ds.matrixMult(M1='tXmat', M2='Xmat', newobj='XTX', datasources=datasources[p])
      #ds.dim('XTX', datasources=datasources[p])
      
      ds.matrixInvert(M1='XTX', newobj='XTXinv', datasources = datasources[p])
      #ds.dim('XTXinv', datasources=datasources[p])
    
      
      ds.make(toAssign = paste0("(",n, "/(", n-3,'))*(residuals.IPD^2)'), newobj='hc1', datasources=datasources[p])
      
      
      ds.matrixDiag(x1='hc1', aim = "serverside.vector.2.matrix", nrows.scalar = n,  # n is for the length of the complete cases dataset
                    newobj='hc1diagN', datasources = datasources[p])
      #ds.dim('hc2diagN', datasources=datasources[p])
      
      ds.matrixMult(M1='tXmat', M2='hc1diagN', newobj='mm1', datasources=datasources[p])
      #ds.dim('mm1', datasources=datasources[p])
      
      ds.matrixMult(M1='mm1', M2='Xmat', newobj='mm2', datasources=datasources[p])
      #ds.dim('mm2', datasources=datasources[p])
      
      ds.matrixMult(M1='XTXinv', M2='mm2', newobj='mm3', datasources = datasources[p])
      #ds.dim('mm3', datasources=datasources[p])
      
      ds.matrixMult(M1='mm3', M2='XTXinv', newobj='mm4', datasources = datasources[p])
      #ds.dim('mm4', datasources=datasources[p])
      
      # calculate hc2 standard errors
      ds.matrixDiag(x1='mm4', aim='serverside.matrix.2.vector', newobj='hc1diagVCE', datasources = datasources[p])
      ds.length('hc1diagVCE', datasources=datasources[p])
      
      ds.rep(x1='hc1diagVCE', each=100, source.x1='s', source.each='c', newobj='hc1diagVCErep', 
             datasources = datasources[p])
      
      ds.rep(x1=c(1:numVar), each=100, source.x1='c', source.each='c', newobj='variable',  # x1= number of covariates + intercept
             datasources = datasources[p]) 
      
      mean <- ds.meanSdGp(x = 'hc1diagVCErep', y = 'variable', datasources = datasources[p])
      
      robust_SE <- sqrt(mean$Mean_gp_study[,'COMBINE']) # WE GET THE ROBUST STANDARD ERRORS
      
      
      # Obtain the new 95% confidence intervals
      beta <- unlist(IPD_list)
      robust_LCI <- beta - (robust_SE*qnorm(0.975))
      robust_UCI <- beta + (robust_SE*qnorm(0.975))
      
      # Obtain test statistic z (= beta/SE)
      robust_z <- beta/robust_SE
      
      # Obtain new p value P = exp(−0.717×z − 0.416×z2)
      robust_p <- 2*pnorm(q=abs(robust_z),lower.tail=FALSE)
      
      # Put it all together (dataframe with all measures, for all variables and the intercept)
      
      IPD_robust <- data.frame(unlist(beta),unlist(robust_SE),unlist(robust_z),unlist(robust_p),unlist(robust_LCI),unlist(robust_UCI))
      
      rownames(IPD_robust)<- rownames(modDS1$coefficients)
      colnames(IPD_robust)<- c("Beta", "Robust SE", "Robust Z", "Robust P", "Robust LCI", "Robust UCI")
      allStudies[[datasources[[p]]@name]] <- IPD_robust
      
    }
    return(allStudies)
  }
}
