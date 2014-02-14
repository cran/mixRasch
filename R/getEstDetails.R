`getEstDetails` <- function(raschResult, camelCase=TRUE){  
    out <- list(model = raschResult$model, iter = raschResult$iter, max.change=raschResult$max.change, converge.flag=raschResult$converge.flag, run.time=raschResult$run.time)	
    
    if(camelCase){
      names(out) <- c("model", "iter", "maxChange", "convergeFlag", "runTime")
    }		  	  
    out
  }