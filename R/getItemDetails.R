`getItemDetails` <- function(raschResult, item, class=1, camelCase=TRUE){
  if(substr(raschResult$model,1,3)=="mix"){
    rR <- raschResult[[1]][[class]]$item.par
  } else{
    rR <- raschResult$item.par
  }
  
  itemNames <- colnames(raschResult$item.par$delta)
  if(item  %in% itemNames){
    itemSelect <- itemNames == item	
  } else itemSelect <- item
  
  n.cat <- rR$tau[,itemSelect]
  n.cat <- sum(! is.na(n.cat))
  if(is.na(rR$SE.tau)){
    SE.tau <- NA
  } else{
    SE.tau <- rR$SE.tau[1:n.cat,itemSelect]
  }	
  out <- list(item.name=item, n.cat=n.cat+1, delta.i = rR$delta.i[itemSelect], SE.delta.i = rR$SE.delta.i[itemSelect], tau = rR$tau[1:n.cat,itemSelect], SE.tau = SE.tau, infit = rR$in.out[,"infit"][itemSelect], in.Z = rR$in.out[,"in.Z"][itemSelect],outfit = rR$in.out[,"outfit"][itemSelect],out.Z = rR$in.out[,"out.Z"][itemSelect])	
  
  if(camelCase) names(out) <- c("itemName", "nCat", "deltaI", "SEDeltaI", "tau", "SETau", "infit", "inZ", "outfit", "outZ")
  out
}