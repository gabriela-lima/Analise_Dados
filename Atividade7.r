y <- 5
mult<- function(x,y){
  return (x*y)
}

mult(10)

a <- USArrests

prisoes <- function(estados,tiposPrisoes){
  if(estados%in%row.names(USArrests)){
  }else{
    return("Estado Inv�lido")
  }
  if(tiposPrisoes%in%colnames(USArrests)){
  }else{
    return("Tipo de pris�o inv�lida")
  }
  totalPrisoes <- rowSums(USArrests[estados,tiposPrisoes])
  
  
  return (totalPrisoes)
}


mapply(FUN = prisoes,c("Utah","Ohio"),c("Murder","UrbanPop"))

prisoes(c("Utah","Ohio"),c("Murder","UrbanPop"))

prisoes(estados="Tennessee", tiposPrisoes=c("Rape","Murder"))
prisoes(estados=c("California ","Miami", "Arizona"), tiposPrisoes=("Assault"))
prisoes(estados=c("Pennsylvania","Mississippi", "Nebraska"), tiposPrisoes=c("Rape","UrbanPop","Assault"))
prisoes(estados=c("Vermont","Wisconsin", "Texas"), tiposPrisoes=c("Rape","Assalto")) 
