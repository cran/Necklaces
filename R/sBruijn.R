sBruijn <- function(n=1,m=2, fn=0, bSep=FALSE){
  if (n < 1) stop("n and m must be positive integers");
  sB<-"";sSep<-ifelse(bSep,".","");
  for (u in lSort(Necklaces(n,m,fn)[[2]])){                        
       apPrefix<-length(cNecklaces(u));
       sB<-paste0(sB,sSep,paste(u[1:apPrefix],collapse=""))}
  noquote(ifelse(bSep,substr(sB,2,nchar(sB)),sB))
} 
