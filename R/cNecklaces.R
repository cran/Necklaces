cNecklaces <- 
function(v=c(),bOut=FALSE) {
  lShift <- function(v){
    if (length(v)<2) return(v); 
    c(v[2:length(v)],v[1])
  }
  if (length(v)<1) stop("The vector cannot be empty");
  if (length(v)==1) return(v);
  u<-list(v);
  for (i in 1:(length(v)-1)) u[[i+1]]<-lShift(unlist(u[[i]]));
  u<-lSort(unique(u))
  if (!bOut) return(u);
  i<-1; for (w in u) {cat("[",w,"]  (",i,")\n");i<-i+1};
}