cBracelets <- 
function(v=c(),bOut=FALSE) {
  u<-lSort(union( cNecklaces(v),cNecklaces(v[length(v):1]) )) ;
  if (!bOut) return(u);
  i<-1; for (w in u) {cat("[",w,"]  (",i,")\n");i<-i+1};
}