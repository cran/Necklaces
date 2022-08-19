LyndonW <- function(n=1,m=2,bOut=FALSE, fn=1){
  if (n < 1 || m< 1) stop("n and m must be positive integers");
  i<-0; oL<-list(); iL<-Necklaces(n,m,fn);
  for (u in iL[[2]]) { 
      if (length(cNecklaces(u))==n) {i<-i+1; oL[[i]]<-u};};
  oL<-lSort(oL);
  if (!bOut) return(oL);
  i<-1; for (w in oL) {cat("[",unlist(w),"]  (",i,")\n");i<-i+1};
}  