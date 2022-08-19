fNecklaces <- 
function(pv=c(), bOut=FALSE, fn=1) {
  if (length(pv)<1) stop("The vector cannot be empty");
  if (fn<0)         stop("The second parameter cannot be negative");
  v<-c();
  for (i in 1:length(pv)) v<-c(v, rep(i-1+fn,pv[i])); 
  vP<-lSort(nPerm(v)); i<-1; 
  while (i<length(vP)){ u<-vP[[i]];
  vP<-setdiff(vP,cNecklaces(u)[-1]);i<-i+1}
  if (!bOut) return(vP);
  i<-1; for (w in vP) {cat("[",w,"]  (",i,")\n");i<-i+1};
}
