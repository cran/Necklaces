fBracelets <- 
function( pv=c(), bOut=FALSE, fn=1 ) {
  if (length(pv)<1) stop("The vector cannot be empty");
  if (fn<0)         stop("The second parameter cannot be negative");
  vN<-fNecklaces(pv,FALSE,fn);
  i<-1; vOut<-list();
  while (length(vN)>0){
      vOut[[i]]<-vN[[1]];
      vN<-setdiff(vN,cBracelets(vN[[1]]) );
      i<-i+1;
  }
  if (!bOut) return(vOut);
  i<-1; for (w in vOut) {cat("[",w,"]  (",i,")\n");i<-i+1};
}