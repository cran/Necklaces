Necklaces <- 
function(n=1,m=1, fn=1){
  if (n < 1 || m< 1) stop("n and m must be positive integers");
  vP<-mkT(n,m)
  vN<-list(); i<-0;
  for (v in vP) {
    for (u in fNecklaces(unlist(v),FALSE,fn)) {
      i<-i+1; vN[[i]]<-u}
  };
  vN<-list( length(vN), lSort(vN));
  vN;
}  
