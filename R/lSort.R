lSort <-
function(pL=list()){
  v<-c();oL<-list()
  if (length(pL)<1) stop("The list cannot be empty");
  for (u in pL) v<-sort(c(v,toString(u))); 
  for (i in 1:length(pL)) oL[[i]]<-strtoi(unlist(strsplit(v[i], "[,]")));
  oL;
}
