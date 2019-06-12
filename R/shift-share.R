ss=function(start,end){

  growrate=end/start-1
  ssum=apply(start,2,sum)
  esum=apply(end,2,sum)

  sstand= apply(as.matrix(ssum), 1, function(x){x/sum(start[,length(start)])*start[,ncol(start)]})
  estand=apply(as.matrix(esum), 1, function(x){x/sum(end[,length(end)])*end[,ncol(end)]})

  N= sstand*growrate[,ncol(growrate)] #增长份额
  N=N[,-(ncol(start))]

  S=(start-sstand)*growrate[,ncol(growrate)] #结构偏量
  S=S[,-(ncol(start))]

  res=cbind(N,S)
  grated=apply(growrate,2,function(x){x-growrate[,ncol(growrate)]})#竞争力偏量
  for (n in 1:(ncol(growrate)-1)) {
    res=cbind(res,start[,n]*grated[,n])
  }
  names(res)=rep(names(start[,1:(ncol(start)-1)]),3)
  res[is.na(res)]=0
  res
}
