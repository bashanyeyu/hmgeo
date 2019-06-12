proximy.m <- function(ridata) {

###Just for proximy!!NOT density!
# location in rows and industies in cloumns
#in case of the error about "as.numic", add "/1"

ridata=as.data.frame(ridata)
rca_ridata=RCA(ridata[-1,-1]/1,binary = T)


####the key process!

rel_ridata=t(rca_ridata)%*%rca_ridata

#the conditional probabilities
rela_ridata=rel_ridata/apply(rel_ridata,2,max) #seclet the max num in each col
rela_ridata[is.nan(rela_ridata)]=0

#mini value ofconditional probabilities about a-b b-a
n=ncol(rela_ridata)
for(i in 1:n){

  for(j in 1:n){
    if (rela_ridata[i,j]>rela_ridata[j,i]){
      rela_ridata[i,j]=rela_ridata[j,i]
    }else{
      rela_ridata[j,i]=rela_ridata[i,j]}
  }

}


rela_ridata[is.nan(rela_ridata)]=0
rela_ridata=as.data.frame(rela_ridata)

}

proximy=function(ridata,location='r'){
  if(location=='r'){
    result=proximy.m(ridata)

  }else{

    if(location=='c'){
      ridata=t(ridata)
      result=proximy.m(ridata)
    }else{
      print("location should be 'r'(in rows) or 'c'(in cols)")
    }
  }
  result
}


