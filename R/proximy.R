proximy <- function(mat) {

###Just for proximy!!NOT density!
# location in rows and industies in cloumns 
#need the cities not provinces
#in case of the error about "as.numic", add "/1"
mat=as.matrix(mat)
rca_mat=RCA(mat/1,binary = T)

####the key process!
rel_mat=t(rca_mat)%*%rca_mat

#the conditional probabilities
rela_mat=rel_mat/apply(rel_mat,2,max)
rela_mat[is.nan(rela_mat)]=0

#min()
n=ncol(rela_mat)
for(i in 1:n){
  for(j in 1:n){
    if (rela_mat[i,j]>rela_mat[j,i]){
      rela_mat[i,j]=rela_mat[j,i]
    }else{
      rela_mat[j,i]=rela_mat[i,j]}
  }
}


rela_mat[is.nan(rela_mat)]=0
rela_mat=as.data.frame(rela_mat)

}
