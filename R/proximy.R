

proximy <- function(mat) {




###要直接计算出来技术关联度，必须提前导入各省的产业区位熵！！！！


#要求rowname为地区，colname为产业；
# location in rows and industies in cloumns 
#只需要导入各个地级市，无需加和为省份
#这一步计算各个地级市的区位熵
#这里的“/1”是为了避免需要as.numic的错误
mat=as.matrix(mat)
rca_mat=RCA(mat/1,binary = T)

#这一步比较精彩，利用矩阵相乘，大大简化了i产业优势的地区，j产业也优势地区的百分比，计算的大量循环
#同时为1，矩阵相乘还是1，最后加和，得出同时为1的个数
#i优势产业条件下j产业也优势的地区个数
rel_mat=t(rca_mat)%*%rca_mat

#每一列的数除以该列最大值
rela_mat=rel_mat/apply(rel_mat,2,max)
rela_mat[is.nan(rela_mat)]=0

# #取（i,j）的最小值
n=ncol(rela_mat)
for(i in 1:n){
  for(j in 1:n){
    if (rela_mat[i,j]>rela_mat[j,i]){
      rela_mat[i,j]=rela_mat[j,i]
    }else{
      rela_mat[j,i]=rela_mat[i,j]}
  }
}

#将被0除而出现的NaN，取为0
rela_mat[is.nan(rela_mat)]=0
rela_mat=as.data.frame(rela_mat)

# rsum_rela=as.matrix( rowSums(rela_mat))
#
# #优势为1，非为0；乘以邻近性，加和；看和在总的邻近性中的比重
# des=matrix(0,n,1)
# for(j in 1:n){
#   des[j,]=sum(province_entory[,province]*rela_mat[,j])/rsum_rela[j,]
# }
# des[is.nan(des)]=0
# rownames(des)=colnames(mat)

}
