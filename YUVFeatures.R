library(jpeg)
library(png)
library(radiomics)
library(imager)

setwd("/home/wilzxu/BDSI_2018/Training")
yourpath <- "/home/wilzxu/BDSI_2018/Training"
files <- list.files(path=yourpath, pattern=".jpg", all.files=TRUE, full.names=FALSE)

lapply_pb <- function(X, FUN, ...){
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
  
  wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal +1 ,envir=env)
    setTxtProgressBar(get("pb", envir=env), curVal +1)
    FUN(...)
  }
  res <- lapply(X, wrapper, ...)
  close(pb)
  res
}

images_list <- lapply_pb(X=files, FUN=readJPEG, native=FALSE)

Y <- function(img){
  .299*img[,,1] + .587*img[,,2] + .114*img[,,3]
}
U <- function(img){
  -.14713*img[,,1] -.28886*img[,,2] + .436*img[,,3]
}
V <- function(img){
  .615*img[,,1] - .51499*img[,,2] - .10001*img[,,3]
}
Y_list <- lapply_pb(images_list, Y)
U_list <- lapply_pb(images_list, U)
V_list <- lapply_pb(images_list, V)
Y_feature <- lapply_pb(Y_list,calc_features)
U_feature <- lapply_pb(U_list,calc_features)
v_feature <- lapply_pb(V_list,calc_features)
Y_data=data.frame(matrix(unlist(Y_feature),nrow=5800,byrow=TRUE))
colnames(Y_data)=paste("Y_", colnames(Y_feature[[1]]), sep = "")
U_data=data.frame(matrix(unlist(U_feature),nrow=5800,byrow=TRUE))
colnames(U_data)=paste("U_", colnames(U_feature[[1]]), sep = "")
V_data=data.frame(matrix(unlist(v_feature),nrow=5800,byrow=TRUE))
colnames(V_data)=paste("V_", colnames(v_feature[[1]]), sep = "")
YUV_features <- cbind(Y_data, U_data, V_data)
write.table(YUV_features, file="YUV_features_5800.txt")