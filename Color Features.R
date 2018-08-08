library(jpeg)
library(png)
library(radiomics)
library(imager)


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
red <- function(img){
  img[,,1]
}
blue <- function(img){
  img[,,2]
}
green <- function(img){
  img[,,3]
}
red_list <- lapply_pb(images_list, red)
blue_list <- lapply_pb(images_list, blue)
green_list <- lapply_pb(images_list, green)
red_feature <- lapply_pb(red_list,calc_features)
blue_feature <- lapply_pb(blue_list,calc_features)
green_feature <- lapply_pb(green_list,calc_features)
red_data=data.frame(matrix(unlist(red_feature),nrow=5800,byrow=TRUE))
colnames(red_data)=paste("red_", colnames(red_feature[[1]]), sep = "")
blue_data=data.frame(matrix(unlist(blue_feature),nrow=5800,byrow=TRUE))
colnames(blue_data)=paste("blue_", colnames(blue_feature[[1]]), sep = "")
green_data=data.frame(matrix(unlist(green_feature),nrow=5800,byrow=TRUE))
colnames(green_data)=paste("green_", colnames(green_feature[[1]]), sep = "")
color_features <- cbind(red_data, blue_data, green_data)
write.table(color_features, file="color_features_5800.txt")