library(MASS)
library(caret)
library(glmnet)
library(jpeg)
library(png)
library(radiomics)
library(imager)
library(Fragman)

collect_features_part3 <- function(test_images){
  n <- length(test_images)
  cat("Reading to JPEG...\n")
  images_list <- lapply_pb(X=test_images, FUN=readJPEG, native=FALSE)
  Y <- function(img){
    .299*img[,,1] + .587*img[,,2] + .114*img[,,3]
  }
  U <- function(img){
    -.14713*img[,,1] -.28886*img[,,2] + .436*img[,,3]
  }
  V <- function(img){
    .615*img[,,1] - .51499*img[,,2] - .10001*img[,,3]
  }
  cat("Converting to Y...\n")
  Y_list <- lapply_pb(images_list, Y)
  cat("Converting to U...\n")
  U_list <- lapply_pb(images_list, U)
  cat("Converting to V...\n")
  V_list <- lapply_pb(images_list, V)
  cat("Calculating Y features...\n")
  Y_feature <- lapply_pb(Y_list,calc_features,features=c("calc_min"))
  cat("Calculating U features...\n")
  U_feature <- lapply_pb(U_list,calc_features,features=c("calc_kurtosis","calc_skewness","calc_min","calc_RMS","calc_sd"))
  cat("Calculating V features...\n")
  v_feature <- lapply_pb(V_list,calc_features,features=c("calc_energy","calc_entropy","calc_max","calc_sd"))
  Y_data=data.frame(matrix(unlist(Y_feature),nrow=n,byrow=TRUE))
  colnames(Y_data)=paste("Y_", colnames(Y_feature[[1]]), sep = "")
  U_data=data.frame(matrix(unlist(U_feature),nrow=n,byrow=TRUE))
  colnames(U_data)=paste("U_", colnames(U_feature[[1]]), sep = "")
  V_data=data.frame(matrix(unlist(v_feature),nrow=n,byrow=TRUE))
  colnames(V_data)=paste("V_", colnames(v_feature[[1]]), sep = "")
  test_features_part3 <- cbind(Y_data, U_data, V_data)
  write.table(test_features_part3, file="test_features_part3.txt")
}

setwd("D:/Documents/bdsi_imaging/Anil/ValidationSet")
yourpath <- "D:/Documents/bdsi_imaging/Anil/ValidationSet"
test_images <- list.files(path=yourpath, pattern=".jpg", all.files=TRUE, full.names=FALSE)
collect_features_part3(test_images)
