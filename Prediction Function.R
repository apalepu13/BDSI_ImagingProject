library(MASS)
library(caret)
library(glmnet)
library(jpeg)
library(png)
library(radiomics)
library(imager)
library(Fragman)

predictTestIms <- function(train_features, test_images, train_labels){
  n <- length(train_labels)
  cvfit = cv.glmnet(as.matrix(train_features), train_labels,family="binomial",nfolds=10)
  cvfit$lambda.1se
  a <- coef(cvfit, s = "lambda.1se")
  indices = slot(a, "i")
  column_index <- indices
  train_features_labels <- cbind(train_features[,column_index], train_labels)
  z <- lda(train_labels ~ ., train_features_labels, prior = c(.52, .48))
  cat("Reading to JPEG...\n")
  images_list <- lapply_pb(X=test_images, FUN=readJPEG, native=FALSE)
  togray = function(img){
    image_gray <- img[,,1]*0.21 + img[,,2]*0.72 + img[,,3]*0.07
    return(image_gray)
  }
  #cat("Converting to grayscale...\n")
  #gray_list = lapply_pb(X=images_list, FUN=togray)
  #cat("Calculating gray features...\n")
  #gray_feature = lapply_pb(gray_list,calc_features)
  #first_order=data.frame(matrix(unlist(gray_feature),nrow=n,byrow=TRUE))
  #colnames(first_order)=colnames(gray_feature[[1]])
  gray_list_matrix <- lapply_pb(X=gray_list, FUN=as.matrix)
  cat("Calculating glcm...\n")
  glcm_list <- lapply_pb(X=gray_list_matrix, FUN=glcm, angle=0, d=1, n_grey=16)
  cat("Calculating glcm features...\n")
  glcm_features <- lapply_pb(X=glcm_list, FUN=calc_features)
  glcm_features_data <- data.frame(matrix(unlist(glcm_features),nrow=n,byrow=TRUE))
  colnames(glcm_features_data)=colnames(glcm_features[[1]])
  cat("Calculating glrlm...\n")
  glrlm_list <- lapply_pb(X=gray_list_matrix, FUN=glrlm, angle=0, n_grey=16, truncate=TRUE)
  cat("Calculating glrlm features...\n")
  glrlm_features <- lapply_pb(X=glrlm_list, FUN=calc_features)
  glrlm_features_data <- data.frame(matrix(unlist(glrlm_features),nrow=n,byrow=TRUE))
  colnames(glrlm_features_data)=colnames(glrlm_features[[1]])
  red <- function(img){
    img[,,1]
  }
  blue <- function(img){
    img[,,2]
  }
  green <- function(img){
    img[,,3]
  }
  cat("Converting to red...\n")
  red_list <- lapply_pb(images_list, red)
  cat("Converting to blue...\n")
  blue_list <- lapply_pb(images_list, blue)
  cat("Converting to green...\n")
  green_list <- lapply_pb(images_list, green)
  cat("Calculating red features...\n")
  red_feature <- lapply_pb(red_list,calc_features)
  cat("Calculating blue features...\n")
  blue_feature <- lapply_pb(blue_list,calc_features)
  cat("Calculating green features...\n")
  green_feature <- lapply_pb(green_list,calc_features)
  red_data=data.frame(matrix(unlist(red_feature),nrow=n,byrow=TRUE))
  colnames(red_data)=paste("red_", colnames(red_feature[[1]]), sep = "")
  blue_data=data.frame(matrix(unlist(blue_feature),nrow=n,byrow=TRUE))
  colnames(blue_data)=paste("blue_", colnames(blue_feature[[1]]), sep = "")
  green_data=data.frame(matrix(unlist(green_feature),nrow=n,byrow=TRUE))
  colnames(green_data)=paste("green_", colnames(green_feature[[1]]), sep = "")
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
  Y_feature <- lapply_pb(Y_list,calc_features)
  cat("Calculating U features...\n")
  U_feature <- lapply_pb(U_list,calc_features)
  cat("Calculating V features...\n")
  v_feature <- lapply_pb(V_list,calc_features)
  Y_data=data.frame(matrix(unlist(Y_feature),nrow=n,byrow=TRUE))
  colnames(Y_data)=paste("Y_", colnames(Y_feature[[1]]), sep = "")
  U_data=data.frame(matrix(unlist(U_feature),nrow=n,byrow=TRUE))
  colnames(U_data)=paste("U_", colnames(U_feature[[1]]), sep = "")
  V_data=data.frame(matrix(unlist(v_feature),nrow=n,byrow=TRUE))
  colnames(V_data)=paste("V_", colnames(v_feature[[1]]), sep = "")
  test_features <- cbind( red_data, blue_data, green_data, Y_data, U_data, V_data, glcm_features_data, glrlm_features_data,)[,column_index]
  write.table(test_features, file="myTestSetFeatures.txt")
  pr <- predict(z, test_features)
  pr$class
}
#total ims: 6300
training.index = (1:5000)
test.index = (5001:6300)
yourpath <- "D:/Documents/bdsi_imaging/Anil/TrainingSet"
setwd(yourpath)
setwd("../R_Files")
train_features <- read.table("allTrainingSetFeatures.txt")[training.index,]
test_images <- list.files(path=yourpath, pattern=".jpg", all.files=TRUE, full.names=FALSE)[test.index]
train_labels <- read.csv("allTrainingLabels.csv", header = TRUE)[training.index,2]

setwd(yourpath)
results <- predictTestIms(train_features, test_images, train_labels)
confusionMatrix(results, read.csv("BDSI_imaging_training_labels.csv", header = TRUE)[test.index,2])
