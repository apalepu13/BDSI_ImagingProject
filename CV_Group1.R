# Clean R
rm(list = ls())

library(jpeg)
library(png)
library(radiomics)
library(imager)
library(nabor)
library(pROC)
library(MASS)
library(Fragman)
library(EBImage)

###LOADING TRAINING IMAGES--FIRST 500###

yourpath <- "/Users/emmi/Desktop/BDSI/Training/"
files <- list.files(path=yourpath, pattern=".jpg", 
                    all.files=TRUE, full.names=TRUE)
images_list <- lapply(X=files[1:500], FUN=load.image)

###LOADING LABELS###

labels <- read.csv("/Users/emmi/Desktop/BDSI/BDSI_imaging_training_labels.csv")
truth <- labels[1:500,2] 
truth <-  car::recode(truth, "'Nevus' = 0; 'Melanoma' = 1")
truth <- as.integer(truth) - 1

###SEGMENTATION OF TRAINING IMAGES###

#outImage <- list()
#length(outImage)
#for(i in 1:500){
#  colorImage <- images_list[[i]]
#  grayImage <- colorImage[,,1]*0.21+colorImage[,,2]*0.72+colorImage[,,3]*0.07
#  outImage[[i]] <- grayImage }

#segmentation <- function(im){
#  threshold = otsu(im) 
#  im.th = im < threshold
#  return(im.th) }

#seg_list = lapply(outImage, FUN = segmentation)

#lesion <- function(inputImage, inputSeg, threshold){
  is.na(inputSeg) <- inputSeg == 0
  for(x in 11:440) {
    for(y in 11:590) {
      if(!is.na(inputSeg[x,y])) {
        count <- 0
        for(i in -10:10) {
          for(j in -10:10) {
            if(!is.na(inputSeg[x+i, y+j])) {
              count <- count + 1
            }
          }
        } 
        if(count < threshold) {
          inputSeg[x,y] <- NA
        }
      }
    }
  }
  
  for(m in 1:10) {
    for(n in 1:600) {
      inputSeg[m,n] <- NA
    }
  }
  
  for(m in 440:450) {
    for(n in 1:600) {
      inputSeg[m,n] <- NA
    }
  }
  
  for(a in 1:10) {
    for(b in 1:450) {
      inputSeg[b,a] <- NA
    }
  }
  
  for(a in 590:600) {
    for(b in 1:450) {
      inputSeg[b,a] <- NA
    }
  }
  
  lesionImage <- inputImage * inputSeg
}

#for(i in 1:500) {
#  lesionImages <- lesion(inputImage=outImage[[i]], inputSeg=seg_list[[i]], 150)
#  segmentation = !is.na(lesionImages)
#  plot(0:1, 0:1, type="n")
#  rasterImage(segmentation, 0, 0, 1, 1) 
#  save.image(as.cimg(rotate(flip(segmentation), 90)), file = paste("Segmentation", i, ".png", sep="")) }

###LOADING SEGMENTED IMAGES###

#yourpath <- "/Users/emmi/Desktop/BDSI/Segmentation/"

#files <- list.files(path=yourpath, pattern=".png",  all.files=TRUE)

#seg_images_list <- lapply(X=files, FUN=load.image)

###FEATURE EXTRACTION###

#gray_list = list()
#for(i in 1:500) {
#gray_im = grayscale(images_list[[i]])
#gray_list[[i]] = as.matrix(gray_im) }

togray = function(img){
  image_gray <- img[,,1]*0.21 + img[,,2]*0.72 + img[,,3]*0.07
  return(image_gray) }

gray_list = lapply(X=images_list, FUN=togray)

glcm_data = list()
for(i in 1:500) {
glcm_im = glcm(gray_list[[i]], angle=0, d = 1, n_grey = 16) 
glcm_data[[i]] <- glcm_im }

glrlm_data = list()
for(i in 1:500) {
glrlm_im = glrlm(gray_list[[i]], angle = 0, n_grey = 16, truncate = TRUE) 
glrlm_data[[i]] <- glrlm_im }

cat("extracting features.. \n")
glcm_features = lapply_pb(glcm_data, calc_features)

glcm_features_data = data.frame(matrix(unlist(glcm_features), nrow=500,byrow=TRUE))

colnames(glcm_features_data) = colnames(glcm_features[[1]])

cat("extracting features.. \n")
glrlm_features = lapply_pb(glrlm_data, calc_features)

glrlm_features_data = data.frame(matrix(unlist(glrlm_features), nrow=500,byrow=TRUE))

colnames(glrlm_features_data)=colnames(glrlm_features[[1]])

###LOADING GRAYSCALE FEATURES###

setwd("/Users/emmi/Desktop/BDSI/")
first_order_features = read.table("Features500.txt", header = TRUE)

all_features = cbind(first_order_features, glcm_features_data, glrlm_features_data)
features_matrix = as.matrix(all_features)

###LDA###
cvfit = cv.glmnet(features_matrix,truth,family="binomial",nfolds=10)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")

labels <- read.csv("/Users/emmi/Desktop/BDSI/BDSI_imaging_training_labels.csv")
labels <- labels[1:500,2] 

z = lda(labels ~ calc_entropy + calc_kurtosis + calc_uniformity + calc_min + glcm_variance + glcm_correlation + glcm_energy + glcm_IDMN + glrlm_GLN +
          glrlm_LRHGLE + glrlm_LRLGLE + glrlm_RLN + glrlm_SRE, all_features, prior = c(1,1)/2)

###GETTING FEATURES FOR TEST IMAGES###

#grayscale features

yourpath <- "/Users/emmi/Desktop/BDSI/Training/"
files <- list.files(path=yourpath, pattern=".jpg", 
                    all.files=TRUE, full.names=TRUE)

test_index = read.table("/Users/emmi/Desktop/BDSI/test_index.txt")[,1]

test_images_list <- lapply(X=files[test_index], FUN=readJPEG, native=FALSE)
cat("Convert to gray scale images ...\n")
test_gray = lapply_pb(X=test_images_list, FUN=togray)
cat("Extract features ...\n")
test_feature = lapply_pb(test_gray,calc_features)

test_first_order_features = data.frame(matrix(unlist(test_feature), nrow=length(test_feature),byrow=TRUE))
colnames(test_first_order_features)=colnames(test_feature[[1]])

#segmented lesion/glcm features

#outImage_test <- list()
#for(i in 1:100){
#  colorImage <- test_list[[i]]
#  grayImage <- colorImage[,,1]*0.21+colorImage[,,2]*0.72+colorImage[,,3]*0.07
#  outImage_test[[i]] <- grayImage }

#seg_list_test = lapply(outImage_test, FUN = segmentation)

#for(i in 1:100) {
#  lesionImages <- lesion(inputImage=outImage_test[[i]], inputSeg=seg_list_test[[i]], 150)
#  segmentation = !is.na(lesionImages)
#  plot(0:1, 0:1, type="n")
#  rasterImage(segmentation, 0, 0, 1, 1)
#  if(i < 10) {
#    save.image(as.cimg(rotate(flip(segmentation), 90)), file = paste("Segmentation", 0100, i, ".png", sep="")) }
#  else if(i <= 99) {
#    save.image(as.cimg(rotate(flip(segmentation), 90)), file = paste("Segmentation", 010, i, ".png", sep="")) }
#  else if(i == 100) {
#    save.image(as.cimg(rotate(flip(segmentation), 90)), file = paste("Segmentation", 01, i, ".png", sep="")) } }

#yourpath <- "/Users/emmi/Desktop/BDSI/Segmentation_Test/"
#files <- list.files(path=yourpath, pattern=".png",  all.files=TRUE)
#seg_images_list <- lapply(X=files, FUN=load.image)

yourpath <- "/Users/emmi/Desktop/BDSI/Training/"
files <- list.files(path=yourpath, pattern=".jpg", 
                    all.files=TRUE, full.names=TRUE)
test_images_list <- lapply(X=files[test_index], FUN=load.image)

###FEATURE EXTRACTION###
#gray_list_test = list()
#for(i in 1:100) {
#  gray_im = grayscale(test_images_list[[i]])
#  gray_list_test[[i]] = as.matrix(gray_im) }

gray_list_test = lapply(X=test_images_list, FUN=togray)

glcm_data_test = list()
for(i in 1:100) {
  glcm_im = glcm(gray_list_test[[i]], angle=0, d = 1, n_grey = 16) 
  glcm_data_test[[i]] <- glcm_im }

glrlm_data_test = list()
for(i in 1:100) {
  glrlm_im = glrlm(gray_list_test[[i]], angle = 0, n_grey = 16, truncate = TRUE) 
  glrlm_data_test[[i]] <- glrlm_im }

cat("extracting features.. \n")
glcm_features_test = lapply_pb(glcm_data_test, calc_features)

glcm_features_data_test = data.frame(matrix(unlist(glcm_features_test), nrow=100,byrow=TRUE))

colnames(glcm_features_data_test)=colnames(glcm_features_test[[1]])

cat("extracting features.. \n")
glrlm_features_test = lapply_pb(glrlm_data_test, calc_features)

glrlm_features_data_test = data.frame(matrix(unlist(glrlm_features_test), nrow=100,byrow=TRUE))

colnames(glrlm_features_data_test)=colnames(glrlm_features_test[[1]])

test_dat <- cbind(test_first_order_features, glcm_features_data_test, glrlm_features_data_test)

###PREDICTING CLASSIFICATION OF TEST IMAGES###

pr=predict(z, test_dat)
pr$class

labels <- read.csv("/Users/emmi/Desktop/BDSI/BDSI_imaging_training_labels.csv")
test_truth <- labels[test_index,2]

confusionMatrix(pr$class,test_truth)
