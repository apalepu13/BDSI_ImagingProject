#GROUP 1 backwards AIC algo
library(radiomics)
library(jpeg)
library(png)


setwd("D:/Documents/BDSI/TrainingSet/")
getwd()

##### Load Data #####

## Read multiple images
# Pathway of your data folder
yourpath <- "D:/Documents/BDSI/TrainingSet/"
# A character vector of the names of files
files <- list.files(path=yourpath, pattern=".jpg", 
                    all.files=TRUE, full.names=FALSE)

files=files[1:100] #Choose the first 100 images 

# A list of 100 images read and saved in the list "images_list"
images_list <- lapply(X=files, FUN=readJPEG, native=FALSE) #native false to read 3 components

#function to make the gray image
togray = function(img){
  image_gray <- img[,,1]*0.21 + img[,,2]*0.72 + img[,,3]*0.07
  return(image_gray)
}

gray_list = lapply(X=images_list, FUN=togray)
#feature=matrix(0,nrow=length(images_list),ncol=)
gray_feature = lapply(gray_list,calc_features)
data=data.frame(matrix(unlist(gray_feature),nrow=100,byrow=TRUE))
colnames(data) = colnames(gray_feature[[1]])
y = read.table("../R_files/synth.txt")
v1 = y$V1

#Function to calculate optimal variables to use, using backwards AIC method
backwardsAIC <- function(Yvals, data) {
  fullFit = lm(Yvals~., data = data)
  minAIC = AIC(fullFit, k = 2)
  bestDF = data[, FALSE]
  nextDF = data
  while (dim(bestDF)[2] != dim(data)[2]) {
    data = nextDF
    for(i in 1:ncol(nextDF)){
      newDataFrame = data[,-i]
      potFit = lm(Yvals~., data = newDataFrame)
      AIC = AIC(potFit, k = 2) 
      if (AIC < minAIC) {
        minAIC = AIC
        bestDF = newDataFrame
      }
    }
    nextDF = bestDF
  }
  lm(Yvals~., data = bestDF)
}

backwardsAIC(v1, data)
