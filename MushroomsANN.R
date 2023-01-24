#Mushroom analysis
#Edin Ziga 190302192
#⠀⠀⠀⠀⠀⠀⠀   ⠀⣠⣤⣤⣤⣤⣤⣶⣦⣤⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀ 
#⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣿⡿⠛⠉⠙⠛⠛⠛⠛⠻⢿⣿⣷⣤⡀⠀⠀⠀⠀⠀ 
#⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⠋⠀⠀⠀⠀⠀⠀⠀⢀⣀⣀⠈⢻⣿⣿⡄⠀⠀⠀⠀ 
#⠀⠀⠀⠀⠀⠀⠀⣸⣿⡏⠀⠀⠀⣠⣶⣾⣿⣿⣿⠿⠿⠿⢿⣿⣿⣿⣄⠀⠀⠀ 
#⠀⠀⠀⠀⠀⠀⠀⣿⣿⠁⠀⠀⢰⣿⣿⣯⠁⠀⠀⠀⠀⠀⠀⠀⠈⠙⢿⣷⡄⠀ 
#⠀⠀⣀⣤⣴⣶⣶⣿⡟⠀ ⠀⢸⣿⣿⣿⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣷⠀ 
#⠀⢰⣿⡟⠋⠉⣹⣿⡇⠀⠀⠘⣿⣿⣿⣿⣷⣦⣤⣤⣤⣶⣶⣶⣶⣿⣿⣿⠀ 
#⠀⢸⣿⡇⠀⠀⣿⣿⡇⠀⠀⠀⠹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠃⠀ 
#⠀⣸⣿⡇⠀⠀⣿⣿⡇⠀ ⠀⠀⠉⠻⠿⣿⣿⣿⣿⡿⠿⠿⠛⢻⣿⡇⠀⠀ 
#⠀⣿⣿⠁⠀⠀⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⢸⣿⣧⠀⠀ 
#⠀⣿⣿⠀⠀⠀⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⢸⣿⣿⠀⠀ 
#⠀⣿⣿⠀⠀⠀⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⢸⣿⣿⠀⠀ 
#⠀⢿⣿⡆⠀⠀⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⢸⣿⡇⠀⠀ 
#⠀⠸⣿⣧⡀⠀⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⣿⣿⠃⠀⠀ 
#⠀⠀⠛⢿⣿⣿⣿⣿⣇⠀ ⠀⠀⣰⣿⣿⣷⣶⣶⣶⣶⠶⠀ ⢠⣿⣿ ⠀⠀⠀ 
#⠀⠀⠀⠀⠀⠀⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⡇⠀⣽⣿⡏⠁⠀⠀ ⢸⣿⡇⠀⠀⠀ 
#⠀⠀⠀⠀⠀⠀⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⡇⠀⢹⣿⡆⠀⠀⠀ ⣸⣿⠇⠀⠀⠀ 
#⠀⠀⠀⠀⠀⠀⠀⢿⣿⣦⣄⣀⣠⣴⣿⣿⠁⠀⠈⠻⣿⣿⣿⣿⡿⠏⠀⠀⠀⠀ 
#⠀⠀⠀⠀⠀⠀⠀⠈⠛⠻⠿⠿⠿⠿⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
#For CS417 Introduction to Data Mining

rm(list = ls())
setwd("C:/Users/Edin's PC/Desktop")
mushroomsData <- read.csv("mushrooms.csv")
str(mushroomsData)
head(mushroomsData)
#install.packages("plyr")
library(plyr)

#GENERAL PREPROCESSING
#Covert Characters to Numerical Values
#-------------------------------------------------------------------------------
mushroomsData$class <- revalue(mushroomsData$class,
                             c("p"="1", "e"="0"))

mushroomsData$cap.shape <- revalue(mushroomsData$cap.shape,
                               c("b"="1", "c"="2","x"="3","f"="4",
                                 "k"="5","s"="6"))
mushroomsData$cap.surface <- revalue(mushroomsData$cap.surface,
                                   c("f"="1", "g"="2","y"="3","s"="4"))

mushroomsData$cap.color <- revalue(mushroomsData$cap.color,
                                   c("n"="1", "b"="2","c"="3","g"="4",
                                     "r"="5","p"="6","u"="7","e"="8",
                                     "w"="9","y"="10"))

mushroomsData$bruises <- revalue(mushroomsData$bruises,
                                   c("t"="1", "f"="2"))

mushroomsData$odor <- revalue(mushroomsData$odor,
                              c("a"="1", "l"="2","c"="3","y"="4",
                                "f"="5","m"="6","n"="7","p"="8",
                                "s"="9"))
mushroomsData$gill.attachment <- revalue(mushroomsData$gill.attachment,
                              c("a"="1", "d"="2","f"="3","n"="4"))

mushroomsData$gill.spacing <- revalue(mushroomsData$gill.spacing,
                              c("c"="1", "w"="2","d"="3"))

mushroomsData$gill.size <- revalue(mushroomsData$gill.size,
                              c("b"="1", "n"="2"))

mushroomsData$gill.color <- revalue(mushroomsData$gill.color,
                                   c("k"="1", "n"="2","b"="3","h"="4",
                                     "g"="5","r"="6","o"="7","p"="8",
                                     "u"="9","e"="10","w"="11","y"="12"))
mushroomsData$stalk.shape <- revalue(mushroomsData$stalk.shape,
                                   c("e"="1", "t"="2"))

mushroomsData$stalk.root <- revalue(mushroomsData$stalk.root,
                                    c("b"="1", "c"="2","u"="3","e"="4",
                                      "z"="5","r"="6","?"="7"))

mushroomsData$stalk.surface.above.ring <- revalue(mushroomsData$stalk.surface.above.ring,
                                    c("f"="1", "y"="2","k"="3","s"="4"))

mushroomsData$stalk.surface.below.ring <- revalue(mushroomsData$stalk.surface.below.ring,
                                                  c("f"="1", "y"="2","k"="3","s"="4"))

mushroomsData$stalk.color.above.ring <- revalue(mushroomsData$stalk.color.above.ring,
                                                c("n"="1", "b"="2","c"="3","g"="4",
                                                  "o"="5","p"="6","e"="7","w"="8",
                                                  "y"="9"))

mushroomsData$stalk.color.below.ring <- revalue(mushroomsData$stalk.surface.below.ring,
                                                c("n"="1", "b"="2","c"="3","g"="4",
                                                  "o"="5","p"="6","e"="7","w"="8",
                                                  "y"="9"))

mushroomsData$veil.type <- revalue(mushroomsData$veil.type,
                                     c("p"="1", "u"="2"))


mushroomsData$veil.color <- revalue(mushroomsData$veil.color,
                                                c("n"="1", "o"="2","w"="3","y"="4"))

mushroomsData$ring.number <- revalue(mushroomsData$ring.number,
                                    c("n"="1", "o"="2","t"="3"))


mushroomsData$ring.type <- revalue(mushroomsData$ring.type,
                                                c("c"="1", "e"="2","f"="3","l"="4",
                                                  "n"="5","p"="6","s"="7","z"="8"))

mushroomsData$spore.print.color <- revalue(mushroomsData$spore.print.color,
                                   c("k"="1", "n"="2","b"="3","h"="4",
                                     "r"="5","o"="6","u"="7","w"="8","y"="9"))


mushroomsData$population <- revalue(mushroomsData$population,
                                           c("a"="1", "c"="2","n"="3","s"="4",
                                             "v"="5","y"="6"))

mushroomsData$habitat <- revalue(mushroomsData$habitat,
                                    c("g"="1", "l"="2","m"="3","p"="4",
                                      "u"="5","w"="6","d"="7"))

str(mushroomsData)

##FEATURE SELECTIOn
##RF Algorithm to discover relevant features
##------------------------------------------------------------------------------

##Covert Data to factors
mushroomsDataFactor <- mushroomsData %>% mutate_at(c("class","cap.shape", "cap.surface", "cap.color", 
                                         "bruises", "odor", "gill.attachment",
                                         "gill.spacing", "gill.size","gill.color",
                                         "stalk.shape", "stalk.root","stalk.surface.above.ring",
                                         "stalk.surface.below.ring", "stalk.color.above.ring",
                                         "stalk.color.below.ring","veil.type", "veil.color",
                                         "ring.number","ring.type", "spore.print.color",
                                         "population","habitat"), as.factor) 
str(mushroomsDataFactor)
#install.packages('DataExplorer')
library("DataExplorer")
library("caret")
library("randomForest")

##Plotting
plot_bar(mushroomsDataFactor) 
plot_correlation(mushroomsDataFactor)
library(caTools)

#Randomly splitting into training and test
in_train <- createDataPartition(mushroomsDataFactor$class, p = 0.80,
                                list = FALSE)
in_train
training_set <- mushroomsDataFactor[in_train, ]
test_set <- mushroomsDataFactor[-in_train, ]

#RF Function Template
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # 

#RF Actual Run
#DO NOT RUN, TAKES FOREVER TO FINISH
result_rfe1 <- rfe(x = training_set[-1], #exclude the target variable  
                   y = training_set$class,
                   sizes = c(1:22),# can try different sizes = c(1:5)
                   rfeControl = control)
#Results
result_rfe1
predictors(result_rfe1)
varImp(result_rfe1)
varImportanceRF <- varImp(result_rfe1, scale = TRUE)
varImportanceRF
print(varImportanceRF,  top = 5) 

varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:7], 
                          importance = varImp(result_rfe1)[1:7, 1])

##Plotting features
ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()

postResample(predict(result_rfe1, test_set[-1]),test_set$class)

##ANN Algorithm
##------------------------------------------------------------------------------


##Preprocessing - Coverting data to Numeric and logical(for class)
mushroomsData$class <- as.numeric(mushroomsData$class)
mushroomsData$class <- as.logical(mushroomsData$class)
mushroomsData$cap.shape <- as.numeric(mushroomsData$cap.shape)
mushroomsData$cap.surface <- as.numeric(mushroomsData$cap.surface)
mushroomsData$cap.color <- as.numeric(mushroomsData$cap.color)
mushroomsData$bruises <- as.numeric(mushroomsData$bruises)
mushroomsData$odor <- as.numeric(mushroomsData$odor)
mushroomsData$gill.attachment <- as.numeric(mushroomsData$gill.attachment)
mushroomsData$gill.spacing <- as.numeric(mushroomsData$gill.spacing)
mushroomsData$gill.size <- as.numeric(mushroomsData$gill.size)
mushroomsData$gill.color <- as.numeric(mushroomsData$gill.color)
mushroomsData$stalk.shape <- as.numeric(mushroomsData$stalk.shape)
mushroomsData$stalk.root <- as.numeric(mushroomsData$stalk.root)
mushroomsData$stalk.surface.above.ring <- as.numeric(mushroomsData$stalk.surface.above.ring)
mushroomsData$stalk.surface.below.ring <- as.numeric(mushroomsData$stalk.surface.below.ring)
mushroomsData$stalk.color.above.ring <- as.numeric(mushroomsData$stalk.color.above.ring)
mushroomsData$stalk.color.below.ring <- as.numeric(mushroomsData$stalk.color.below.ring)
mushroomsData$veil.type <- as.numeric(mushroomsData$veil.type)
mushroomsData$veil.color <- as.numeric(mushroomsData$veil.color)
mushroomsData$ring.number <- as.numeric(mushroomsData$ring.number)
mushroomsData$ring.type <- as.numeric(mushroomsData$ring.type)
mushroomsData$spore.print.color <- as.numeric(mushroomsData$spore.print.color)
mushroomsData$population <- as.numeric(mushroomsData$population)
mushroomsData$habitat <- as.numeric(mushroomsData$habitat)
str(mushroomsData)

#Normalization Function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
mushroomsNorm <- as.data.frame(lapply(mushroomsData, normalize))

#Randomly splitting into training and test
set.seed(12345)
in_train <- createDataPartition(mushroomsNorm$class, p = 0.80,
                                list = FALSE)
mushroomTrain <- mushroomsNorm[in_train, ]
mushroomTest <- mushroomsNorm[-in_train, ]


##Load and run the ANN Algorithm
library(neuralnet)
set.seed(100)
mushroomsModel <- neuralnet(formula = class ~odor+spore.print.color+gill.color
                            +gill.size+population+stalk.root+habitat,
                            data = mushroomTrain,hidden = 2)

#Plotting the model and testing it against the test dataset
plot(mushroomsModel)
mushroomsModel$result.matrix
model_results <- compute(mushroomsModel, mushroomTest)

#Postprocessing, coverting the results into boolean values
MushroomsPredicted <- model_results$net.result
MushroomsPredicted <-as.numeric(MushroomsPredicted)
MushroomTestClass <- as.logical(mushroomTest$class)
rounding <- function(x){
  if(x<0.1){x=FALSE} else if(x>0.9) {x=TRUE}
  return(x)
}
MushroomsPredicted <- as.logical(lapply(MushroomsPredicted, rounding))

##Overview of results using a confustion matrix
#-------------------------------------------------------------------------------
#install.packages('caret')
#install.packages('e1071')
library(caret)
library(e1071)


confusion_mat = as.matrix(table(Actual_Values = MushroomTestClass, Predicted_Values = MushroomsPredicted))
print(confusion_mat)

confusionMatrix(factor(MushroomsPredicted),factor(MushroomTestClass))






