## Importing initial file into R ##

hotel<- read.csv(file.choose(), header = T, sep = ";")


hotel_int <- hotel[, c(4,7,15,16,19,21)]
head(hotel_int)
str(hotel_int)
## Random Sampling ##

samplesize = 0.80 * nrow(hotel_int)
set.seed(80)
index<-sample(seq_len(nrow(hotel_int)), size = samplesize)


# Create training and test set

datatrain = hotel_int[ index, ]
datatest = hotel_int[ -index, ]


## Scale data for neural network

max = apply(hotel_int , 2 , max)
min = apply(hotel_int , 2 , min)

scaled = as.data.frame(scale(hotel_int, center = min, scale = max - min))

head(scaled)


## Fit neural network 

# creating training and test set

trainNN = scaled[index , ]

testNN = scaled[-index , ]
head(testNN)
str(testNN)

# fit neural network 

library(neuralnet)

set.seed(2)

NN <- neuralnet(trainNN$Nightly_Rate~ trainNN$Number_Of_Rooms +
                              trainNN$Product_ID +
                              trainNN$Advance_Purchase_Adjusted +
                              trainNN$Party_Size +
                              trainNN$Length_of_Stay_Adjusted,
                              data = trainNN,
                              hidden = c(2,1),
                              act.fct = "logistic",
                              linear.output = TRUE)



# plot neural network

plot(NN)


## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(-6)])

str(predict_testNN)



# Scaling back to standard values to make a meaninful comparison #

predict_testNN = (predict_testNN$net.result * (max(hotel_int$Nightly_Rate)- 
                                              min(hotel_int$Nightly_Rate)))+ 
                                              min(hotel_int$Nightly_Rate)

# Prediction Vs. testing data for quality visualization #

plotNN <- plot(datatest$Nightly_Rate, predict_testNN, col='orange', 
                                            pch=16, 
                                            cex.axis = 1.0,
                                            main = "Prediction Quality vizualization on Testing Data",
                                            ylab = "predicted Nightly Rate", 
                                            xlab = "Testing Nightly Rate",
                                            abline(0,1))

plot(datatest$Nightly_Rate, col='blue', 
     pch=16, 
     cex.axis = 1.0,
     main = "Validated (Observed) Nightly Rate",
     ylab = "Nightly Rate")

plot(predict_testNN, col='red', 
     pch=16, 
     cex.axis = 1.0,
     main = "Predicted Nigtlhy Rate",
     ylab = "Nightly Rate")


# Quality by error

err_predict_NN = mean(abs(predict_testNN - datatest$Nightly_Rate))

#  = 43.77  #


## Prediction Quality comparison with Linear Regression ##


TRN_LM = scaled[index , ]

TST_LM = scaled[-index , ]



model = lm(Nightly_Rate ~ Number_Of_Rooms+
                          Product_ID +
                          Advance_Purchase_Adjusted + 
                          Party_Size + 
                          Length_of_Stay_Adjusted, 
                          data = TRN_LM)


predict_testLM = predict(model, TST_LM[,c(-6)])

str(predict_testLM)

predict_testLM = (predict_testLM * (max(hotel_int$Nightly_Rate) - 
                                    min(hotel_int$Nightly_Rate)))+ 
                                    min(hotel_int$Nightly_Rate)

err_predict_LM = mean(abs(predict_testLM - datatest$Nightly_Rate))

##  = 51.41  ##

err_base = mean(abs(mean(datatrain$Nightly_Rate) - datatest$Nightly_Rate))

# = 119.67  #


