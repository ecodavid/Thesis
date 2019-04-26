## Creating index variable 

hotel <- read.csv(file.choose(), header=T, sep=";")
hotel_tree <- hotel[ , c(3,4,5,7,8,14,15,17,19,20,23,25,28)]
str(hotel_tree)
head(hotel_tree)

## Converting variables into the right classification

hotel_tree$Hotel_Location_Category<-as.factor(hotel_tree$Hotel_Location_Category)
hotel_tree$Nightly_Rate_Category<-as.factor(hotel_tree$Nightly_Rate_Category)
hotel_tree$Rate_number<-as.factor(hotel_tree$Rate_number)
hotel_tree$Room_Type_number<-as.factor(hotel_tree$Room_Type_number)
hotel_tree$Membership_Status<-as.factor(hotel_tree$Membership_Status)

# Decision tree with Party
install.packages("party")
library(party)

# Random sampling

samplesize = 0.80 * nrow(hotel_tree)
set.seed(80)
index_tree <- sample(seq_len(nrow(hotel_tree)), size = samplesize)


# Create training and test datasets

train_hotel = hotel_tree[index_tree, ]
test_hotel = hotel_tree[-index_tree, ]


# fit decision tree

h_tree <- ctree(Nightly_Rate_Category~ Hotel_Location_Category+
                    Number_Of_Rooms+
                    Hotel_Size+
                    Product_ID+
                    Customer_Income_Level+
                    Distribution_Channel+
                    Advance_Purchase_Adjusted+
                    Party_Size_Description+
                    Length_of_Stay_Adjusted+
                    Rate_number+
                    Room_Type_number+
                    Membership_Status,
                    data = train_hotel,
                    controls = ctree_control(mincriterion = 0.99, minsplit = 1000))
  
h_tree

plot(h_tree, cex = .4)
          

## Prediction on testing data ##

predict_datatest_t <- predict(h_tree, test_hotel)

plot(predict_datatest_t)

## Comparison between testing and predictive data ##

head(predict_datatest_t)

head(test_hotel$Nightly_Rate_Category)

plot(predict_datatest_t,test_hotel$Nightly_Rate_Category)


    ## Misclassification error for train data ##

table_hotel<- table(predict(h_tree),train_hotel$Nightly_Rate_Category)
table_hotel

1-sum(diag(table_hotel))/sum(table_hotel)
# = 0.2399 #
# The misclassification error is about 24.36% based on the training data



    ## Misclassification error for testing data ##

table_hotel2<- table(predict_datatest_t,test_hotel$Nightly_Rate_Category)

table_hotel2


1-sum(diag(table_hotel2))/sum(table_hotel2)

# = 0.2367 #



