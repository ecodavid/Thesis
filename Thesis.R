hotel<- read.csv(file.choose(), header = T, sep=";")
head(hotel)

                

hotel_filtered <- hotel[ , c(3,4,5,6,7,9,13,14,15,16,18,19,20,21,23,25,27,28,29)]
str(hotel_filtered)
head(hotel_filtered)

hotel_filtered$Hotel_Location_Category<- as.factor(hotel_filtered$Hotel_Location_Category)
hotel_filtered$Hotel_Size_Category<- as.factor(hotel_filtered$Hotel_Size_Category)
hotel_filtered$Income_Level_Category<- as.factor(hotel_filtered$Income_Level_Category)
hotel_filtered$Distribution_Channel_number<- as.factor(hotel_filtered$Distribution_Channel_number)
hotel_filtered$Party_Size_Category<- as.factor(hotel_filtered$Party_Size_Category)
hotel_filtered$Nightly_Rate_Category<- as.factor(hotel_filtered$Nightly_Rate_Category)
hotel_filtered$Rate_number<- as.factor(hotel_filtered$Rate_number)
hotel_filtered$Room_Type_number<- as.factor(hotel_filtered$Room_Type_number)
hotel_filtered$Merge_Indicator<- as.factor(hotel_filtered$Merge_Indicator)
hotel_filtered$Membership_Status<- as.factor(hotel_filtered$Membership_Status)
hotel_filtered$VIP_Membership_Status<- as.factor(hotel_filtered$VIP_Membership_Status)


          #######  Nigtly rate Category Vs. Distribution Channel  ######


## Nightly Rate category
T_Rate<-table(hotel_filtered$Nightly_Rate_Category)
## frequencey table for T_Rate
frq_T_Rate<- T_Rate/4346
barplot(frq_T_Rate, beside=t, col=rainbow(4), 
                  main= "Room Rate Category",
                  ylab= "Frecuency",
                  xlab= "Price range")

legend("topright",c("0 <= 80", 
                    "MORE THAN 80 <= 150", 
                    "MORE THAN 150 <= 300", 
                    "MORE THAN 300"),
                    pch =15, col = rainbow(4))

# more than 40% of bookings fell in the price category 2
## combining frequency and table Rate
cbind(T_Rate, frq_T_Rate)


##    Distribution channel
T_channel<-table(hotel_filtered$Distribution_Channel)
frq_Channel<- T_channel/4346
barplot(frq_Channel, beside=t, col=rainbow(3),
                main= "Distribution Channel",
                ylab= "Frecuency",
                xlab= "Distribution Channel")

# more of 60% of the bookins were made through the CRO/Hotel
cbind(T_channel, frq_Channel)


## Distribution. Contingency table

test_table<- table(hotel_filtered$Nightly_Rate_Category,
                   hotel_filtered$Distribution_Channel)

ftest_table<- test_table/4346
round(ftest_table,2)

barplot(ftest_table, beside = T, col = rainbow(4), 
                                  ylab = "Frecuency", 
                                  xlab = "Distribution Channel")  

legend("topright",c("0 <= 80", 
                    "MORE THAN 80 <= 150", 
                    "MORE THAN 150 <= 300", 
                    "MORE THAN 300"),
                    pch =15, col = rainbow(4))

 
## Conducting Chi-squared test with significant level of 0.05

# Ho. Nightly rate and distribution channel are independent
  #   Distribution channel has not influence in the definition of prices #
  #   DC does not explain prices behaviour #
# Ha. Nightly rate and distribution channel are dependent


chisq.test(test_table)

# As the significant level is less than 0.05 according to the p-value, then
# Ho is rejected and is concluded with 95% of confidence that nightly rate and
# distribution channel are dependent.


#######  Nightly Rate Vs. Advance purchase  ######

cor(hotel_filtered$Nightly_Rate,hotel_filtered$Advance_Purchase_Adjusted)
# 0.16 #
plot(hotel_filtered$Nightly_Rate,hotel_filtered$Advance_Purchase_Adjusted)

# There is not significant correlation, however it can be observed that of
# the bookings were made whitin 50 days prior to the check in etc.


#######  Type of hotel size Vs. Distribution Channel  ######


## Type of Hotel size
T_Hotel<-table(hotel_filtered$Hotel_Size)
frq_T_Hotel<- T_Hotel/4346
barplot(frq_T_Hotel, beside=t,col=rainbow(3), 
                    main= "Distribution of bookings by hotel size",
                    ylab= "Frecuency",
                    xlab= "Hotel size")

legend("topright",c("Big - 260 or more rooms", 
                    "Medium - 71 to 259 rooms", 
                    "Small - up to 70 rooms"),
                    pch =15, col = rainbow(3))
       

# more than 50% of hotels are big more than 259 rooms fell in the price category 2
cbind(T_Hotel, frq_T_Hotel)

##    Distribution channel
T_channel<-table(hotel_filtered$Distribution_Channel)
frq_Channel<- T_channel/4346
barplot(frq_Channel, beside=t, col=rainbow(3),
                          main= "Distribution Channel",
                          ylab= "Frecuency",
                          xlab= "Distribution Channel")

# more of 60% of the bookins were made through the CRO/Hotel
cbind(T_channel, frq_Channel)


## Distribution. Contingency table

DC_HS<- table(hotel_filtered$Distribution_Channel,
                   hotel_filtered$Hotel_Size)

DC_HS_table<- DC_HS/4346
round(DC_HS_table,2)

barplot(DC_HS_table, beside = T, col = rainbow(3), 
                               main= "Distribution Channel",
                              ylab = "Frecuency", 
                              xlab = "Hotel Size")  

legend("topright",c("CRO/Hotel","GDS","WEB"),
                    pch =15, col = rainbow(3))

# CRO/Hotel was the prefered distribution channel amongst all three type of hotels.


## Conducting Chi-squared test with significant level of 0.05

# Ho. Hotel size and distribution channel are independent
# Ha. Hotel size and distribution channel are dependent


chisq.test(DC_HS)

# As the significant level is less than 0.05 according to the p-value, then
# Ho is rejected and is concluded with 95% of confidence that hotel size and
# distribution channel are dependent.



#######  Nigtly rate Category Vs. Rate number (Type of booking)  ######


## Nightly Rate category
T_Rate<-table(hotel_filtered$Nightly_Rate_Category)
## frequencey table for T_Rate
frq_T_Rate<- T_Rate/4346
barplot(frq_T_Rate, beside=t,col = rainbow(4),
              main= "Distribution of bookings by Rate category",
              ylab= "Frecuency",
              xlab= "Rate category")

legend("topright",c("0 <= 80", 
                    "MORE THAN 80 <= 150", 
                    "MORE THAN 150 <= 300", 
                    "MORE THAN 300"),
                    pch =15, col = rainbow(4))

# more than 40% of bookings fell in the price category 2
## combining frequency and table Rate
cbind(T_Rate, frq_T_Rate)

##    Rate Number
R_Number<-table(hotel_filtered$Rate_number)
frq_R_Number<- R_Number/4346
barplot(frq_R_Number, beside=t, 
              col = rainbow(8),
              main= "Distribution of bookings by Rate number",
              ylab= "Frecuency",
              xlab= "Rate number")

legend("topright",c("Adv. Purchase at least 7 day. Fully Restricted Rate",
                    "Rack Rate - Unrestricted Rate", 
                    "Rack Rate with Add. Hotel Services", 
                    "Discount Rate Less Restricted Than Adv. Purchase",
                    "Accom. with Frequent Traveler Rewards",
                    "Accom. with In Room Services",
                    "Accom.Airport/Airline Services",
                    "Accom. with City/Weekend Activities"),
                    pch =15, col = rainbow(8))

# The most sold type of room with 57% fell in the 2 category Rack Rate
cbind(R_Number, frq_R_Number)


## Distribution. Contingency table

DR_Number<- table(hotel_filtered$Rate_number,
                   hotel_filtered$Nightly_Rate_Category)

DR_Number_table<- DR_Number/4346
round(DR_Number_table,2)

barplot(DR_Number_table, beside = T, col = rainbow(8), 
                          ylab = "Frecuency", 
                          xlab = "Rate Category") 

legend("bottomleft", c("Adv. Purchase at least 7 day. Fully Restricted Rate",
                    "Rack Rate - Unrestricted Rate", 
                    "Rack Rate with Add. Hotel Services", 
                    "Discount Rate Less Restricted Than Adv. Purchase",
                    "Accom. with Frequent Traveler Rewards",
                    "Accom. with In Room Services",
                    "Accom.Airport/Airline Services",
                    "Accom. with City/Weekend Activities"),
                    pch =15, col = rainbow(8))


## Conducting Chi-squared test with significant level of 0.05

# Ho. Nightly rate category and type of rate are independent
# Ha. Nightly rate category and type of rate are dependent


chisq.test(DR_Number)

# As the significant level is less than 0.05 according to the p-value, then
# Ho is rejected and is concluded with 95% of confidence that nightly rate category and
# type of rate are dependent.



###  Correlation with Pearson coefficient for numerical variables   ###


n_variable <- hotel_filtered[, c(2,9,10,12,14)]

str(n_variable)

hotel_cor <- cor(n_variable, method = "pearson")
round(hotel_cor,2)

   
plot(hotel_filtered$Advance_Purchase_Adjusted,hotel_filtered$Length_of_Stay_Adjusted)

# There is not significant correlation, however it can be observed that
# most of the bookings were made whitin 50 days prior to the check in etc and length of stay
# was mostly within a frame time of 10 days.
