#First we need to read the csv file and assign it to data.

prudentialData = read.csv('C:\\Users\\avina\\Desktop\\prudential data\\Prudential_train.csv',header = TRUE)
data <- prudentialData

#Removing the columns where the mssing data is dominant in the entire column
data <- data[,-c(1,38,48,53,62,70)]


#Since we have missing data, we need to compute the average of the columns and insert that
#data to the missing columns

data$Employment_Info_1 <- ifelse(is.na(data$Employment_Info_1), ave(data$Employment_Info_1, 
                                                                    FUN=function(x) mean(x, na.rm = TRUE)), data$Employment_Info_1)

data$Employment_Info_4 <- ifelse(is.na(data$Employment_Info_4), ave(data$Employment_Info_4, 
                                                                     FUN=function(x) mean(x, na.rm = TRUE)), data$Employment_Info_4)

data$Employment_Info_6 <- ifelse(is.na(data$Employment_Info_6), ave(data$Employment_Info_6, 
                                                                     FUN=function(x) mean(x, na.rm = TRUE)), data$Employment_Info_6)

data$Insurance_History_5 <- ifelse(is.na(data$Insurance_History_5), ave(data$Insurance_History_5,
                                                                         FUN=function(x) mean(x, na.rm = TRUE)),data$Insurance_History_5)

data$Family_Hist_2 <- ifelse(is.na(data$Family_Hist_2), ave(data$Family_Hist_2, 
                                                             FUN=function(x) mean(x, na.rm = TRUE)), data$Family_Hist_2)

data$Family_Hist_3 <- ifelse(is.na(data$Family_Hist_3), ave(data$Family_Hist_3, 
                                                             FUN=function(x) mean(x, na.rm = TRUE)), data$Family_Hist_3)

data$Family_Hist_4 <- ifelse(is.na(data$Family_Hist_4), ave(data$Family_Hist_4, 
                                                             FUN=function(x) mean(x, na.rm = TRUE)), data$Family_Hist_4)

#data$Family_Hist_5 <- ifelse(is.na(data$Family_Hist_5), mean(data$Family_Hist_5, 
#    FUN=function(x) mean(x, na.rm = TRUE)), data$Family_Hist_5)


data$Medical_History_1 <- ifelse(is.na(data$Medical_History_1), ave(data$Medical_History_1,
                                                                     FUN=function(x) mean(x, na.rm = TRUE)), data$Medical_History_1)



# Now we need to convert the categorical columns, since these categories are not just
#numeric coeffcients, we need to divide these into separate columns.

#This is the function which applicable for all categorical columns. Since we have large
#data, practically it is not possible to convert levels to labels for every column,  so I
#created a function to do this job.

categoricalfunc <- function(data, Columnname){
  for(level in unique(data[[Columnname]])){
    data[paste(Columnname,seq = "_",level)]<- ifelse(data[[Columnname]] == level,1,0)
  }
  return(subset(data,select = -get(Columnname)))
}

#After creating the function, we need to apply this function to category columns.

data <- categoricalfunc(data, "Product_Info_1")
data <- categoricalfunc(data, "Product_Info_2")
#Too many categories, treated as descrete value
#data <- categoricalfunc(data, "Product_Info_3")
data <- categoricalfunc(data, "Product_Info_5")
data <- categoricalfunc(data, "Product_Info_6")
data <- categoricalfunc(data, "Product_Info_7")
#Too many categories, treated as descrete value
#data <- categoricalfunc(data, "Employment_Info_2")
data <- categoricalfunc(data, "Employment_Info_3")
data <- categoricalfunc(data, "Employment_Info_5")
data <- categoricalfunc(data, "InsuredInfo_1")
data <- categoricalfunc(data, "InsuredInfo_2")
data <- categoricalfunc(data, "InsuredInfo_3")
data <- categoricalfunc(data, "InsuredInfo_4")
data <- categoricalfunc(data, "InsuredInfo_5")
data <- categoricalfunc(data, "InsuredInfo_6")
data <- categoricalfunc(data, "InsuredInfo_7")
data <- categoricalfunc(data, "Insurance_History_1")
data <- categoricalfunc(data, "Insurance_History_2")
data <- categoricalfunc(data, "Insurance_History_3")
data <- categoricalfunc(data, "Insurance_History_4")
data <- categoricalfunc(data, "Insurance_History_7")
data <- categoricalfunc(data, "Insurance_History_8")
data <- categoricalfunc(data, "Insurance_History_9")
data <- categoricalfunc(data, "Family_Hist_1")
#Too many categories, treated as descrete value
#data <- categoricalfunc(data, "Medical_History_2")
data <- categoricalfunc(data, "Medical_History_3")
data <- categoricalfunc(data, "Medical_History_4")
data <- categoricalfunc(data, "Medical_History_5")
data <- categoricalfunc(data, "Medical_History_6")
data <- categoricalfunc(data, "Medical_History_7")
data <- categoricalfunc(data, "Medical_History_8")
data <- categoricalfunc(data, "Medical_History_9")
data <- categoricalfunc(data, "Medical_History_11")
data <- categoricalfunc(data, "Medical_History_12")
data <- categoricalfunc(data, "Medical_History_13")
data <- categoricalfunc(data, "Medical_History_14")
data <- categoricalfunc(data, "Medical_History_16")
data <- categoricalfunc(data, "Medical_History_17")
data <- categoricalfunc(data, "Medical_History_18")
data <- categoricalfunc(data, "Medical_History_19")
data <- categoricalfunc(data, "Medical_History_20")
data <- categoricalfunc(data, "Medical_History_21")
data <- categoricalfunc(data, "Medical_History_22")
data <- categoricalfunc(data, "Medical_History_23")
data <- categoricalfunc(data, "Medical_History_25")
data <- categoricalfunc(data, "Medical_History_26")
data <- categoricalfunc(data, "Medical_History_27")
data <- categoricalfunc(data, "Medical_History_28")
data <- categoricalfunc(data, "Medical_History_29")
data <- categoricalfunc(data, "Medical_History_30")
data <- categoricalfunc(data, "Medical_History_31")
data <- categoricalfunc(data, "Medical_History_33")
data <- categoricalfunc(data, "Medical_History_34")
data <- categoricalfunc(data, "Medical_History_35")
data <- categoricalfunc(data, "Medical_History_36")
data <- categoricalfunc(data, "Medical_History_37")
data <- categoricalfunc(data, "Medical_History_38")
data <- categoricalfunc(data, "Medical_History_39")
data <- categoricalfunc(data, "Medical_History_40")
data <- categoricalfunc(data, "Medical_History_41")

summary(data[,121:180])

#Finally, here is the last step, we need to write this file to csv to apply regressions.
Finaldata <- data[-(50001:59382)]

write.csv(Finaldata, file = "C:\\Users\\avina\\Desktop\\prudential data\\Prudential_Cleaned.csv", row.names = FALSE)
