#Installing Necessary packages:
install.packages("forecast")
install.packages("Metrics")
library(ggplot2)
library(tseries)
library(forecast)

#Creating the function for Root_Mean_Square_Error:
rmse=function(error)
{ 
  sqrt(mean(error^2))
}

#Importing Datasets and dividing into Training [1992-2015] and Test [2016-2017] sets:

cpi_u = CPI_U_City_Average$CPI_U
cpi_u1 = cpi_u[-c(289:312)]
cpi_u2 = cpi_u[c(289:312)]

Tot_Mid =All_Features$V2
Tot_Mid1 = Tot_Mid[-c(94:325)]
Tot_Mid2 = Tot_Mid[c(94:105)]


Fin_Mid = All_Features$V3
Fin_Mid1 = Fin_Mid[-c(94:325)]
Fin_Mid2 = Fin_Mid[c(94:105)]

Cash_Mid = All_Features$V4
Cash_Mid1 = Cash_Mid[-c(94:325)]
Cash_Mid2 = Cash_Mid[c(94:105)]

Tot_Avg = All_Features$V5
Tot_Avg1 = Tot_Avg[-c(94:325)]
Tot_Avg2 = Tot_Avg[c(94:105)]

Fin_Avg = All_Features$V6
Fin_Avg1 = Fin_Avg[-c(94:325)]
Fin_Avg2= Fin_Avg[c(94:105)]

Cash_Avg = All_Features$V7
Cash_Avg1 = Cash_Avg[-c(94:325)]
Cash_Avg2 = Cash_Avg[c(94:105)]

Tot_Cnt = All_Features$V8
Tot_Cnt1 = Tot_Cnt[-c(94:325)]
Tot_Cnt2 = Tot_Cnt[c(94:105)]

Fin_Cnt = All_Features$V9
Fin_Cnt1 = Fin_Cnt[-c(94:325)]
Fin_Cnt2 = Fin_Cnt[c(94:105)]

Tot_Sum = All_Features$V10
Tot_Sum1 = Tot_Sum[-c(94:325)]
Tot_Sum2 = Tot_Sum[c(94:105)]

Cash_Sum = All_Features$V11
Cash_Sum1 = Cash_Sum[-c(94:325)]
Cash_Sum2 = Cash_Sum[c(94:105)]

CPI_dat = All_Features$V13
CPI_dat1 = CPI_dat[-c(94:325)]
CPI_dat2 = CPI_dat[c(94:105)]

Thirty_yr = All_Features$V14
Thirty_yr1 = Thirty_yr[-c(94:325)]
Thirty_yr2 = Thirty_yr[c(94:105)]

Retail = All_Features$V15
Retail1 = Retail[-c(94:325)]
Retail2 = Retail[c(94:105)]

home_fur = All_Features$V16
home_fur1 = home_fur[-c(94:325)]
home_fur2 = home_fur[c(94:105)]

Popu = All_Features$V17
Popu1 = Popu[-c(94:325)]
Popu2 = Popu[c(94:105)]

##############################
avg_hs_pr = Avg_House_Sell_Price$Avg_House_Sell_Pr
avg_hs_pr1=avg_hs_pr[-c(289:314)]
avg_hs_pr2=avg_hs_pr[c(289:312)]

retail = Retail_Excluding_Auto_Parts$Retail_Excluding_Auto
retail1 = retail[-c(289:312)]
retail2 = retail[c(289:312)]

pop = Population_Data$Population
pop1 = pop[-c(289:324)]
pop2 = pop[c(289:312)]

pop_diff = Population_Data_2$Pop_Diff
pop_diff1 = pop_diff[-c(289:324)]
pop_diff2 = pop_diff[c(289:312)]

home = Home_and_furniture_sale_dat$Home_and_furniture_sale
home1 = home[-c(289:312)]
home2 = home[c(289:312)]

#Printing Out the datasets and dimensions of datasets to verify the filtering process and consistency of feature space (equal no. of training data-points as the no. of labels):
print(cpi_u)
length(pop2); length(cpi_u2);length(home2);length(retail2); length(avg_hs_pr2); 
length(sales)

############### Testing various Linear Regressive Models on the training data: ############################
model2 = lm(sales ~ cpi_u1 + avg_hs_pr1 + retail1 + pop1 + home1 +pop_diff1)
summary(model2)
model3 = lm(sales ~  pop1 + retail1 + home1)
model4 = glm(sales ~  pop1 + retail1 + home1,family=gaussian)
accuracy(model4)
model5 = lm(sales ~  retail1|pop1|home1)
model6 = lm(sales ~ polym(avg_hs_pr1,cpi_u1,retail1, pop1, home1, degree=4,raw=TRUE))
forcasted = forecast.lm(model6, newdata= yh=12,level = c(80, 95))
AIC(model6)
summary(model6)$coefficients["Pr(>|t|)"]
data.frame(summary(model6)$coef[summary(model6)$coef[,4] <= .05, 4])
summary(model6)
anova(model6)
accuracy(model6)
plot(model6) 
###############################################################################################################


# Creating a dataframe for the 'training_set' and the 'test_set':
train_data=data.frame(cpi_u1,avg_hs_pr1,retail1,pop1,pop_diff1,home1)
new_data_test = data.frame(cpi_u2,avg_hs_pr2,retail2,pop2,pop_diff2,home2)
colnames(new_data_test)[colnames(new_data_test)=="cpi_u2"] = "cpi_u1"
colnames(new_data_test)[colnames(new_data_test)=="avg_hs_pr2"] = "avg_hs_pr1"
colnames(new_data_test)[colnames(new_data_test)=="retail2"] = "retail1"
colnames(new_data_test)[colnames(new_data_test)=="pop2"] = "pop1"
colnames(new_data_test)[colnames(new_data_test)=="pop_diff2"] = "pop_diff1"
colnames(new_data_test)[colnames(new_data_test)=="home2"] = "home1"
y_train = Datathon_Trainset$Sales_MM
print(new_data_test)

# Final Chosen MULTIVARIATE POLYNOMIAL REGRESSIVE MODEL:
model7 = lm(sales ~ polym(home1,retail1,pop1,cpi_u1,degree=2,raw=TRUE),data=train_data)

# Checking model validity through p-value, F-test, AIC and significance of coefficients:
summary(model7)
accuracy(model7)

# Testing the model on the 'Training Dataset' and the 'Test Dataset':
p_train = predict(model7,train_data)
p_test = predict(model7,new_data_test)

#Computing the Root Mean Square Error:
diff3 = (y - p_test)
rmse(diff3)

# Plotting predicted Training dataset against ground-truth:
plot(p_train,type="l",col="red")
lines(y_train,col="green")

# Plotting predicted Testing dataset against ground-truth:
plot(p_test,type="l",col="red")
lines(y,col="green")

# Final RMSE: 684.6295.


