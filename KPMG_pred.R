#loading Packages
install.packages("tidyverse")
install.packages("ggplotly")
install.packages("readxl")
install.packages("dlookr")
install.packages("ggcorrplot")
install.packages("janitor")
install.packages("lubridate")
install.packages("forecast")

#Loading Data
library(readxl)
customer_transaction <- read_excel('C:/Users/tanma/Downloads/KPMG_final_data.xlsx')
customer_transaction
new_cust <- read_excel('C:/Users/tanma/Downloads/KPMG_final_data.xlsx', sheet = 2)
new_cust
cust_demo <- read_excel('C:/Users/tanma/Downloads/KPMG_final_data.xlsx', sheet = 3)
cust_demo
cust_address<- read_excel('C:/Users/tanma/Downloads/KPMG_final_data.xlsx', sheet = 4)
cust_address



#Diagnosing data quality
library(dlookr)
diagnose(customer_transaction)
diagnose(new_cust)
diagnose(cust_address)
diagnose(cust_demo)


# Diagnosing Numeric data quality
diagnose_numeric(customer_transaction)
diagnose_numeric(new_cust)
diagnose_numeric(cust_address)
diagnose_numeric(cust_demo)


diagnose_category(customer_transaction)

# Diagnosing data if having outliers
diagnose_outlier(customer_transaction)
diagnose_outlier(new_cust)
diagnose_outlier(cust_address)
diagnose_outlier(cust_demo)

library(tidyverse)
#plotting Outliers
customer_transaction %>% plot_outlier(standard_cost)


#Finding Correlation among variables
library(ggcorrplot)
customer_transaction_1 <- data.frame(customer_transaction$transaction_id, customer_transaction$product_id, customer_transaction$customer_id, customer_transaction$list_price, customer_transaction$standard_cost, customer_transaction$product_first_sold_date)
names(customer_transaction_1) <- c("transaction_id", "product_id", "customer_id", "list_price", "standard_cost", "product_first_sold_date")
customer_transaction_1 <- na.omit(customer_transaction_1)
corr <- round(cor(customer_transaction_1), 1)
ggcorrplot(corr, hc.order = TRUE,type = "lower",lab = TRUE,
           lab_size = 3, method="circle",
           colors = c("#6D9EC1", "white", "#E46726"),
           title="Correlogram of Customer Transaction",
           ggtheme=ggplot2::theme_gray)+ theme (legend.position="bottom")


# Merging Customer Data 
#customer_transaction %>% diagnose_report(output_format = "html")
transaction_data <- merge(customer_transaction, cust_demo, by = "customer_id")
transaction_data
transaction_data <- merge(transaction_data, cust_address, by = "customer_id")
transaction_data

## Data Wrangling
# Cleansing Data
transaction_data$DOB <- as.integer(transaction_data$DOB)
library(janitor)
transaction_data$DOB<-excel_numeric_to_date(transaction_data$DOB)

library(lubridate)
transaction_data["Age"] <- NA
calc_age <- function(birthDate, refDate = Sys.Date()) {
  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  period$year
} 



transaction_data$Age <- calc_age(transaction_data$DOB)



new_cust <- na.omit(new_cust)

new_cust["Age"] <- NA
calc_age <- function(birthDate, refDate = Sys.Date()) {
  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  period$year
} 

new_cust$Age <- calc_age(new_cust$DOB)
new_cust$gender[new_cust$gender == 'F']<- 'Female'
new_cust$gender[new_cust$gender == 'M']<- 'Male'
new_cust$gender[new_cust$gender == 'Femal']<- 'Female'


#transaction_data <- na.omit(transaction_data)
transaction_data$gender[transaction_data$gender == 'F']<- 'Female'
transaction_data$gender[transaction_data$gender == 'M']<- 'Male'
transaction_data$gender[transaction_data$gender == 'Femal']<- 'Female'


transaction_data_1 <- data.frame(transaction_data$gender, transaction_data$past_3_years_bike_related_purchases)
names(transaction_data_1) <- c("gender", "past_3_years_bike_related_purchases")



## Data Exploration Customer Transaction

library(ggplot2)
g_age_overall <- ggplot(transaction_data, aes(transaction_data$Age, transaction_data$past_3_years_bike_related_purchases))
g_age_overall + 
  geom_point(aes(color=transaction_data$gender)) + geom_smooth(color="darkblue") + 
  ggtitle("Distribution between Age and Bike Purchase")  + labs(x = "Age" , y = "Bike purchased in last 3 years", color = "Gender") +  theme (legend.position="bottom")


hist(transaction_data$Age)
plot(density(transaction_data$Age))

library(dplyr)
transaction_data_2 <- data.frame(transaction_data$wealth_segment, transaction_data$past_3_years_bike_related_purchases)
names(transaction_data_2) <- c("wealth_segment", "past_3_years_bike_related_purchases")
Wealth <- transaction_data_2%>%group_by(wealth_segment) %>% summarize_all(sum)%>% ungroup()
ggplot(Wealth, aes(wealth_segment,past_3_years_bike_related_purchases,fill= "amber")) + geom_bar(stat="identity") + geom_text(aes(wealth_segment, past_3_years_bike_related_purchases, label = past_3_years_bike_related_purchases), size = 3.0, hjust = 0.2, vjust = -0.1)

library(plotly)
transaction_data_3 <- data.frame(transaction_data$job_industry_category, transaction_data$past_3_years_bike_related_purchases)
names(transaction_data_3) <- c("job_industry_category", "past_3_years_bike_related_purchases")
job_industry <- transaction_data_3%>% group_by(job_industry_category) %>% summarize_all(sum)%>% ungroup()
job_category <- ggplot(job_industry, aes(reorder(job_industry_category,past_3_years_bike_related_purchases),past_3_years_bike_related_purchases)) + geom_bar(stat="identity", fill = "blue") + theme (legend.position="bottom", axis.text.x = element_text(angle = 45, vjust=0.4, hjust=0.7))
ggplotly(job_category, tooltip = c("past_3_years_bike_related_purchases") )%>% layout(legend = list(orientation = 'h', x = 0.0, y = -0.25))


library(plotly)
transaction_data_4 <- data.frame(transaction_data$state, transaction_data$past_3_years_bike_related_purchases)
names(transaction_data_4) <- c("state", "past_3_years_bike_related_purchases")
state <- transaction_data_4%>% group_by(state) %>% summarize_all(sum)%>% ungroup()
State_plot <- ggplot(state, aes(reorder(state,past_3_years_bike_related_purchases),past_3_years_bike_related_purchases)) + geom_bar(stat="identity", fill = "Steelblue") + theme (legend.position="bottom", axis.text.x = element_text(angle = 45, vjust=0.4, hjust=0.7))
ggplotly(State_plot, tooltip = c("past_3_years_bike_related_purchases") )%>% layout(legend = list(orientation = 'h', x = 0.0, y = -0.25))


gender <- transaction_data_1%>% group_by(gender) %>% summarize_all(sum)%>% ungroup()
ggplot(gender, aes(gender,past_3_years_bike_related_purchases)) + geom_bar(stat="identity", fill = "Orange") + theme (legend.position="bottom", axis.text.x = element_text(angle = 45, vjust=0.4, hjust=0.7))


transaction_data_5 <- data.frame(transaction_data$owns_car, transaction_data$past_3_years_bike_related_purchases)
names(transaction_data_5) <- c("owns_car", "past_3_years_bike_related_purchases")
car_owned <- transaction_data_5%>% group_by(owns_car) %>% summarize_all(sum)%>% ungroup()
ggplot(car_owned, aes(owns_car,past_3_years_bike_related_purchases)) + geom_bar(stat="identity", fill = "dark grey") + theme (legend.position="bottom", axis.text.x = element_text(angle = 45, vjust=0.4, hjust=0.7)) + geom_text(aes(owns_car, past_3_years_bike_related_purchases, label = past_3_years_bike_related_purchases), size = 3.0, hjust = 0.2, vjust = -0.2)

'''
brand <- data.frame(transaction_data$brand)
brand["Count"] <- 1
names(brand) <- c("brand","Count")
brand_count <- brand%>% group_by(brand) %>% summarize_all(sum)%>% ungroup()


ggplot(brand_count)+ 
  aes(reorder(brand, Count),Count) + 
  geom_bar(stat = "identity", fill = "cyan", color = "white")+
  geom_text(aes(brand, Count, label = Count), size = 3.0, hjust = -0.1)+ 
  coord_flip()+ 
  xlab ("Brand") + theme_dark()
'''

## Data Exploration new Customers

library(ggplot2)
new_age_overall <- ggplot(new_cust, aes(Age,past_3_years_bike_related_purchases))
new_age_overall + 
  geom_point(aes(color=gender)) + geom_smooth(color="darkblue") + 
  ggtitle("Distribution between Age and Bike Purchase")  + labs(x = "Age" , y = "Bike purchased in last 3 years", color = "Gender") +  theme (legend.position="bottom")



hist(new_cust$Age)



new_cust_2 <- data.frame(new_cust$wealth_segment, new_cust$past_3_years_bike_related_purchases)
names(new_cust_2) <- c("wealth_segment", "past_3_years_bike_related_purchases")
Wealth <- new_cust_2%>% group_by(wealth_segment) %>% summarize_all(sum)%>% ungroup()
ggplot(Wealth, aes(wealth_segment,past_3_years_bike_related_purchases,fill= "amber")) + geom_bar(stat="identity") + geom_text(aes(wealth_segment, past_3_years_bike_related_purchases, label = past_3_years_bike_related_purchases), size = 3.0, hjust = 0.2, vjust = -0.1)



library(plotly)
new_cust_3 <- data.frame(new_cust$job_industry_category, new_cust$past_3_years_bike_related_purchases)
names(new_cust_3) <- c("job_industry_category", "past_3_years_bike_related_purchases")
job_industry <- new_cust_3%>% group_by(job_industry_category) %>% summarize_all(sum)%>% ungroup()
job_category <- ggplot(job_industry, aes(reorder(job_industry_category,past_3_years_bike_related_purchases),past_3_years_bike_related_purchases)) + geom_bar(stat="identity", fill = "blue") + theme (legend.position="bottom", axis.text.x = element_text(angle = 45, vjust=0.4, hjust=0.7))
ggplotly(job_category, tooltip = c("past_3_years_bike_related_purchases") )%>% layout(legend = list(orientation = 'h', x = 0.0, y = -0.25))




library(plotly)
new_cust_4 <- data.frame(new_cust$state, new_cust$past_3_years_bike_related_purchases)
names(new_cust_4) <- c("state", "past_3_years_bike_related_purchases")
state <- new_cust_4%>% group_by(state) %>% summarize_all(sum)%>% ungroup()
State_plot <- ggplot(state, aes(reorder(state,past_3_years_bike_related_purchases),past_3_years_bike_related_purchases)) + geom_bar(stat="identity", fill = "Steelblue") + theme (legend.position="bottom", axis.text.x = element_text(angle = 45, vjust=0.4, hjust=0.7))
ggplotly(State_plot, tooltip = c("past_3_years_bike_related_purchases") )%>% layout(legend = list(orientation = 'h', x = 0.0, y = -0.25))



new_cust_1 <- data.frame(new_cust$gender, new_cust$past_3_years_bike_related_purchases)
names(new_cust_1) <- c("gender", "past_3_years_bike_related_purchases")
gender <- new_cust_1%>% group_by(gender) %>% summarize_all(sum)%>% ungroup()
ggplot(gender, aes(gender,past_3_years_bike_related_purchases)) + geom_bar(stat="identity", fill = "Orange") + theme (legend.position="bottom", axis.text.x = element_text(angle = 45, vjust=0.4, hjust=0.7)) + geom_text(aes(gender, past_3_years_bike_related_purchases, label = past_3_years_bike_related_purchases), size = 3.0, hjust = 0.2, vjust = -0.1)



## Model


new_cust$past_3_years_bike_related_purchases <- as.integer(new_cust$past_3_years_bike_related_purchases)
new_cust$property_valuation <- as.integer(new_cust$property_valuation)
new_cust$postcode <- as.integer(new_cust$postcode)


### Decision Tree

library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
transaction_data$brand <- as.factor(transaction_data$brand)
#dataset splitting
new_cust_valid <- data.frame(new_cust$first_name, new_cust$last_name, new_cust$gender, new_cust$past_3_years_bike_related_purchases, new_cust$DOB, new_cust$job_title,  new_cust$job_industry_category , new_cust$owns_car , new_cust$tenure , new_cust$address , new_cust$postcode, new_cust$state, new_cust$country, new_cust$property_valuation, new_cust$Age)
transaction_data_train <- data.frame(transaction_data$first_name, transaction_data$last_name, transaction_data$gender, transaction_data$past_3_years_bike_related_purchases, transaction_data$DOB, transaction_data$job_title,  transaction_data$job_industry_category , transaction_data$owns_car , transaction_data$tenure , transaction_data$address , transaction_data$postcode, transaction_data$state, transaction_data$country, transaction_data$property_valuation, transaction_data$Age)
names(new_cust_valid) <- c("first_name","last_name", "gender", "past_3_years_bike_related_purchases" , "DOB","job_title" , "job_industry_category", "owns_car", "tenure", "address", "postcode","state", "country", "property_valuation", "Age")
names(transaction_data_train) <- c("first_name","last_name", "gender", "past_3_years_bike_related_purchases" , "DOB","job_title" , "job_industry_category", "owns_car", "tenure", "address", "postcode","state", "country", "property_valuation", "Age")
train <- transaction_data_train
valid <- new_cust_valid
#applying the classification tree algorithm
class_tree <- rpart( gender ~ state  + job_industry_category  + property_valuation  + postcode  + past_3_years_bike_related_purchases , data=train, method="class", 
                     control=rpart.control(minbucket=50, maxdepth=7))
#looking at the classification 
printcp(class_tree)
tree <- prp(class_tree, type=1, extra=1, under=TRUE, split.font=2, varlen=-12, box.col="orange")
# Confusion matrix with accuracy of model
class.treepred <- predict(class_tree, valid, type = 'class')
class.treepred1 <- predict(class_tree, valid, type = 'prob')
table(class.treepred, valid$gender)
confusionMatrix(class.treepred, as.factor(valid$gender))



sapply(transaction_data, mode)
sapply(new_cust, mode)


## Linear Regression


transaction_data$gender <- as.character(transaction_data$gender)
transaction_data$Age <- as.integer(transaction_data$Age)
transaction_data$customer_id <- as.integer(transaction_data$customer_id)
# Applying Multivariate linear regression
linearreg <- lm( Age  ~ past_3_years_bike_related_purchases + gender  + job_industry_category + wealth_segment + owns_car + postcode , data = transaction_data)
summary(linearreg)
# Plotting the QQ Plot
par(mfrow = c(2, 2))
plot(linearreg)
# Predicting the accuracy
library(forecast)
Modelpred <- predict(linearreg, new_cust)
accuracy(Modelpred, new_cust$Age)



write.csv(transaction_data,"transactions.csv", row.names = FALSE)
write.csv(new_cust,"new_customer.csv", row.names = FALSE)