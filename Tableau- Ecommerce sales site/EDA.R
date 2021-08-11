#Name- Anamika Singh
#BANA 6760 E01- Data Visualization
#Assignment-10
#Exploratory Data Analysis with Project Data

# set working directory
getwd()
setwd("C:/Users/anami/Desktop/MS BA/AaSpring 2021/Data Visualization/Assignments/A10/Dataset")

# To load excel data into R
orders<- read.csv("Orders.csv")
od<-read.csv("Order Details.csv")
sales<-read.csv("Sales Target.csv")

# Basic structure of the dataset
names(orders);names(od);names(sales)
class(orders); class(od); class(sales)
str(orders); str(od); str(sales)
summary(orders)
sum(is.na(orders))
summary(od)
summary(sales)

# Joining orders and od
mergedata<- merge(orders,od,by="Order.ID")
write.csv(mergedata,"C:/Users/anami/Desktop/MS BA/AaSpring 2021/Data Visualization/Assignments/A10/Dataset\\OD.csv", row.names = FALSE)


############################ Descriptive Statistics #########################

# change the column name Amount to Sale
colnames(mergedata)[6]<-"Sales"

# To see the detail summary of the dataset
summary(mergedata)

# Create a function to find descriptive stats of the dataset
dataset <- function(x)
  return(list(Mean=mean(x), Median=median(x), Variance=var(x), 
              StandardDeviation=sd(x), Range= range(x), IQR= IQR(x)))
Sales_stats<-dataset(mergedata$Sales)
Sales_stats

profit_stats<-dataset(mergedata$Profit)
profit_stats

# To see the distribution of the dataset
install.packages("moments")
library(moments)

# sale variable
skewness(mergedata$Sales)
kurtosis(mergedata$Sales)

# Profit Variable
skewness(mergedata$Profit)
kurtosis(mergedata$Profit)


############################### Histogram ##################################

install.packages(ggplot2)
library(ggplot2)

# Sales 
ggplot(mergedata, aes(x=Sales)) +geom_histogram(binwidth = 5)+labs(title="Sales Histogram")

# Profit
ggplot(mergedata, aes(x=Profit)) +geom_histogram(binwidth = 4)+labs(title="Profit Histogram")


############################# Bar graphs #################################

# State on x axis
ggplot(mergedata, aes(x=factor(State))) + 
  geom_bar(fill="blue", color="black", width=0.6)+ xlab("State")

# City on x axis
ggplot(mergedata, aes(x=factor(City))) + 
  geom_bar(fill="brown", width=0.5) + xlab("City")

# Category on x axis
ggplot(mergedata, aes(x=factor(Category))) + 
  geom_bar(fill="red", width=0.5) + xlab("Category")+ labs(title = "Product Category")

# Sub Category on x axis
ggplot(mergedata, aes(x=factor(Sub.Category))) +
  geom_bar(fill="blue") + xlab("Sub Category")+ coord_flip() + labs(title = "Product Sub-Category")

# Sales for all States filled with Category
ggplot(mergedata, aes(x=State,y=Sales, fill=Category)) +  
  geom_bar(stat="identity",position="dodge") + labs(title="Sales by Category", y="Sales") + 
  scale_fill_brewer(palette="Pastel2") +theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Profit by Category
cat_data<-mergedata %>% group_by(Sub.Category) %>% summarize(Profit=sum(Profit))
ggplot(cat_data,aes(x=Sub.Category,y=Profit)) +  
  geom_bar(stat="identity",fill="blue") + labs(title="Profit by Sub Category", y="Profit")
  +theme(axis.text.x = element_text(angle = 60, hjust = 1))


############################### Box plot ## ##############################

# sales 
boxplot(mergedata$Sales, ylab= "Sales", main="Boxplot Sales")

# Profit
boxplot(mergedata$Profit, ylab= "Profit", main="Boxplot Profit")

# Sales Vs Product Category
boxplot(Sales~factor(Category), data=mergedata, xlab="Category",
        ylab= "Sales", main="Sales VS Product Category")

# Profit Vs Product Category
boxplot(Profit~factor(Category), data= mergedata, xlab="Category",
        ylab= "Profit", main="Profit VS Product Category")


################################ Line graphs #############################

install.packages("dplyr")
library(dplyr)

# Quantity by State
data<-mergedata %>% group_by(State) %>% summarize(Quantity=sum(Quantity))
ggplot(data, aes(x=factor(State), y=Quantity,group=1))+
  geom_line(color="red", size=1)+ geom_point(color="black", size=2, shape=22,fill="black") +
  labs(title="Order Quantity by State", x="State")

# Sales by State
sales_data<-mergedata %>% group_by(State) %>% summarize(Sales=sum(Sales))
ggplot(sales_data, aes(x=factor(State),y=Sales,group=1)) + 
  geom_line(color="black", size=1)+ geom_point(color="blue", size=2, shape=23, fill="blue") +
  labs(title="Sales by State", x="State")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Profit by State
Profit_data<-mergedata %>% group_by(State) %>% summarize(Profit=sum(Profit))
ggplot(Profit_data, aes(x=factor(State),y=Profit,group=1)) + 
  geom_line(color="blue", size=1)+ geom_point(color="red", size=2, shape=21, fill="red") +
  labs(title="Profit by State", x="State")+theme(axis.text.x = element_text(angle = 60, hjust = 1))


############################# Scatter plots ################################

# Plot between profit and sales
ggplot(mergedata, aes(x=Sales, y=Profit))+geom_point(alpha=0.1, color="blue")+
  labs(title="Plot between Sales and Profit")

cor(mergedata$Sales, mergedata$Profit)

# Plot between Quantity and sales
ggplot(mergedata, aes(x=Quantity,y=Sales))+geom_point(alpha=0.1, color="black")+
  labs(title="Plot between Sales and Order Quantity")

# Plot between Quantity and Profit
ggplot(mergedata, aes(x=Quantity,y=Profit))+geom_point(alpha=0.1, color="black")+
  labs(title="Plot between Profit and Order Quantity")


######################### Time series Plot ################################

install.packages("lubridate")
library(lubridate)

mergedata$Order.Date<-parse_date_time(x = mergedata$Order.Date,
                orders = c("d m y", "d B Y", "m/d/y"),
                locale = "eng")
mergedata$Order.Date

# Sales per year by product category
mergedata$year <- floor_date(mergedata$Order.Date, "year")
mergedata$month<-floor_date(mergedata$Order.Date, "month")
ydata<-mergedata %>% group_by(month,Category ) %>% summarize(sum=sum(Sales))
ydata

ggplot(ydata, aes(x=month,y=sum, color=Category)) +  
  geom_line(size=1) + geom_point(color="black", size=4, shape=24,fill="red")+
  labs(title="Sales per month", y="Sales") +
  scale_fill_brewer(palette="Pastel1")

# Profit per year (order date) filled with product category
pdata<-mergedata %>% group_by(month,Category) %>% summarize(sum=sum(Profit))
pdata

ggplot(pdata, aes(x=month,y=sum, fill=Category)) +  
  geom_bar(stat="identity") + scale_fill_brewer(palette="Set1") +
  labs(title="Profit per year", y="Profit")



