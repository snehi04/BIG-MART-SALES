"library"

library(ggplot2)
library(caTools)
library(corrplot)
library(rpart)
library(Metrics)



"splitting data in to train 75% and test 25%"

sample = sample.split(Train, SplitRatio = .75)
train_data = subset(Train, sample == TRUE)
test_data  = subset(Train, sample == FALSE)
dim(train_data)
dim(test_data)

td1<-train_data
#---univaraite analysis---#

#---Item_Identifier---#
qplot(td1$Item_Identifier,main = toupper(" Item_Identifier "),col = "blue")

#---Item_Weight---#
qplot(td1$Item_Weight,main = toupper(" Item_Weight "), col = "blue")

#---Item_Fat_Content---#
qplot(td1$Item_Fat_Content,main = toupper(" Item_Fat_Content "), col = "blue")

#---Item_Visibility---#
qplot(td1$Item_Visibility,main = toupper(" Item_Visibility "), col = "blue")

#---Item_Type---#
qplot(td1$Item_Type,main = toupper(" Item_Type "), col = "blue")

#---Item_MRP---#
qplot(td1$Item_MRP,main = toupper(" Item_MRP "), col = "blue")

#---Outlet_Identifier---#
qplot(td1$Outlet_Identifier,main = toupper(" Outlet_Identifier "), col = "blue")

#---Outlet_Establishment_Year---#
qplot(td1$Outlet_Establishment_Year,main = toupper(" Outlet_Establishment_Year "), col = "blue")

#---Outlet_Size---#
qplot(td1$Outlet_Size,main = toupper(" Outlet_Size "), col = "blue")

#---Outlet_Location_Type---#
qplot(td1$Outlet_Location_Type,main = toupper(" Outlet_Location_Type "), col = "blue")

#---Outlet_Type---#
qplot(td1$Outlet_Type,main = toupper(" Outlet_Type "), col = "blue")

#---Item_Outlet_Sales---#
qplot(aa,main = toupper(" Item_Outlet_Sales "), col = "blue")

aa<-log(td1$Item_Outlet_Sales)

#missing value treatment in item weight
colSums(is.na(train_data))
td1<-train_data
summary(train_data)
summary(td1$Item_Weight)
td1$Item_Weight[which(is.na(td1$Item_Weight))]<- 12.500
table(is.na(td1$Item_Weight))
summary(td1)

#outlier for item_weight
ggplot(td1,aes(x=Item_Outlet_Sales,y=Item_Weight))+geom_boxplot()

#outlier for item_visibility
ggplot(td1,aes(x=Item_Outlet_Sales,y=Item_Visibility))+geom_boxplot()

summary(td1$Item_Visibility)
td1_out_lie<-subset(td1,Item_Visibility <0.18)
ggplot(td1_out_lie,aes(x=Item_Outlet_Sales,y=Item_Visibility))+geom_boxplot()

#oulier for Item_MRP
ggplot(td1,aes(x=Item_Outlet_Sales,y=Item_MRP))+geom_boxplot()


#outlier for Item_outlet_sales
boxplot(td1$Item_Outlet_Sales)

summary(td1$Item_Outlet_Sales)
quantile(td1$Item_Outlet_Sales,c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,1))

td1_out_lie_1<-subset(td1,Item_Outlet_Sales <5500)
boxplot(td1_out_lie_1$Item_Outlet_Sales)


ggplot(td1_out_lie_1, aes(Outlet_Identifier, Item_Outlet_Sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Item_Outlet_Sales") + 
  ggtitle("Item_Outlet_Sales vs Outlet identifier")

ggplot(td1_out_lie_1, aes(Outlet_Type, Item_Outlet_Sales,fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Type") + 
  ylab(" Item_Outlet_Sales") + 
  ggtitle("Item_Outlet_Sales vs Outlet Outlet_Type")

ggplot(td1_out_lie_1, aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")




barplot(table(td1$Outlet_Size,td1$Outlet_Type))
#missing value treatment for outlet size
summary(train_data)
summary(td1$Outlet_Size)
td1$Outlet_Size[which(is.na(td1$Outlet_Size))]<- 'small'
colSums(is.na(td1))
colSums(is.na(train_data))

#replace of '0' in Item_visibility

med_value <- median(td1$Item_Visibility)
td1$Item_Visibility[td1$Item_Visibility == 0] <- med_value


#corrplot
nums <- sapply(td1, is.numeric)
train_nums <- td1[ , nums]
M <- cor(train_nums)
corrplot(M, method="number")


test1<-test_data

# Outlet_Size's NA values replaced with Small

test1$Outlet_Size[test1$Outlet_Size == ""] <- "Small" 

# Item_Weight's NA values replaced with median value
med_value <- median(train_data$Item_Weight,na.rm = TRUE)

test1$Item_Weight[is.na(test1$Item_Weight) == TRUE] <- med_value

# Item_Visibility's NA values replaced with median value
med_value <- median(train_data$Item_Visibility)


test1$Item_Visibility[test1$Item_Visibility == 0] <- med_value

#names(train_data)

#train1 <- subset(train1, select = -Item_Identifier)
test2 <- subset( test1, select = -c(Item_Outlet_Sales,Item_Identifier) )
test_output <- test1$Item_Outlet_Sales

#View(test2)  
test2$Outlet_Size[which(is.na(test2$Outlet_Size))]<- 'small'
colSums(is.na(test2))

summary(test2)

fitLm <- lm(log(Item_Outlet_Sales) ~ Item_Weight 
            + Item_Fat_Content 
            + Item_Visibility 
            + Item_Type 
            + Item_MRP
            + Outlet_Identifier
            + Outlet_Establishment_Year + Outlet_Size + Outlet_Location_Type + Outlet_Type, data = td1)


preds <- predict(fitLm, test2)

predicated <- exp(preds)

rmse(test_output,predicated)

# -----  Original test -------

org_test <- read.csv("C:\\Users\\venka\\Documents\\poc_4\\Big Mart Sales\\Test.csv")

org_test$Outlet_Size[org_test$Outlet_Size == ""] <- "Small" 
med_value <- median(org_test$Item_Weight,na.rm = TRUE)
org_test$Item_Weight[is.na(org_test$Item_Weight) == TRUE] <- med_value
med_value <- median(org_test$Item_Visibility)
org_test$Item_Visibility[org_test$Item_Visibility == 0] <- med_value


preds1 <- predict(fitLm, org_test)

submit <- data.frame(Item_Identifier = org_test$Item_Identifier, Outlet_Identifier = org_test$Outlet_Identifier,Item_Outlet_Sales = exp(preds1))
write.csv(submit, file = "sub_lmm.csv", row.names = FALSE)

