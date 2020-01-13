
#import data file
data<-read.csv("F:/Data Science/Week4/Property_Price_Train.csv")

#initial assessment
str(data)
#View(data)
dim(data)
summary(data)

#checking for null values
library(naniar)
vis_miss(data)

#eliminating rows and columns with null values
d<-na.omit(data)
dim(d)  #(0,81), elimination not recommended


colSums(is.na(data))

library(psych)
library("car")

#pre-impute analysis of continuous variables
which(is.na(data$Lot_Extent))
sum(is.na(data$Lot_Extent)) 
describe(data$Lot_Extent)
hist(data$Lot_Extent, main = "Pre-impute analysis")

#impute median for null values
data$Lot_Extent[is.na(data$Lot_Extent)] <- median(data$Lot_Extent, na.rm = TRUE)

#post-impute analysis of continuous variables
which(is.na(data$Lot_Extent))
sum(is.na(data$Lot_Extent)) 
describe(data$Lot_Extent)
hist(data$Lot_Extent, main = "Post-impute analysis")

cor.test(data$Sale_Price,data$Lot_Extent) 
plot(data$Lot_Extent,data$Sale_Price,col = "red", lwd = 4)
reg1 = lm(data$Sale_Price~data$Lot_Extent)
reg1
abline(reg1, lwd = 2, col = "blue")
scatterplot(data$Sale_Price~data$Lot_Extent,data = data, col = "red")

#pre-impute analysis of continuous variables
which(is.na(data$Brick_Veneer_Area))
sum(is.na(data$Brick_Veneer_Area)) 
describe(data$Brick_Veneer_Area)
hist(data$Brick_Veneer_Area, main = "Pre-impute analysis")

#imputation for continuous variables
data$Brick_Veneer_Area[is.na(data$Brick_Veneer_Area)] <- median(data$Brick_Veneer_Area, na.rm = TRUE)

#post-impute analysis of continuous variables
which(is.na(data$Brick_Veneer_Area))
sum(is.na(data$Brick_Veneer_Area)) 
describe(data$Brick_Veneer_Area)
hist(data$Brick_Veneer_Area, main = "Post-impute analysis")

cor.test(data$Sale_Price,data$Brick_Veneer_Area) 
plot(data$Brick_Veneer_Area,data$Sale_Price,col = "red", lwd = 4)
reg2 = lm(data$Sale_Price~data$Brick_Veneer_Area)
reg2
abline(reg2, lwd = 2, col = "blue")
scatterplot(data$Sale_Price~data$Brick_Veneer_Area,data = data, col = "red")

#pre-impute analysis of categorical variables
str(data$Brick_Veneer_Type)
summary(data$Brick_Veneer_Type)
which(is.na(data$Brick_Veneer_Type))
sum(is.na(data$Brick_Veneer_Type))
t1 = table(data$Brick_Veneer_Type)
t1 
sum(t1)
barplot(t1)
mode(data$Brick_Veneer_Type)

#imputation for categorical variables
data$Brick_Veneer_Type[is.na(data$Brick_Veneer_Type)] <- "None"

#post-impute analysis of categorical variables
str(data$Brick_Veneer_Type)
summary(data$Brick_Veneer_Type)
which(is.na(data$Brick_Veneer_Type))
sum(is.na(data$Brick_Veneer_Type))
t1 = table(data$Brick_Veneer_Type)
t1 
sum(t1)
barplot(t1)
mode(data$Brick_Veneer_Type)

summary(aov(data$Sale_Price ~ data$Brick_Veneer_Type))
TukeyHSD(aov(data$Sale_Price ~ data$Brick_Veneer_Type))

#pre-impute analysis of categorical variables
str(data$Basement_Height)
summary(data$Basement_Height)
which(is.na(data$Basement_Height))
sum(is.na(data$Basement_Height))
t1 = table(data$Basement_Height)
t1 
sum(t1)
barplot(t1)
mode(data$Basement_Height)

#imputation for categorical variables
data$Basement_Height[is.na(data$Basement_Height)] <- "TA"

#post-impute analysis of categorical variables
str(data$Basement_Height)
summary(data$Basement_Height)
which(is.na(data$Basement_Height))
sum(is.na(data$Basement_Height))
t1 = table(data$Basement_Height)
t1 
sum(t1)
barplot(t1)
mode(data$Basement_Height)

summary(aov(data$Sale_Price ~ data$Basement_Height))
TukeyHSD(aov(data$Sale_Price ~ data$Basement_Height))

#pre-impute analysis of categorical variables
str(data$Basement_Condition)
summary(data$Basement_Condition)
which(is.na(data$Basement_Condition))
sum(is.na(data$Basement_Condition))
t1 = table(data$Basement_Condition)
t1 
sum(t1)
barplot(t1)
mode(data$Basement_Condition)

#imputation for categorical variables
data$Basement_Condition[is.na(data$Basement_Condition)] <- "TA"

#post-impute analysis of categorical variables
str(data$Basement_Condition)
summary(data$Basement_Condition)
which(is.na(data$Basement_Condition))
sum(is.na(data$Basement_Condition))
t1 = table(data$Basement_Condition)
t1 
sum(t1)
barplot(t1)
mode(data$Basement_Condition)

summary(aov(data$Sale_Price ~ data$Basement_Condition))
TukeyHSD(aov(data$Sale_Price ~ data$Basement_Condition))

#pre-impute analysis of categorical variables
str(data$Exposure_Level)
summary(data$Exposure_Level)
which(is.na(data$Exposure_Level))
sum(is.na(data$Exposure_Level))
t1 = table(data$Exposure_Level)
t1 
sum(t1)
barplot(t1)
mode(data$Exposure_Level)

#imputation for categorical variables
data$Exposure_Level[is.na(data$Exposure_Level)] <- "No"

#post-impute analysis of categorical variables
str(data$Exposure_Level)
summary(data$Exposure_Level)
which(is.na(data$Exposure_Level))
sum(is.na(data$Exposure_Level))
t1 = table(data$Exposure_Level)
t1 
sum(t1)
barplot(t1)
mode(data$Exposure_Level)

summary(aov(data$Sale_Price ~ data$Exposure_Level))
TukeyHSD(aov(data$Sale_Price ~ data$Exposure_Level))

#pre-impute analysis of categorical variables
str(data$BsmtFinType1)
summary(data$BsmtFinType1)
which(is.na(data$BsmtFinType1))
sum(is.na(data$BsmtFinType1))
t1 = table(data$BsmtFinType1)
t1 
sum(t1)
barplot(t1)
mode(data$BsmtFinType1)

#imputation for categorical variables
data$BsmtFinType1[is.na(data$BsmtFinType1)] <- "Unf"

#post-impute analysis of categorical variables
str(data$BsmtFinType1)
summary(data$BsmtFinType1)
which(is.na(data$BsmtFinType1))
sum(is.na(data$BsmtFinType1))
t1 = table(data$BsmtFinType1)
t1 
sum(t1)
barplot(t1)
mode(data$BsmtFinType1)

summary(aov(data$Sale_Price ~ data$BsmtFinType1))
TukeyHSD(aov(data$Sale_Price ~ data$BsmtFinType1))

#pre-impute analysis of categorical variables
str(data$BsmtFinType2)
summary(data$BsmtFinType2)
which(is.na(data$BsmtFinType2))
sum(is.na(data$BsmtFinType2))
t1 = table(data$BsmtFinType2)
t1 
sum(t1)
barplot(t1)
mode(data$BsmtFinType2)

#imputation for categorical variables
data$BsmtFinType2[is.na(data$BsmtFinType2)] <- "Unf"

#post-impute analysis of categorical variables
str(data$BsmtFinType2)
summary(data$BsmtFinType2)
which(is.na(data$BsmtFinType2))
sum(is.na(data$BsmtFinType2))
t1 = table(data$BsmtFinType2)
t1 
sum(t1)
barplot(t1)
mode(data$BsmtFinType2)

summary(aov(data$Sale_Price ~ data$BsmtFinType2))
TukeyHSD(aov(data$Sale_Price ~ data$BsmtFinType2))

#pre-impute analysis of categorical variables
str(data$Electrical_System)
summary(data$Electrical_System)
which(is.na(data$Electrical_System))
sum(is.na(data$Electrical_System))
t1 = table(data$Electrical_System)
t1 
sum(t1)
barplot(t1)
mode(data$Electrical_System)

#imputation for categorical variables
data$Electrical_System[is.na(data$Electrical_System)] <- "SBrkr"

#post-impute analysis of categorical variables
str(data$Electrical_System)
summary(data$Electrical_System)
which(is.na(data$Electrical_System))
sum(is.na(data$Electrical_System))
t1 = table(data$Electrical_System)
t1 
sum(t1)
barplot(t1)
mode(data$Electrical_System)

summary(aov(data$Sale_Price ~ data$Electrical_System))
TukeyHSD(aov(data$Sale_Price ~ data$Electrical_System))

#pre-impute analysis of categorical variables
str(data$Garage)
summary(data$Garage)
which(is.na(data$Garage))
sum(is.na(data$Garage))
t1 = table(data$Garage)
t1 
sum(t1)
barplot(t1)
mode(data$Garage)

#imputation for categorical variables
data$Garage[is.na(data$Garage)] <- "Attchd"

#post-impute analysis of categorical variables
str(data$Garage)
summary(data$Garage)
which(is.na(data$Garage))
sum(is.na(data$Garage))
t1 = table(data$Garage)
t1 
sum(t1)
barplot(t1)
mode(data$Garage)

summary(aov(data$Sale_Price ~ data$Garage))
TukeyHSD(aov(data$Sale_Price ~ data$Garage))

#pre-impute analysis of categorical variables
str(data$Garage_Quality)
summary(data$Garage_Quality)
which(is.na(data$Garage_Quality))
sum(is.na(data$Garage_Quality))
t1 = table(data$Garage_Quality)
t1 
sum(t1)
barplot(t1)
mode(data$Garage_Quality)

#imputation for categorical variables
data$Garage_Quality[is.na(data$Garage_Quality)] <- "TA"

#post-impute analysis of categorical variables
str(data$Garage_Quality)
summary(data$Garage_Quality)
which(is.na(data$Garage_Quality))
sum(is.na(data$Garage_Quality))
t1 = table(data$Garage_Quality)
t1 
sum(t1)
barplot(t1)
mode(data$Garage_Quality)

summary(aov(data$Sale_Price ~ data$Garage_Quality))
TukeyHSD(aov(data$Sale_Price ~ data$Garage_Quality))

#pre-impute analysis of categorical variables
str(data$Garage_Condition)
summary(data$Garage_Condition)
which(is.na(data$Garage_Condition))
sum(is.na(data$Garage_Condition))
t1 = table(data$Garage_Condition)
t1 
sum(t1)
barplot(t1)
mode(data$Garage_Condition)

#imputation for categorical variables
data$Garage_Condition[is.na(data$Garage_Condition)] <- "TA"

#post-impute analysis of categorical variables
str(data$Garage_Condition)
summary(data$Garage_Condition)
which(is.na(data$Garage_Condition))
sum(is.na(data$Garage_Condition))
t1 = table(data$Garage_Condition)
t1 
sum(t1)
barplot(t1)
mode(data$Garage_Condition)

summary(aov(data$Sale_Price ~ data$Garage_Condition))
TukeyHSD(aov(data$Sale_Price ~ data$Garage_Condition))

#testing other continuous variables
cor.test(data$Sale_Price,data$Building_Class)
cor.test(data$Sale_Price,data$Lot_Size)
cor.test(data$Sale_Price,data$Overall_Material)
cor.test(data$Sale_Price,data$House_Condition)
cor.test(data$Sale_Price,data$Construction_Year)
cor.test(data$Sale_Price,data$Remodel_Year)
cor.test(data$Sale_Price,data$BsmtFinSF1)
cor.test(data$Sale_Price,data$BsmtFinSF2)
cor.test(data$Sale_Price,data$BsmtUnfSF)
cor.test(data$Sale_Price,data$Total_Basement_Area)
cor.test(data$Sale_Price,data$First_Floor_Area)
cor.test(data$Sale_Price,data$Second_Floor_Area)
cor.test(data$Sale_Price,data$LowQualFinSF)
cor.test(data$Sale_Price,data$Grade_Living_Area)
cor.test(data$Sale_Price,data$Underground_Full_Bathroom)
cor.test(data$Sale_Price,data$Underground_Half_Bathroom)
cor.test(data$Sale_Price,data$Full_Bathroom_Above_Grade)
cor.test(data$Sale_Price,data$Half_Bathroom_Above_Grade)
cor.test(data$Sale_Price,data$Bedroom_Above_Grade)
cor.test(data$Sale_Price,data$Kitchen_Above_Grade)
cor.test(data$Sale_Price,data$Rooms_Above_Grade)
cor.test(data$Sale_Price,data$Fireplaces)
cor.test(data$Sale_Price,data$Garage_Size)
cor.test(data$Sale_Price,data$Garage_Area)
cor.test(data$Sale_Price,data$W_Deck_Area)
cor.test(data$Sale_Price,data$Open_Lobby_Area)
cor.test(data$Sale_Price,data$Enclosed_Lobby_Area)
cor.test(data$Sale_Price,data$Three_Season_Lobby_Area)
cor.test(data$Sale_Price,data$Screen_Lobby_Area)
cor.test(data$Sale_Price,data$Pool_Area)
cor.test(data$Sale_Price,data$Miscellaneous_Value)
cor.test(data$Sale_Price,data$Month_Sold)
cor.test(data$Sale_Price,data$Year_Sold)

#testing other categorical variables
summary(aov(data$Sale_Price ~ data$Zoning_Class+data$Road_Type
            +data$Property_Shape+data$Land_Outline+data$Utility_Type
            +data$Lot_Configuration+data$Property_Slope+data$Neighborhood
            +data$Condition1+data$Condition2))
summary(aov(data$Sale_Price ~ data$House_Type+data$House_Design
            +data$Roof_Design+data$Roof_Quality+data$Exterior1st
            +data$Exterior2nd+data$Exterior_Material+data$Exterior_Condition
            +data$Foundation_Type+data$Heating_Type+data$Heating_Quality
            +data$Air_Conditioning))
summary(aov(data$Sale_Price ~ data$Kitchen_Quality+data$Functional_Rate
            +data$Fireplace_Quality+data$Garage_Quality+data$Garage_Condition
            +data$Fence_Quality+data$Sale_Type+data$Sale_Condition))
TukeyHSD(aov(data$Sale_Price ~ data$Zoning_Class+data$Property_Shape
             +data$Neighborhood+data$House_Type+data$House_Design))
TukeyHSD(aov(data$Sale_Price ~ data$Roof_Design+data$Roof_Quality
             +data$Exterior1st+data$Exterior2nd+data$Exterior_Material
             +data$Heating_Quality+data$Air_Conditioning+data$Kitchen_Quality))

#Creating final models

#Model1
fic<-lm(Sale_Price ~ Overall_Material+Construction_Year+Remodel_Year+Total_Basement_Area+First_Floor_Area+Grade_Living_Area+Full_Bathroom_Above_Grade+Garage_Size, data=data)
fic
summary(fic)
vif(fic)

#Model2
fiz<-lm(Sale_Price ~ Overall_Material+Construction_Year+Remodel_Year+Total_Basement_Area+First_Floor_Area+Grade_Living_Area+Full_Bathroom_Above_Grade+Garage_Size+Brick_Veneer_Area+Fireplaces, data=data)
fiz
summary(fiz)
vif(fiz)

#Model3
fico<-lm(Sale_Price ~ Overall_Material+Total_Basement_Area+First_Floor_Area+Grade_Living_Area+Garage_Size, data = data)
fico
summary(fico)
vif(fico)

#Model4
ficos<-lm(Sale_Price ~ Overall_Material+First_Floor_Area+Grade_Living_Area+Garage_Size, data = data)
summary(ficos)
vif(ficos)

#Model5
ficont<-lm(Sale_Price ~ Overall_Material+Grade_Living_Area, data = data)
ficont
summary(ficont)

#Model6
fiv<-lm(Sale_Price ~ Neighborhood+Zoning_Class+Brick_Veneer_Type+Exterior_Material+House_Type+House_Design+Property_Shape+Lot_Configuration, data = data)
summary(fiv)
vif(fiv)

#Model7
fivv<-lm(Sale_Price ~ Kitchen_Quality+Heating_Quality+Air_Conditioning+Foundation_Type, data = data)
summary(fivv)
vif(fivv)

fivvv<-lm(Sale_Price ~ Lot_Configuration, data = data)
summary(fivvv)

#Model8
fiw<-lm(Sale_Price ~ Neighborhood+Exterior_Material+Kitchen_Quality, data = data)
summary(fiw)
vif(fiw)


#Model9
ficw<-lm(Sale_Price ~ Overall_Material+First_Floor_Area+Grade_Living_Area+Garage_Size+Neighborhood+Exterior_Material+Kitchen_Quality, data = data)
summary(ficw)  # 82.7% accuracy
vif(ficw) # VIF<2
ficw

#Testing for autocorrelation through Durbin Watson Method
library("lmtest")
dwtest(ficw) # p=0.1278>0.05, so no autocorrelation
#So, ficw is the final model
#Property Sales Price mainly depends on Overall Material, First Floor Area, Grade Living Area, Garage Size, Neighborhood, Exterior Material and Kitchen Quality.

#Implementing the final model on test data
data_test<-read.csv("F:/Data Science/Week4/Property_Price_Test.csv")
data_test$Sale_Price<-predict(ficw,data_test)
write.csv(data_test,"F:/Data Science/Week4/Property_Price_Test_updated.csv")

#Publishing the report
install.packages("rmarkdown")
library("rmarkdown")
