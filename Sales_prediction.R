install.packages("corrplot")

library(randomForest)
library(caret)
library(corrplot)
library(dplyr)


setwd("~/Google Drive/DATA ANALYSTICS/REVISIÓN/prueba")
sales <- read.table("existingproductattributes2017.2 (1).csv", sep=",", header=TRUE)
newproducts <- read.table("newproductattributes2017.2 (1).csv", sep=",", header=TRUE)
sales_new_products <- read.table("newproductattributes2017.2 (1).csv", sep=",", header=TRUE)

names(newproducts)
names(sales)
str(sales)

sales$ProductNum <- NULL
sales$BestSellersRank <- NULL

dummyproducttype <- dummyVars("~ProductType", data = sales)
dummyproducttype

producttype_with_dummy <- data.frame(predict(dummyproducttype, newdata = sales))
head(producttype_with_dummy)

sales_with_dummy <- data.frame(cbind(producttype_with_dummy, sales))

names(sales_with_dummy)

head(sales_with_dummy)

str(sales_with_dummy)



boxplot(sales_with_dummy$Volume, main="outlier", boxwex=0.1)

head(sales_with_dummy$Volume, 80)

sales_with_dummy_sin_outliers <- sales_with_dummy %>%
  filter(Volume < 6000)

sales_with_dummy <- sales_with_dummy_sin_outliers

sales_with_dummy <- cor(sales_with_dummy)
corrplot(sales_with_dummy, method = "number", number.cex= 15/ncol(sales_with_dummy), title = "Correlation Matrix", tl.cex = 0.5, type = "upper",tl.col = "blue2")

sales_with_dummy$ProductType.Accessories   <- NULL
sales_with_dummy$ProductType.Display <- NULL
sales_with_dummy$ProductType.ExtendedWarranty <- NULL
sales_with_dummy$ProductType.Netbook <- NULL
sales_with_dummy$ProductType.PC <- NULL
sales_with_dummy$ProductType.GameConsole <- NULL
sales_with_dummy$ProductType.Printer <- NULL
sales_with_dummy$ProductType.PrinterSupplies <- NULL
sales_with_dummy$ProductType <- NULL
sales_with_dummy$ProductType.Smartphone <- NULL
sales_with_dummy$ProductType.Software <- NULL
sales_with_dummy$ProductType.Tablet <- NULL
sales_with_dummy$Price <- NULL
sales_with_dummy$x5StarReviews <- NULL
sales_with_dummy$x3StarReviews <- NULL
sales_with_dummy$x1StarReviews <- NULL
sales_with_dummy$Recommendproduct <- NULL
sales_with_dummy$ShippingWeight <- NULL
sales_with_dummy$ProductDepth <- NULL
sales_with_dummy$ProductWidth <- NULL
sales_with_dummy$ProductHeight <- NULL
sales_with_dummy$ProfitMargin <- NULL
sales_with_dummy$ProductType.Laptop <- NULL
sales_with_dummy$ProductType <- NULL

salesfinal <- sales_with_dummy


names(salesfinal)

set.seed(123)
trainIndex <- createDataPartition(salesfinal$Volume, p = .80, list = FALSE)

salesTrain <- salesfinal[ trainIndex,]
salesTest  <- salesfinal[-trainIndex,]

set.seed(123)

system.time(rfsales <- randomForest(Volume ~ ., salesTrain, ntree=350))

rfpredsales <- predict(rfsales, newdata = salesTest)

rfpredsales

salesTest$rfpredsales <- rfpredsales

postResample(rfpredsales, salesTest$Volume)


newproducts$Price <- NULL
newproducts$x5StarReviews <- NULL
newproducts$x3StarReviews <- NULL
newproducts$x1StarReviews <- NULL
newproducts$Recommendproduct <- NULL
newproducts$ShippingWeight <- NULL
newproducts$ProductDepth <- NULL
newproducts$ProductWidth <- NULL
newproducts$ProductHeight <- NULL
newproducts$ProfitMargin <- NULL
newproducts$ProductType.Laptop <- NULL
newproducts$ProductType <- NULL
newproducts$ProductNum <- NULL
newproducts$BestSellersRank <- NULL

names(newproducts)

rfprednewproducts <- predict(rfsales, newdata = newproducts)

rfprednewproducts

newproducts$rfprednewproducts <- rfprednewproducts

sales_new_products <- data.frame(cbind(sales_new_products, rfprednewproducts))

View(sales_new_products)




