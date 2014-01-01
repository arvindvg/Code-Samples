# Simple regression model to predict price of a diamond based on diamond characteristics

library(ggplot2)
library(fpc)
getwd()
setwd("C:/Users/arvindvg/Desktop/Sparklie/Data")

#Aggregate multiple files containing diamond pricing data

files <- list.files(pattern = "[.]csv$")

new_data = {
  read.all <- lapply(files, read.csv, 
                     header = FALSE,
                     col.names = c("carat","cut","color","clarity","polish","symmetry",
                                   "depth","table","fluor","pricepercarat","cutlet",
                                   "deliverydate","price"))
  new_data <- do.call(rbind, read.all)
}

write.table(new_data,file="diamond_data_new.csv",sep = ",",col.names = TRUE)

# Re-read file already some changes made in Excel
new_data_v2 <- read.csv("diamond_data_new.csv",header=TRUE,row.names="ID")
str(new_data_v2)
summary(new_data_v2$price)

new_data <- new_data_v2[c(-10,-12)]
#new_data_dummy <-  model.matrix(~.,data=new_data)

# Visually study relationship between price & Other variables
p <- ggplot(new_data,aes(x=carat,y=price))
p + geom_point(color="green",size=0.2,position="jitter")
p + geom_point(aes(color=clarity),position="jitter")
p + geom_point(aes(color=color),position="jitter")
p + geom_point(aes(color=cut),position="jitter")

hist <- ggplot(new_data,aes(price))
hist + geom_histogram(binwidth = 0.1,aes(y = ..density..)) + geom_density()

# Summary Metrics
str(new_data)
summary(new_data$cut)
summary(new_data$color)
summary(new_data$clarity)
summary(new_data$polish)
summary(new_data$symmetry)

# Create a class variables on Carat - Based on the visualization diamond price jump at certain breaks
new_data$carat_class <- cut(new_data$carat,c(0.3,0.6,1,1.5,2,20))

# First model with all variables and quadratic and cubic terms of carat variables
# Model log(price)
reg_model <- lm(data=new_data,log(price) ~ carat + depth + table + carat_class + I(carat^2) +  I(carat^3) +
I(cut == "GoodGoodG") + I(cut == "IdealIdealI") + I(cut == "Signature IdealSig IdealSIG") + I(cut == "Very GoodV GoodVG") +  
I(color == "D")  +  I(color == "E") + I(color == "F")  + I(color == "G")  + I(color == "H")+ I(color == "I") +
I(clarity == 'FL') + I(clarity == 'IF') + I(clarity == 'SI1')+ I(clarity == 'VS1')+ I(clarity == 'VS2')+ I(clarity == 'VVS1')+ I(clarity == 'VVS2')+
I(polish == 'EX') + I(polish == 'G') + I(polish == 'VG') +
I(symmetry == 'EX') + I(symmetry == 'G') + I(symmetry == 'VG'))

reg_model
summary(reg_model)

# Refine model based on previous results keeping only the predictive variables
reg_model_v2 <- lm(data=new_data,price ~ carat + carat_class + I(carat^2) +  I(carat^3) +
                  I(cut == "GoodGoodG") + I(cut == "IdealIdealI") + I(cut == "Signature IdealSig IdealSIG") + I(cut == "Very GoodV GoodVG") +  
                  I(color == "D")  +  I(color == "E") + I(color == "F")  + I(color == "G")  + I(color == "H")+ I(color == "I") +
                  I(clarity == 'FL') + I(clarity == 'IF') + I(clarity == 'SI1')+ I(clarity == 'VS1')+ I(clarity == 'VS2')+ I(clarity == 'VVS1')+ I(clarity == 'VVS2')
                   )

# Diagonistics
summary(reg_model_v2)
names(reg_model_v2)
reg_model_v2$residuals
plot(reg_model_v2)

