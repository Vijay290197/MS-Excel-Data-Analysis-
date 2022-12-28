library(readr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(olsrr)

setwd("D:/Augment Systems Kasotiya/Excel/15. Car Dataset Excel and R- AH078063BB")

# Import Data set (Car)

Car_df <- read.csv("Car details v3.csv")
View(Car_df)

library(psych)
describe(Car_df)

# Looking at null values
Car_df %>% 
  summarise_all(~ sum(is.na(.)))

# ---------------- Data preprocessing-------------------

## For name variable

split_cars <- strsplit((Car_df$name), " ")
split_cars

Brand <- sapply(split_cars, "[",1)
Brand

Car_df['Brand'] <- c(Brand)

#View(Car_df)

Car_df <- subset(Car_df, select = -c(name)) #Drop name column
View(Car_df)

## For mileage

#split_mileage <- strsplit((Car_df$mileage)," ")
#View(split_mileage)

#Mileage_kmpl <- sapply(split_mileage, "[",1.2)
#View(Mileage_kmpl)
#Mileage_kmpl

#OR

Mileage_kmpl <- parse_number(Car_df$mileage)
Mileage_kmpl

Car_df['Mileage_kmpl'] <- c(Mileage_kmpl)
View(Car_df)

### Conversion of km/kg to kmpl

Car_df <- Car_df %>%
  mutate(Status = case_when(endsWith(mileage, "g") ~ 1,
                            endsWith(mileage, "l") ~ 0))
Car_df <- Car_df %>% 
mutate(Mileage_kmpl = if_else(Status == 1, Mileage_kmpl*1.4, Mileage_kmpl))

## For Engine (CC)

Engine_CC <- parse_number(Car_df$engine)
Engine_CC

Car_df['Engine_CC'] <- c(Engine_CC)
View(Car_df)

max_powerbhp <- parse_number(Car_df$max_power)
max_powerbhp

Car_df['max_powerbhp'] <- c(max_powerbhp)
View(Car_df)

#Car_df[4934,]

#Drop Old or unused Variables

Car_df <- subset(Car_df, select = -c(mileage, Status, engine, max_power, torque)) #Drop columns
View(Car_df)

# Conversion of Categorical to Numerical Value

str(Car_df)

unique(Car_df$fuel)
Car_df <- Car_df %>%
  mutate(fuel = case_when(fuel == "Diesel" ~ 1,
                          fuel == "Petrol" ~ 2,
                          fuel == "LPG" ~ 3,
                          fuel == "CNG" ~ 4,
                                 TRUE ~ 0))
#unique(Car_df$fuel)

#unique(Car_df$seller_type)

Car_df <- Car_df %>%
  mutate(seller_type = case_when(seller_type == "Individual" ~ 1,
                                 seller_type == "Dealer" ~ 2,
                                 seller_type == "Trustmark Dealer" ~ 3,
                                     TRUE ~ 0))

unique(Car_df$seller_type)


unique(Car_df$transmission)
Car_df <- Car_df %>%
  mutate(transmission = case_when(transmission == "Manual" ~ 1,
                                  transmission == "Automatic" ~ 2,
                                  TRUE ~ 0))

#unique(Car_df$transmission)


unique(Car_df$owner)
Car_df <- Car_df %>%
  mutate(owner = case_when(owner == "First Owner" ~ 1,
                           owner == "Second Owner" ~ 2,
                           owner == "Third Owner" ~ 3,
                           owner == "Fourth & Above Owner" ~ 4,
                           owner == "Test Drive Car" ~ 5,
                                  TRUE ~ 0))


View(Car_df)

# Filling Missing Values

#1.
Car_df$Mileage_kmpl[is.na(Car_df$Mileage_kmpl)]<- mean(Car_df$Mileage_kmpl, na.rm=TRUE)
View(Car_df)

#2.
Car_df$Engine_CC[is.na(Car_df$Engine_CC)]<- median(Car_df$Engine_CC, na.rm=T)
View(Car_df)

#3. 

Car_df$max_powerbhp[is.na(Car_df$max_powerbhp)]<- mean(Car_df$max_powerbhp, na.rm=T)
View(Car_df)

#4.

Car_df$seats[is.na(Car_df$seat)]<- median(Car_df$seat, na.rm=T)
View(Car_df)


#Check Correlation


#install.packages("ggcorrplot")
library(ggcorrplot)

df <- subset(Car_df, select = -c(Brand)) #Drop name column
View(df)

corr <- round(cor(df), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)


## Graphical Analysis

library(ggplot2)

ggplot(Car_df, aes(x = fuel)) +
  geom_bar()


ggplot(Car_df, aes(x = owner)) +
  geom_bar()
  

ggplot(Car_df, aes(x = transmission)) +
  geom_bar()

ggplot(Car_df, aes(x = seller_type)) +
  geom_bar()


scatter.smooth (x=Car_df$Mileage_kmpl, y=Car_df$Engine_CC,
               main="Engine_CC vs Mileage_kmpl")

avg_sp <- aggregate(selling_price ~ year, Car_df, mean)
avg_sp

avg_sp <- data.frame(avg_sp)
avg_sp

ggplot(avg_sp)+
  geom_bar(mapping = aes(x = year, y = selling_price), stat = "identity")


brand10 <- aggregate(selling_price ~ Brand, Car_df, mean)
brand10

data.frame(brand10)

brand10 <- brand10[order(brand10$selling_price, decreasing = TRUE),][1:10,]

brand10


mileage10 <- aggregate(Mileage_kmpl ~ Brand , Car_df, mean)
mileage10

data.frame(mileage10)

mileage10 <- mileage10[order(mileage10$Mileage_kmpl, decreasing = TRUE),][1:10,]

mileage10

data.frame(mileage10)
ggplot(mileage10)+
  geom_bar(mapping = aes(x = Brand, y = Mileage_kmpl), stat = "identity")




#------------------------Linear Regression-------------------------------------
  

# x and y Determination
#x_var <- data.frame(Car_df$fuel, Car_df$seller_type, Car_df$transmission, Car_df$owner)

y_var <- data.frame(Car_df$selling_price)

model_reg <- lm(formula = y_var ~ fuel+seller_type+transmission+
                  seats+Mileage_kmpl+Engine_CC+max_powerbhp+km_driven, data = Car_df)
print(summary(model_reg))

model_reg$coefficients
