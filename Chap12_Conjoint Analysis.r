#####################
# Conjoint Analysis #
#####################

## Install Packages (if needed)
install.packages("conjoint")

## Load Packages and Set Seed
library(conjoint)
set.seed(1)
setwd("G:/My Drive/Teaching/Marketing Analytics/Chapter Examples")
## Set up attributes and levels as a list
attrib.level <- list(brand = c("CR", "Apple", "Samsung", "FitBit"),
	ship = c("$0", "$10", "$20"), 
	restock = c("0%", "5%", "10%", "15%"),
	retdays = c("7 days", "14 days", "21 days"), 
	price = c("$150", "$200", "$250", "$300"))
attrib.level
## Create the fractional factorial design
experiment <- expand.grid(attrib.level)
design <- caFactorialDesign(data=experiment, type="fractional", cards=30, seed=1)
experiment
design
## Check for correlation in fractional factorial design
print(cor(caEncodedDesign(design)))

## Export design for survey
write.csv(design, file.choose(new=TRUE), row.names = FALSE) ## Name the file conjoint_profiles.csv

## Run the conjoint analysis study

## Read in the survey preference results
pref <- read_csv("Chapter 12/conjoint_preferences.csv") ## Choose the file named conjoint_preferences.csv

## Set up attributes and levels as a vector and Estimate the part-worths for each respondent
attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))
attrib.vector
colnames(attrib.vector) <- c("levels")
attrib.vector
part.worths <- NULL
for (i in 1:ncol(pref)){
  temp <- caPartUtilities(pref[,i], design, attrib.vector)
  ## Pick the baseline case
  ## Adjust coding as needed based on number of attributes and levels
  ## Base Case: Brand CR, Shipping $0, Restock 0%, Retdays 7 days, Price $150
  Base_Brand <- temp[,"CR"]; Base_Ship <- temp[,"$0"]; Base_Restock <- temp[,"0%"]
  Base_Retdays <- temp[,"7 days"]; Base_Price <- temp[,"$150"]
  ## Adjust Intercept
  temp[,"intercept"] <- temp[,"intercept"] - Base_Brand - Base_Ship - Base_Restock - 
    Base_Retdays - Base_Price
  ## Adjust Coefficients
  ## Brand
  L1 <- length(attrib.level$brand) + 1 ## Add 1 for the intercept
  for (j in 2:L1){temp[,j] <- temp[,j] - Base_Brand}
  ## Shipping
  L2 <- length(attrib.level$ship) + L1
  for (k in (L1+1):L2){temp[,k] <- temp[,k] - Base_Ship}
  ## Restock
  L3 <- length(attrib.level$restock) + L2
  for (l in (L2+1):L3){temp[,l] <- temp[,l] - Base_Restock}
  ## Retdays
  L4 <- length(attrib.level$retdays) + L3
  for (m in (L3+1):L4){temp[,m] <- temp[,m] - Base_Retdays}
  ## Price
  L5 <- length(attrib.level$price) + L4
  for (n in (L4+1):L5){temp[,n] <- temp[,n] - Base_Price}
  part.worths <- rbind(part.worths, temp)
}
rownames(part.worths) <- colnames(pref)

## Export part-worths from analysis
write.csv(part.worths, file.choose(new=TRUE), row.names = FALSE) ## Name the file conjoint_partworths.csv

##################
####################### Preparation
#### Step 4: Experimental Design
# Creating a fractional Design
install.packages("DoE.base")
library(DoE.base)
test.design <-oa.design(nlevels =c(6,2,3,3,3,2,2))

FracDesign <-as.data.frame(test.design)
names(FracDesign) <-c("Brand", "Cores", "RAM", "HardDrive","DSize","DQuality","TouchScreen")
levels(FracDesign$Brand) <-c("Apple", "Lenovo", "Acer", "Asus","Ethos", "Other")
levels(FracDesign$Cores) <-c("Dual Core", "Quad Core")
levels(FracDesign$RAM) <- c("4GB", "8 GB", "16 GB")
levels(FracDesign$HardDrive) <-c("256 GB", "512 GB", "1024 GB")
levels(FracDesign$DSize) <-c("12 Inch", "14 Inch", "15.2 Inch")
levels(FracDesign$DQuality) <-c("Normal", "HD")
levels(FracDesign$TouchScreen) <-c("Yes", "No")
rm(test.design)

# Save design into an excel file
install.packages("xlsx")
library(xlsx)
write.xlsx(FracDesign, "C:/Users/Economalytics/Desktop/ExperimentalDesign.xlsx")

# Example for full factorial design
install.packages("AlgDesign")
library(AlgDesign)

numberlevel = c(c(6,2,3,3,3,2,2))
fulldesign <-gen.factorial(numberlevel)
nrow(fulldesign) # Runs full factorial
nrow(FracDesign) # Runs fractionalfactorial

####################### Creating Utility Functions
#### Data Collection (Create Dataset)
# Create basis
set.seed(1234)
n <- 89 # number of participants
Data <- data.frame(Participant =1:89)
Data$Participant <-as.factor(Data$Participant)

for (run in 1:36) {
  Data[,paste("Run",as.character(run), sep = "")]<- sample(c(1:9), n, replace = TRUE)
}
Data
# Shaping the data
Data[,c(6,11,17,28,33)] <-Data[,c(6,11,17,28,33)] + 2 # Improve Apple
Data[,c(8,13,14,15,18,35)] <-Data[,c(8,13,14,15,18,35)] - 2 # Decrease Ethos
Data[,c(2,4,5,7,8,11,12,13,16,18,19,25,28,29,31,32,33,37)]<- Data[c(2,4,5,7,8,11,12,13,16,18,19,25,28,29,31,32,33,37)] - 0.6 
Data[,c(2,3,5,9,11,13,15,16,19,23,26,30)]<- Data[, c(2,3,5,9,11,13,15,16,19,23,26,30)] + 0.9
Data[,c(2,3,6,9,10,13,18,19,20,21,22,23,25,28,29,31,33,35)]<- Data[,c(2,3,6,9,10,13,18,19,20,21,22,23,25,28,29,31,33,35)] + 1

Data[,-1] <- round(Data[,-1])
Data[,-1][Data[,-1] < 1] <- 1
Data[,-1][Data[,-1] > 9] <- 9
Data

########################## Estimatingthe Part-Worth Models
# Merging FracDesign and Data
install.packages("data.table")
library(data.table)

Data$Participant <- NULL
Data <- transpose(Data)
rownames(Data) <- c(1:36)
Conjoint <- cbind(FracDesign, Data)
Conjoint
# Compute linear regression for eachperson 

install.packages("rlist")
library(rlist)
Regressions <- list()

for (person in 8:ncol(Conjoint)) {
  model <- lm(Conjoint[,person]~ factor(Brand) + 
                factor(Cores) + 
                factor(RAM) + 
                factor(HardDrive) + 
                factor(DSize) + 
                factor(DQuality) + 
                factor(TouchScreen) , data =Conjoint)
  Regressions <- list.append(Regressions, model)
}

Regressions
# Create dataframe with part-worthvalues

vars <- c("Intercept",
          rep("Brand",6),
          rep("Cores",2),
          rep("RAM",3),
          rep("HardDrive", 3),
          rep("DSize",3),
          rep("DQuality",2),
          rep("TouchScreen",2))
vars
lvls <- c("Intercept",
          as.character(levels(Conjoint$Brand)),
          as.character(levels(Conjoint$Cores)),
          as.character(levels(Conjoint$RAM)),
          as.character(levels(Conjoint$HardDrive)),
          as.character(levels(Conjoint$DSize)),
          as.character(levels(Conjoint$DQuality)),
          as.character(levels(Conjoint$TouchScreen)))
lvls

Results <-data.frame(Variable=vars,Levels=lvls)
Results

for (person in 1:n) {
  c <- as.vector(Regressions[[person]]$coefficients)
  coef <-c(c[1],0,c[2:6],0,c[7],0,c[8:9],0,c[10:11],0,c[12:13],0,c[14],0,c[15])
  Results[,paste("Person",person,sep="")] <-round(coef, digits = 1)
}
Results

# Get averages and visualize them foreach variable

Results[,"Average"] <-round(rowMeans(Results[,-c(1,2)]),digits = 1)

install.packages("ggplot2")
library(ggplot2)


# Brand
subs <- droplevels(subset(Results,Variable == "Brand"))
subs$Levels <- reorder(subs$Levels,subs$Average)

if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg1 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("Brand")

# Cores
subs <- droplevels(subset(Results,Variable == "Cores"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg2 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("Cores")

# DQuality
subs <- droplevels(subset(Results,Variable == "DQuality"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg3 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("DQuality")

# DSize
subs <- droplevels(subset(Results,Variable == "DSize"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg4 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("DSize")

# HardDrive
subs <- droplevels(subset(Results,Variable == "HardDrive"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg5 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("HardDrive")

# RAM
subs <- droplevels(subset(Results,Variable == "RAM"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg6 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("RAM")

# TouchScreen
subs <- droplevels(subset(Results,Variable == "TouchScreen"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg7 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("TouchScreen")

library(gridExtra)

grid.arrange(gg1, gg2, gg3, gg4, gg5,gg6, gg7, nrow=4, ncol=2)

# Compute relative importance
install.packages("relaimpo")
library(relaimpo)

Importances <- data.frame(Variable= c("Brand", "RAM", "HardDrive","DSize", "Cores", "DQuality","TouchScreen"))

for (model in 1:n) {
  relImp <- calc.relimp(Regressions[[model]], type =c("lmg"), rela = TRUE)
  relImp <- as.vector(relImp@lmg)
  Importances[,paste("Person",model,sep="")] <-round(relImp, digits = 3)
}
Importances
Importances$Average <-rowMeans(Importances[,-1])
Importances

ggplot(Importances,aes(x=reorder(Variable, Average), y=Average)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = function(x)paste(x*100, "%"))
######### Predict potential marketshare
# Simulate laptops from brands

vnames <- c("Brand","Cores", "RAM", "HardDrive","DSize","DQuality","TouchScreen")


brand <-sample(c("Apple", "Lenovo", "Acer","Asus", "Other"),49,replace = TRUE)
cores <- sample(c("DualCore", "Quad Core"), 49, replace = TRUE)
ram <- sample(c("4 GB","8 GB", "16 GB"), 49, replace = TRUE)
harddrive <- sample(c("256GB", "512 GB", "1024 GB"), 49, replace = TRUE)
dsize <- sample(c("12Inch", "14 Inch", "15.2 Inch"), 49, replace = TRUE)
dquality <-sample(c("Normal", "HD"), 49, replace = TRUE)
touchscreen <-sample(c("Yes", "No"), 49, replace = TRUE)

Market <- data.frame(a=brand,b=cores, c=ram, d=harddrive, e = dsize, f= dquality, g = touchscreen)

names(Market) <- vnames
Market
n
Regressions

# Caclulate utility scores for eachlaptop for each user
for (participant in 1:n) {
  Market[,paste("P",participant,sep="")] <-predict(Regressions[[participant]], newdata = Market[,1:7])
} 

# Determine the potential market share
purchased <-unlist(apply(Market[,8:ncol(Market)], 2, function(x) which(x == max(x))))
purchased <-Market$Brand[purchased]
brandcount <-as.data.frame(table(purchased))
brandcount$Freq <- brandcount$Freq/ sum(brandcount$Freq)

ggplot(brandcount, aes(x=purchased,y=Freq)) + geom_bar(stat="identity")

#side----------
library(tidyverse)
library(readxl)

icecream <- read_excel("C:/Users/Sungjin Kim/Downloads/icecream.xlsx")
icecream

icecream <- icecream %>% 
  pivot_longer(cols = starts_with("Individual"), names_to = "respondent", values_to = "rating") %>% # respondent keeps track of the respondent, rating will store the respondent's ratings, and we want to stack every variable that starts with Individual
  rename("profile" = "Observations") %>% # rename Observations to profile
  mutate(profile = factor(profile), respondent = factor(respondent),  # factorize identifiers
         Flavor = factor(Flavor), Packaging = factor(Packaging), Light = factor(Light), Organic = factor(Organic)) # factorize the ice cream attributes

icecream
# Wide dataset: one row per unit of observation (here: profile) and a number of columns for the different observations (here: respondents)
# Long dataset: one row per observation (here: profile x respondent combination)

# Converting from wide to long means that we're stacking a number of columns on top of each other.
# The pivot_longer function converts datasets from wide to long and takes three arguments:
# 1. The cols argument: here we tell R which columns we want to stack. The original dataset had 10 rows with 15 columns for 15 individuals. The long dataset will have 150 rows with 150 values for 15 individuals. This means we need to keep track of which individual we're dealing with.
# 2. The names_to argument: here you define the name of the variable that keeps track of which individual we're dealing with.
# 3. The values_to argument: here you define the name of the variable that stores the actual values.

icecream