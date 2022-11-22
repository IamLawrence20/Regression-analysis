library(raster) #Calls the raster function for use in the data analysis of raster layers
fires.data <- read.csv2("C:/Users/lawre/Desktop/Uni Eastern Finland/Introduction to R/Intro to R/Data/fire_logit.csv") #Import the data containing the coordinates and dependent variables and stored in object 'fire.data'
fires.data <- na.omit(fires.data) #To remove the NAs present in the data
tail(fires.data) #To see the last six rows of the 'fires.data' object created and be sure that the NAs are removed

#Obtaining the dependent variables
vdep.fires <- fires.data[1] #Select the first column of 'fires.data'  and store in a new object 'vdep.fires' as the dependent variable
head(vdep.fires) #To see the first six rows of the dependent variable

#extracting the independent variables
coord.fires <- fires.data[2:3] #Extracting the coordinates by selecting the second and third columns of 'fires.data' and storing as object 'coord.fires'
head(coord.fires)  #To see the first six rows of the coordinates              
files.fire <- list.files("C:/Users/lawre/Desktop/Uni Eastern Finland/Introduction to R/Intro to R/Data/variables_fire",
                         full.names = T, pattern = "asc") #Import the raster layers consisting information about the predictors variables, selecting the full paths and specifying the patter of the data to ensure proper reading
fires <- stack(files.fire) #This combines the rater layers together into one raster, consisting of all the layers
plot(fires) #Gives a plot/pictorial view of the raster layers
vindep.fires <- extract(fires, coord.fires) #This extract the pixel values of the of the fires raster based on the coordinates, and store as the independent or predictor variables
head(vindep.fires, 10) #To see the first ten rows of the independent variables

#combining to obtain our data
fires.logit <- data.frame(cbind(vdep.fires, vindep.fires)) # Combines the dependent and independent variables and convert to data frame structure for fitting the regression model
head(fires.logit, 5) #Gives the first 5 rows of the data frame created above

#Logistic regression without splitting
regress.logit <- glm(logit_1_0 ~ ., data = fires.logit, family = binomial) #Fits a regression model without splitting to observe the AIC and compare to the AIC after splitting
summary(regress.logit) #Give the summary of the regression model

#Splitting the data into two sets (calibration and validation samples)
no.of.rows <- nrow(fires.logit) #To obtain the number of rows in the 'fire.logit' data and store in the object 'no.of.rows.'
ratio <- 0.25 #specifies the desired 25%'
sample.size <- floor(no.of.rows*ratio) #This extract the 25% of the number of rows and store as the sample size
val.sample <- sample(nrow(fires.logit), size = sample.size) #The sample function help to randomly select 25% of the our data
logit.validate <- fires.logit[val.sample,] #Extract 25% of our data variables and store as the validation sample
logit.calculate <- fires.logit[-val.sample,] #Use the remaining 75% as the calibration sample

#fitting the logistic regression model
mod.logit <- glm(logit_1_0 ~ ., data = logit.calculate, family = binomial) #Fits the logistic regression model using the calibration sample
summary(mod.logit) #Obtain the output of the logistic regression model

#calculating the RMSE
#numeric prediction
rmse <- function(error){sqrt(mean(error^2))} #Defines a function for calculating the root mean square error
logit.predict.num <- predict(mod.logit, data = logit.validate, type = "response") #Obtain the numeric validation of the fitted regression model
logit.combined <- cbind(logit.calculate$logit_1_0, logit.predict.num) #Combines the observed and predicted data into one table
logit.combined <- data.frame(logit.combined) #Change the data class into a data frame structure
colnames(logit.combined) <- c("Observed", "Predicted") # To specify the column names of the data frame
rmse(logit.combined$Observed-logit.combined$Predicted) # calls the function created above to calculate the root mean square error

#Spatial predicting the model
logit.predict <- predict(fires, mod.logit, filename = "fires.tif", type = "response", index = 1,
                         progress = "window", overwrite = T) #Obtain the spatial prediction of the fitted logistic regression model
plot(logit.predict, main = "Logistic regression", xlab ="Longitude", ylab = "Latitude",  col = heat.colors(15)) #obtain the plot or pictorial view of the predicted model









