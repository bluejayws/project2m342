#Clear the data
rm(list=ls())

#The dataset of interest
data <- read.csv("jeju_data2model_pud_4WWU.csv",header <- TRUE)
attach(data)

#Size of the data (number of rows by number of columns)
dim(data)
#[1] 824  15

#Response variable (average annual PUD)
y <- usdyav

# Histogram of the response variable (usdyav)
hist(y, breaks = 30, freq = FALSE, main = "Histogram of the Response (y)", 
     xlab = "Response (y)")

# location of zeros
y0index <- which(y == 0)
y0index

#Number of zeros in the original response variable
y0s <- length(y0index)
y0s
#[1] 156

#Response variable without 0s.
yw0 <- y[-y0index]

# Histogram of the response variable without zeros
hist(yw0, breaks = 30, freq = FALSE, main = "Histogram of the Response without 0s (yw0)", 
     xlab = "Response w/o zeros (yw0)")

#Box-Cox transformation to determine an optimal transformation.
#For more information, see: 
#https://towardsdatascience.com/box-cox-transformation-explained-51d745e34203
#The most optimal transformation theoretically makes the data closest to normal.
library(MASS)
bc_info <- boxcox(yw0~1, lambda = seq(-5, 5, 1/10))

#Optimal power transformation for y (0 indicates log transformation)
y_opt_power <- bc_info$x[which.max(bc_info$y)]
y_opt_power
#[1] -0.2

#The power is close enough to zero, so apply the log transformation
#I would not choose -0.2 (inverse fifth-root) as it is less popular than log.
# Histogram of log(yw0)
logyw0 <- log(yw0)
hist(logyw0, breaks = 30, freq = FALSE, main = "Histogram of log(yw0)",
     xlab = "Log-transformed response w/o 0s")

#For comparison, inverse and square-root transformations are displayed below
#For an inverse transformation, it'd be a good idea to add a negative sign.
#That will preserve the order of the original transformation.
invyw0 <- -(yw0)^(-1)
sqrtyw0 <- (yw0)^(1/2)
par(mfrow=c(1,2))
hist(invyw0, breaks = 30, freq = FALSE, main = "Histogram of (yw0)^(-1)",
     xlab = "Inverse response w/o 0s")
hist(sqrtyw0, breaks = 30, freq = FALSE, main = "Histogram of (yw0)^(1/2)",
     xlab = "Square-rooted response w/o 0s")

#If you choose to work with the data without 0s, remove all the rows for which y is 0.
#If you decide to do so, uncomment the following and use data0 instead.

#data0 <- data[-y0index,]
#attach(data0)

#You may still work with original data. In that case, consider the following

#Fisher et al. (2016) tried the log(y+1) transformation on the response variable.
#1 is added to avoid the problem with zeros.
y1 <- y+1
logy1 <- log(y1)
par(mfrow=c(1,1))
hist(logy1, freq=FALSE, main="Histogram of log(y+1)",
     xlab = "log(y+1) as Response")

#Is the log transformation really the best transformation? Try the following.
bc_info1 <- boxcox(y1~1, lambda = seq(-5, 5, 1/10))

#Optimal power transformation for y (0 indicates log transformation)
y_opt_power1 <- bc_info1$x[which.max(bc_info1$y)]
y_opt_power1
#[1] -1.7

#Some variables never become normal even after trying a bunch of transformations.
#In that case, try to make the transformed distribution symmetric.
par(mfrow=c(1,3))
hist(-y1^(-1), breaks = 30, freq = FALSE, main = "Histogram of (y1)^(-1)",
     xlab = "Inverse")
hist(-y1^(-2), breaks = 30, freq = FALSE, main = "Histogram of (y1)^(-2)",
     xlab = "Inverse squared")
hist(-y1^(-3), breaks = 30, freq = FALSE, main = "Histogram of (y1)^(-3)",
     xlab = "Inverse cubed")

#The explanatory variables.
#For more details, see metadata_4WWU.xls.
#Try applying different transformations using histograms and boxcox().
#Or, in some cases, it may make sense to try dummy variables.
#This is useful especially when there are so many zeros.
x1 <- commercial_km2
x2 <- culthistpl_km2
x3 <- naturalmon_km2
x4 <- road_moct_kms_100s
x5 <- sandbeach_km2
x6 <- seacliff_km2
x7 <- viewpoint
x8 <- airdist_kms_100s
x9 <- land_km2_10s
x10 <- forest_km2_10s
x11 <- athletic_km2_10s
x12 <- industrial_km2
x13 <- trails_osm_kms_100s
x14 <- nearroad_kms

#Locations of 0s for the x1 variable
#Most of them are 0s, so we try the dummy variable approach.
hist(x1, freq = FALSE, main = "Histogram of x1",
     xlab = "x1")
x10index <- which(x1==0)
sum(x10index)
#[1] 768

#x1dummy below converts anything > 0 to 1.
x1dummy <- as.integer(x1 > 0)
table(x1dummy)
#Check other variables to see if there are many 0s, in which case, consider dummy variables.

#Multiple linear regression model 
model <- lm(logy1~x1dummy+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
summary(model)

#Perform variable selection (to be covered in Chapter 11)

#Run regression diagnostics by checking the residuals
resid <- model$residuals
hist(resid, freq = FALSE, main = "Model Residuals",
     xlab = "Residuals")

#Advanced topic (Lasso regression, a type of penalized least squares)
#The lasso regression performs variable selection and model estimation simultaneously.

#install.packages("glmnet")
library("glmnet")
#Using lasso
alpha = 1

#Save all the explanatory variables in data frame.
xmat <- data.frame(x1dummy=x1dummy, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6, x7=x7, 
                   x8=x8, x9=x9, x10=x10, x11=x11, x12=x12, x13=x13, x14=x14)
#Convert the data frame into a matrix.
xmat <- as.matrix(xmat)

#Fit the model
cvfit = cv.glmnet(xmat, logy1, alpha=alpha)

#lambda based on the 1se criterion
lambda_1se = cvfit$lambda.1se
lambda_1se
#[1] 0.03675202
#The lambda estimate may vary, so it would be a good idea to save it to replicate the results.
lambda_1se = 0.03675202

#The ones with dots mean dropped variables. 
#The ones without dots mean estimated coefficients.
coef.lambda.1se <- coef(cvfit, s = lambda_1se)
coef.lambda.1se
# 15 x 1 sparse Matrix of class "dgCMatrix"
# s1
# (Intercept)  0.41977236
# x1dummy      0.21202847
# x2           .         
# x3           0.06486587
# x4           1.72231236
# x5           9.89707748
# x6          16.68576213
# x7           0.19102726
# x8          -1.19104622
# x9          -0.22339178
# x10          .         
# x11          .         
# x12          .         
# x13          9.34953555
# x14          0.01071314

#Predicted values of y (y.hat) based on the x values in the data using lambda_1se.
fit = glmnet(xmat, logy1, alpha = alpha, nlambda = 20)
y.hat = predict(fit,newx=xmat,s=lambda_1se)

#Residuals (run the regression diagnostics based on this)
resid.lasso = y-y.hat

#Note: In general, it is difficult to measure the significance of these predictor variables.
#This happens because the p-value calculation for the lasso regression is very difficult.
#At least, the lasso results may supplement the results from other model selection results.

#R code for variable transformation:
  #x1,x2,x3,x9 variables (Raina Hoy, hoyr2@wwu.edu)
  #x4,x5,x6 variables (Che Ho)
  #x7,x8,x10 variable (Antonio J. Gurrola-Beltran)
  #x11,x12,x13,x14 (Samuel Borcherding)
  
  #x1
  x1 <- commercial_km2
#we first take a look at the histogram of x1 
hist(x1, breaks = 30, freq = FALSE, main = "Histogram of x1", 
     xlab = "x1")
#the dat looks heavily skewed to the left, 
#therefore it would be a good idea to look at how many zeros are in the data 
x1index <- which(x1 == 0)
x1index
x1s <- length(x1index)
x1s
#the data returns 768 zeros which is a lot for this data set
#the best thing would be to perform the dummy variable function 
x1dummy <- as.integer(x1 > 0)
table(x1dummy)
#take the histogram 
hist(x1dummy, breaks = 30, freq = FALSE, main = "Histogram of x1dummy", 
     xlab = "x1dummy")
#once we take the dummy variable, there's nothing left to do 

#x2
x2 <- culthistpl_km2
#let's look at the histogram of x2
hist(x2, breaks = 30, freq = FALSE, main = "Histogram of x2", 
     xlab = "x2")
#suspicious amount of zeros, let's look at the exactly how many zeros  
x2 <- culthistpl_km2
x2index <- which(x2 == 0)
x2index
x2s <- length(x2index)
x2s
#the data returns 705 zeros, therefore dummy variable is best choice,
#for instance, if i did the log function and took histogram 
x2_log = log(x2)
hist(x2_log, breaks = 30, freq = FALSE, main = "Histogram of x2_log", 
     xlab = "x2_log")
#it looks normal, however, when we take the log of x2, 
log(x2)
#we get a ton of -inf
#bad
#therefore, we leave it as a dummy variable
x2dummy <- as.integer(x2 > 0)
table(x2dummy)
#take the histogram 
hist(x2dummy, breaks = 30, freq = FALSE, main = "Histogram of x2dummy", 
     xlab = "x2dummy")
#the histogram of the dummy variable shows that there is a disproportionate amount of zeros as there are ones 

#x3
x3 <- naturalmon_km2
#let's look at the histogram of x3 first, then try to use the log function 
hist(x3, breaks = 30, freq = FALSE, main = "Histogram of x3", 
     xlab = "x3")
#we run into a similar situation as x2, where the log appeared to produce a nice normal distribution, however, it returned lots of -inf when the log(x3) was run 
#there are a lot of zeros, let's check exactly how many 
x3index <- which(x3 == 0)
x3index
x3s <- length(x3index)
x3s
#there are 632 zeros in the data, therefore we decide to dummy variable x3
x3dummy <- as.integer(x3 > 0)
table(x3dummy)
#take the histogram 
hist(x3dummy, breaks = 30, freq = FALSE, main = "Histogram of x3dummy", 
     xlab = "x3dummy")
#the histogram of the dummy variable shows that there is a disproportionate amount of zeros as there are ones 

#X4
#log(10) transf
x4_log = log10(x4) 
#cube root
x4_cube_root = pracma::nthroot(x4, 2) 
#cubic root
x4_cubic_root = pracma::nthroot(x4, 3) 
#comparing
par(mfrow=c(1,3))
hist(x4_log, breaks = 30, freq = FALSE, main = "Histogram of log10(x4)", 
     xlab = "log10(x4)")
hist(x4_cube_root, breaks = 30, freq = FALSE, main = "Histogram of x4_cube_root", 
     xlab = "x4_cube_root")
hist(x4_cubic_root, breaks = 30, freq = FALSE, main = "Histogram of x4_cubic_root", 
     xlab = "x4_cubic_root")

#X5
#we first take a look at the histogram of x5 
hist(x5, breaks = 30, freq = FALSE, main = "Histogram of x5", 
     xlab = "x5")
#we see that transformations are needed to try to normalize the data 
#checking how many 0s we are dealing with is the best place to start 
x5dummy <- as.integer(x5 > 0)
table(x5dummy)
#take the histogram 
hist(x5dummy, breaks = 30, freq = FALSE, main = "Histogram of x5dummy", 
     xlab = "x5dummy")
#the histogram of the dummy variable shows that there is a disproportionate amount of zeros as there are ones 
#we leave it as a dummy variable 

#x6
#checking original x6
hist(x6, breaks = 30, freq = FALSE, main = "Histogram of x6", 
     xlab = "x6")
#trying log
x6_log = log(x6)
hist(x6_log, breaks = 30, freq = FALSE, main = "Histogram of log(x6)", 
     xlab = "log(x6)")
#seems not ideal, keep trying
#checking how many 0s we are dealing with
x6dummy <- as.integer(x6 > 0)
table(x6dummy)
hist(x6dummy, breaks = 30, freq = FALSE, main = "Histogram of x6dummy", 
     xlab = "x6dummy")
#seems there are a lot of zeros therefore we leave it as the dummy variable 

#x7
x7 <- viewpoint
# Initial observations : Note the large amount of 0's, with some 1's, 2's, and an outlier of 4 and 6
max(x7) # Max value
hist(x7, breaks = 30, freq = FALSE, main= "Histogram of x7", xlab = "x7")
# So a dummy variable seems like the best way to proceed here, and we see a (very) vaguely symmetric distribution
x7_dummy = as.integer(x7 > 0)
table(x7_dummy)
hist(x7_dummy, breaks = 30, freq = FALSE, main = "Histogram of x7 Dummy")

#x8
x8 <- airdist_kms_100s
# Initial observations show the distribution is close to normal distribution, but
#   perhaps there's room for improvement
# Histogram and QQPlot
qqnorm(x8, pch = 1, frame = FALSE)
qqline(x8, col = "steelblue", lwd = 2)
hist(x8, breaks = 30, freq = FALSE, main= "Histogram of x8", xlab = "x8")
# Shapiro-Wilkes
install.packages("dplyr")
library("dplyr")
shapiro.test(x8)
# W = 0.96806, p-value = 1.906e-12 | P-value indicates lack of statistical evidence for normal distribution
# x8 has the most fleshed out data (no significant amount of 0's), so we can directly use transformations
# We found that log(x+1), and log_2(x+1) gave really good normalizations. It appears
#   that log_2(x+1) is more normal
# Log(x8+1)
log_x8 = log(x8+1)
qqnorm(log_x8, pch = 1, frame = FALSE)
qqline(log_x8, col = "steelblue", lwd = 2)
hist(log_x8, breaks = 30, freq = FALSE, main= "Histogram of log_x8", xlab = "log(x8+1)")
# log2(x8+1)
log_2_x8 = log2(x8+1)
qqnorm(log_2_x8, pch = 1, frame = FALSE)
qqline(log_2_x8, col = "steelblue", lwd = 2)
hist(log_2_x8, breaks = 30, freq = FALSE, main= "Histogram of log_2_x8", xlab = "log2(x8+1)")
# Normalize for box-cox
# install.packages('caret')
library(caret)
x8_norm = preProcess(as.data.frame(x8), method=c("range"))
x8_normalized = predict(x8_norm, as.data.frame(x8))
# Convert from Dataframe to list for lm(). Add 1 for BoxCox compatibility
x8_norm_list = (x8_normalized$x8 + 1)
library(MASS)
model = lm(x8_norm_list~1)
bc_x8 = boxcox(model)
x8_opt_power = bc_x8$x[which.max(bc_x8$y)]
x8_opt_power # -0.6262626 -> So round to -0.5 for a transformation of 1 / sqrt(x8)
hist(1 / sqrt(x8_norm_list), breaks = 30, freq = FALSE, main = "Histogram of BoxCox transformation of x8_normalized", xlab = "1 / sqrt(x8_normalized)")
#after all these transformations, it's decided that no transformation is necessary and the data was best originally 

#x9
x9 <- land_km2_10s
#first check the variable
hist(x9, breaks = 30, freq = FALSE, main = "Histogram of x9", 
     xlab = "x9")
#lets try a log function because the graph does not look normal
x9_log = log(x9)
hist(x9_log, breaks = 30, freq = FALSE, main = "Histogram of x9_log", 
     xlab = "x9_log")
#try the box cox to see what power would work best to fix the distribution 
bc_info_x9 <- boxcox(x9~1, lambda = seq(-5, 5, 1/10))
#try the cube and cubic functions 
x9_cube_root = pracma::nthroot(x9, 2) # cube root
x9_cubic_root = pracma::nthroot(x9, 3) # cubic root
hist(x9_cube_root, breaks = 30, freq = FALSE, main = "Histogram of x9_cube_root", 
     xlab = "x9_cube_root")
hist(x9_cubic_root, breaks = 30, freq = FALSE, main = "Histogram of x9_cubic_root", 
     xlab = "x9_cubic_root")
#nothing good is getting returned therefore try squaring everything because boxcox says squaring the function would help
x9sq = x9^2
hist(x9sq)
#squaring the function didn't help, try using the log function plus 1 to account for zeros in variable 
min(x9)
x9a = x9+1
logy1 <- log(x9a)
par(mfrow=c(1,1))
hist(logy1, freq=FALSE, main="Histogram of log(x9+1)",
     xlab = "log(x9+1) as Response")
bc_info_x9a <- boxcox(x9a~1, lambda = seq(-5, 5, 1/10))
#let's take a close look at the values in the x9 variable to see if there are any abnormalities 
table(x9)
#we see we have 587 variables with 0.2598 
#try using the dummy variable 
x9dummy <- as.integer(x9 > 0) 
#weird issue where if i put x9>0, gives weird histogram because there's no actual zeros but most of the variables are less than 0.26
table(x9dummy)
hist(x9dummy, breaks = 30, freq = FALSE, main = "Histogram of x9dummy", 
     xlab = "x9dummy")
#try using the cutoff of 0.255 for x9
x9dummy = as.integer(x9 > 0.255)
hist(x9dummy, breaks = 30, freq = FALSE, main = "Histogram of x9dummy", 
     xlab = "x9dummy")
# it looks like there is nothing left to try and we stick with the dummy variable of x9

#x10
x10 <- forest_km2_10s
# Trying various transformations resulted in having the cube-root, and square-root transformation
#   giving the best results. We cannot seem to get a normal-looking histogram, but we get close
#   with at least a symmetric looking histogram
#Square Root Transformation
hist(sqrt(x10), breaks = 30, freq = FALSE, main= "Histogram of sqrt(x10)", xlab = "sqrt(x10)")
#Cube Root Transformation
#install.packages("pracma")
library(pracma)
cube_rt_x10 = pracma::nthroot(x10, 3)
hist(cube_rt_x10, breaks = 30, freq = FALSE, main = "Histogram of cube-root(x10)", xlab = "cube_root(x10)")
# Let's see how a box-cox transformation fares for this
# min(x10) # [1] 0 -> Add 1
bc_x10 = boxcox( lm((x10)+1 ~ 1) )
x10_opt_power = bc_x10$x[which.max(bc_x10$y)]
x10_opt_power # Gives a lambda value of -2
hist( 1 / (x10 ^ 2), breaks = 30, freq = FALSE, main = "Histogram of BoxCox transformation of 1 / x10^2", xlab = "1 / x10^2")
# CONCLUSION: It appears the the cube-root gives the best transformation for x10

#x11 
current_explanatory = x11
break_amount = 30 # set the amount of breaks to use for the histogram
xc = x11
# Variable without transformation
hist(current_explanatory, breaks = break_amount, freq = FALSE, main= "Histogram of x11", xlab = "x11")
xcindex <- which(xc == 0)
xcindex
xcs <- length(xcindex)
xcs
#there is a large amount of zeros in the data 
#dummy variable would work best here 
xcdummy <- as.integer(xc > 0)
table(xcdummy)
#take the histogram 
hist(x1dummy, breaks = 30, freq = FALSE, main = "Histogram of x11dummy", 
     xlab = "x11dummy")
#once we take the dummy variable, there's nothing left to do

#x12 
current_explanatory = x12
break_amount = 30 # set the amount of breaks to use for the histogram
xc = x12
# Variable without transformation
hist(current_explanatory, breaks = break_amount, freq = FALSE, main= "Histogram of x12", xlab = "x12")
#Natural log Transformation (add small constant to input beforehand)
log_curr_expl = log(current_explanatory)
hist(log_curr_expl, breaks = break_amount, freq = FALSE, main= "Histogram of log(x12)", xlab = "log(x12)")
#looks normal but returns a lot of -inf when inputted back it
#there is a large amount of zeros so the dummy variable works best here 
xcdummy <- as.integer(xc > 0)
table(xcdummy)
#take the histogram 
hist(xcdummy, breaks = 30, freq = FALSE, main = "Histogram of x12dummy", 
     xlab = "x12dummy")
#once we take the dummy variable, there's nothing left to do

#x13
current_explanatory = x13
break_amount = 30 # set the amount of breaks to use for the histogram
xc = x13
# Variable without transformation
hist(current_explanatory, breaks = break_amount, freq = FALSE, main= "Histogram of x13", xlab = "xc")
#lot of zeros, therefore dummy variable works best here 
xcdummy <- as.integer(xc > 0)
table(xcdummy)
#take the histogram 
hist(xcdummy, breaks = 30, freq = FALSE, main = "Histogram of x13dummy", 
     xlab = "x13dummy")
#once we take the dummy variable, there's nothing left to do

#14
current_explanatory = x14
break_amount = 30 # set the amount of breaks to use for the histogram
xc = x14
# Variable without transformation
hist(current_explanatory, breaks = break_amount, freq = FALSE, main= "Histogram of x14", xlab = "x14")
#let's do the dummy variable on this since there is a lot of zeros in the data 
xcdummy <- as.integer(xc > 0)
table(xcdummy)
#take the histogram 
hist(xcdummy, breaks = 30, freq = FALSE, main = "Histogram of x14dummy", 
     xlab = "x14dummy")
#once we take the dummy variable, there's nothing left to do

#Model Selection Code: (along with residual plots)
#Lasso Analysis#
# LASSO Code taken from Section 1 (Instructor Provided Code), adapted to our project results.

#Load data set, and values
rm(list=ls())
data <- read.csv("jeju_data2model_pud_4WWU.csv",header <- TRUE)
attach(data)

# Use log-transformed y
y <- usdyav
y1 <- y+1
logy1 <- log(y1)

# Load variables
x1 <- commercial_km2
x2 <- culthistpl_km2
x3 <- naturalmon_km2
x4 <- road_moct_kms_100s
x5 <- sandbeach_km2
x6 <- seacliff_km2
x7 <- viewpoint
x8 <- airdist_kms_100s
x9 <- land_km2_10s
x10 <- forest_km2_10s
x11 <- athletic_km2_10s
x12 <- industrial_km2
x13 <- trails_osm_kms_100s
x14 <- nearroad_kms

#Use transformed variables
x1dummy = as.integer(x1 > 0)
x2dummy = as.integer(x2 > 0)
x3dummy = as.integer(x3 > 0)
x4sqrt = sqrt(x4)
x5dummy = as.integer(x5 > 0)
x6dummy = as.integer(x6 > 0)
x7dummy = as.integer(x7 > 0)
x8 # no transformation was found to be needed
x9dummy = as.integer(x9 > 0.2598)
cubertx10 = x10^(1/3)
x11dummy = as.integer(x11 > 0)
x12dummy = as.integer(x12 > 0)
x13dummy = as.integer(x13 > 0)
x14dummy = as.integer(x14 > 0)

#Multiple linear regression model 
model = lm(logy1 ~ x1dummy + x2dummy + x3dummy + x4sqrt + x5dummy + x6dummy + 
             x7dummy + x8 + x9dummy + cubertx10 + x11dummy + x12dummy + x13dummy + x14dummy ) 
summary(model)

#Perform variable selection (to be covered in Chapter 11)

#Run regression diagnostics by checking the residuals

# Here, the histogram shows a bell-like curve, centered around 0
#   Indication that we can assume normality. Neat.
resid <- model$residuals
hist(resid, freq = FALSE, main = "Model Residuals",
     xlab = "Residuals")

#Advanced topic (Lasso regression, a type of penalized least squares)
#The lasso regression performs variable selection and model estimation simultaneously.

#install.packages("glmnet")
library("glmnet")
#Using lasso
alpha = 1

#Save all the explanatory variables in the data frame.
xmat <- data.frame(x1dummy = x1dummy, x2dummy = x2dummy, x3dummy = x3dummy,  x4sqrt = x4sqrt,  x5dummy = x5dummy,
                   x6dummy = x6dummy,  x7dummy = x7dummy,  x8 = x8, x9dummy = x9dummy,
                   cubertx10 = cubertx10, x11dummy = x11dummy,  x12dummy =  x12dummy, x13dummy = x13dummy,  x14dummy = x14dummy)
# Call:
#   lm(formula = logy1 ~ x1dummy + x2dummy + x3dummy + x4sqrt + x5dummy + 
#        x6dummy + x7dummy + x8 + x9dummy + cubertx10 + x11dummy + 
#        x12dummy + x13dummy + x14dummy)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.98321 -0.20411 -0.05434  0.14841  1.88853 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13332    0.05716   2.332  0.01993 *  
#   x1dummy      0.32132    0.05806   5.534 4.22e-08 ***
#   x2dummy      0.02285    0.04057   0.563  0.57344    
# x3dummy      0.09165    0.03261   2.810  0.00507 ** 
#   x4sqrt       1.19198    0.18526   6.434 2.13e-10 ***
#   x5dummy      0.40964    0.06465   6.336 3.91e-10 ***
#   x6dummy      0.64575    0.07148   9.034  < 2e-16 ***
#   x7dummy      0.26154    0.04291   6.095 1.69e-09 ***
#   x8          -1.36031    0.15749  -8.637  < 2e-16 ***
#   x9dummy     -0.06652    0.05830  -1.141  0.25422    
# cubertx10    0.09826    0.06922   1.420  0.15610    
# x11dummy    -0.02504    0.03497  -0.716  0.47425    
# x12dummy    -0.03734    0.03270  -1.142  0.25373    
# x13dummy     0.23056    0.03026   7.620 7.08e-14 ***
#   x14dummy     0.22148    0.04879   4.540 6.49e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3552 on 809 degrees of freedom
# Multiple R-squared:  0.5004,	Adjusted R-squared:  0.4917 
# F-statistic: 57.87 on 14 and 809 DF,  p-value: < 2.2e-16

#Convert the data frame into a matrix.
xmat <- as.matrix(xmat)

#Fit the model
cvfit = cv.glmnet(xmat, logy1, alpha=alpha)

#lambda based on the 1se criterion
lambda_1se = cvfit$lambda.1se
lambda_1se # 0.03370218
# Saved lambda for result replication, May 19th 10:35 PM
lambda_1se = 0.03370218

#The ones with dots mean dropped variables. 
#The ones without dots mean estimated coefficients.
coef.lambda.1se <- coef(cvfit, s = lambda_1se)
coef.lambda.1se

# 15 x 1 sparse Matrix of class "dgCMatrix"
# s1
# (Intercept)     0.36221224
# x1dummy      0.35293730
# x2dummy         .         
# x3dummy      0.04298106
# x4sqrt            0.27607536
# x5dummy      0.29749123
# x6dummy      0.48906747
# x7dummy      0.20177644
# x8                 -1.12142004
# x9dummy        .         
# cubertx10        .         
# x11dummy       .         
# x12dummy       .         
# x13dummy    0.21020464
# x14dummy       .      

# Here, we drop x2, x9, x10, x11, x12, 14 for our model, indicated by LASSO
# This leads to the following least-squares equation: 
model2 = lm(logy1 ~ x1dummy + x3dummy + x4sqrt + x5dummy + x6dummy + x7dummy + x8  + x13dummy )
summary(model2)

# Call:
#   lm(formula = logy1 ~ x1dummy + x3dummy + x4sqrt + x5dummy + x6dummy + 
#        x7dummy + x8 + x13dummy)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.02820 -0.21702 -0.05332  0.14626  1.90236 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.32668    0.03433   9.515  < 2e-16 ***
#   x1dummy      0.37815    0.05598   6.756 2.70e-11 ***
#   x3dummy      0.11197    0.03061   3.658 0.000271 ***
#   x4sqrt       0.49319    0.10619   4.644 3.97e-06 ***
#   x5dummy      0.38986    0.06513   5.986 3.22e-09 ***
#   x6dummy      0.62365    0.07169   8.700  < 2e-16 ***
#   x7dummy      0.25523    0.04324   5.903 5.25e-09 ***
#   x8          -1.41850    0.15769  -8.996  < 2e-16 ***
#   x13dummy     0.22647    0.03015   7.511 1.55e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3593 on 815 degrees of freedom
# Multiple R-squared:  0.4849,	Adjusted R-squared:  0.4799 
# F-statistic: 95.92 on 8 and 815 DF,  p-value: < 2.2e-16


#Predicted values of y (y.hat) based on the x values in the data using lambda_1se.
fit = glmnet(xmat, logy1, alpha = alpha, nlambda = 20)
# head(fit)
y.hat = predict(fit,newx=xmat,s=lambda_1se)
# head(y.hat)

#Residuals (run the regression diagnostics based on this)
resid.lasso = y-y.hat
resid.lasso

# Histogram of Residuals. Here, we can see the residuals are skewed
hist(resid.lasso)

# The QQ-Plot also shows right-skewness.
# In the QQ-Plot, we see some hints of normality, as for a large part of QQ-Plot, we have
#   that our data points follow the line. And then at the end, the points start to deviate
#   from the line. 
qqnorm(resid.lasso, pch = 1, frame = FALSE)
qqline(resid.lasso, col = "steelblue", lwd = 2)

# Residual Plot of Model
# Note how the residual plot, too has a skewness to it 
plot(fitted(model2), resid.lasso)
abline(0,0)

# Studentized Residuals
# We see that there are quite a few points above the line y=3
# Also note a lack of outliers below the line y = -3
# These two observations can explain the right-skewness of the model
# y = b0 + b1x1 + b3x3 + b4x4 + b5x5 + b6x6 + b7x7 + b8x8 + b13x3 + error
library(MASS)
stud_resid = studres(model2)
plot(x1dummy + x3dummy + x4sqrt + x5dummy + x6dummy + x7dummy + x8  + x13dummy, stud_resid,  ylab='Studentized Resid', xlab='x1dummy + x3dummy + x4sqrt + x5dummy + x6dummy + x7dummy + x8  + x13dummy') 
abline(0,0)
abline(3,0)
abline(-3,0)

# Density Plot of Residuals
# We can also note the right skewness in this plot as well, which may be explained by the outliers seen in the studentized residuals
plot(density(resid.lasso))

# Residuals vs Fitted (Cook's Distance)
# 4th-plot : Residuals vs Fitted
#   We can notice the lack of change in spread as leverage increases
plot(model2, which = 4)
plot(model2, which = 5)



