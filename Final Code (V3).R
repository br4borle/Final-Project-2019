install.packages("sf")
install.packages("spgwr")
install.packages("plyr")
install.packages("dplyr")
install.packages("spdep")
install.packages("GISTools")
install.packages("raster")
install.packages("maptools")
install.packages("rgdal")
install.packages("spatstat")
install.packages("tmap")
install.packages("gstat")
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("maps")
install.packages("bcmaps")
install.packages("tmap")
install.packages("fBasics")
install.packages('bcmapsdata', repos='https://bcgov.github.io/drat/')
install.packages("data.table")


library(sf)
library(spgwr)
library(plyr)
library(dplyr)
library(spdep)
library(GISTools)
library(raster)
library(maptools)
library(rgdal)
library(spatstat)
library(sp)
library(tmap)
library(gstat)
library("tidyverse")
library("lubridate")
library("grid")
library("gridExtra")
library("gtable")
library("maps")
library("bcmaps")
library("fBasics")
library("bcmapsdata")
library("data.table")

####

#Set working directory
setwd("Z:/Geog 418/Final Project/Working")

###Data Preparation

#Reading in particulate matter dataset
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data
#Select only columns 1 and 2
pm25 <- pm25[,1:2]
#Change the column names 
colnames(pm25) <- c("POSTALCODE", "PM25")
pm25 <- na.omit(pm25)

#Reading in postal code shapefile
postalcodes <- shapefile("BC_Postal_Codes") #Read in related postal code data

#Reading in dissemination tract and income data
income <- read.csv("Income.csv") #Read in census income data  
colnames(income) <- c("DAUID", "Income") #Select only ID and Income columns
census.tracts <- shapefile("BC_DA.shp") #Read in dissemination tract shapefile
income.tracts <- merge(census.tracts,income, by = "DAUID") #Merge income and dissemination data
nrow(income.tracts) #Determine the number of columns in the dataframe
income.tracts <- income.tracts[!is.na(income.tracts$Income),]

## Something Happened where we cut holes in our income tract shape file

class(income.tracts) ### Used in Moran's I - Step 1
plot(income.tracts)

#Create choropleth map of income
med.income <- income.tracts$Income
shades <- auto.shading(med.income, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.tracts, med.income,shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Median Income within Metro-Vancouver")
#income.tracts ### Use to set the coordinates of the legend


map_MedINCExp <- tm_shape(income.tracts) + 
  tm_polygons(col = "Income", 
              title = "Median Income within Metro-Vancouver", 
              style = "fisher", 
              palette = "-RdBu", n = 8)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_MedINCExp

png ("Map_ExploratoryMedIncome.png")
map_MedINCExp
dev.off()


png ("ExploratoryMedIncomeMapChloro.png")

choropleth(income.tracts, med.income,shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Median Income within Metro-Vancouver") #add a legend (you might need to change the location)

dev.off()

##Data Below Used in Step 2

#Select postal codes that fall within dissemination tracts)
postalcodes <- intersect(postalcodes,income.tracts)
plot(postalcodes) #See what the data looks like spatially
head(postalcodes) #See what the data looks like in tabular form

#Join PM2.5 data with postal code data
pm25.spatial <- merge(postalcodes,pm25,by = "POSTALCODE")
head(pm25.spatial)
class(pm25.spatial)

#Aggregate the PM2.5 values in each DA in order to have a single value per DA. Here we aggregate based on the max
pm25.aggregate <- aggregate((as.numeric(pm25.spatial$PM25)/10)~pm25.spatial$DAUID,FUN=max)
class(pm25.aggregate)

#Re-join aggregated data to the income.tracts layer.
colnames(pm25.aggregate) <- c("DAUID", "PM25AGG") #Select only ID and Income columns
income.pm25 <- merge(income.tracts,pm25.aggregate, by = "DAUID") #Merge income and dissemination data
class(income.pm25)
head(income.pm25)

PM25AGG.df <- income.pm25$PM25AGG
PM25AGG.df <- na.omit(PM25AGG.df)
shades <- auto.shading(PM25AGG.df, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.pm25, PM25AGG.df,shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Max PM25AGG within Metro-Vancouver")

map_PM25Exp <- tm_shape(income.pm25) + 
  tm_polygons(col = "PM25AGG", 
              title = "Max PM25AGG within Metro-Vancouver", 
              style = "fisher", 
              palette = "YlOrRd", n = 8)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_PM25Exp

png ("Map_ExploratoryPM25AGGMap.png")
map_PM25Exp
dev.off()


png ("ExploratoryPM25AGGMapChloro.png")
choropleth(income.pm25, PM25AGG.df,shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Max PM25AGG within Metro-Vancouver")
dev.off()

#Re-join aggregated data to the pm25.spatial points layer.
pm25.points.aggregate <- merge(pm25.spatial, pm25.aggregate, by = "DAUID")
head(pm25.points.aggregate)
class(pm25.points.aggregate)

#Create a subsample of the datapoints provided in the PM2.5 dataset using the sample n provided on CourseSpaces
set.seed(110)
sampleSize=110 ## Based on Excel File
spSample <- pm25.points.aggregate[sample(1:length(pm25.points.aggregate),sampleSize),]
plot(spSample)

class(spSample)

head(spSample)

spSampleMap <- tm_shape(income.pm25) + 
  tm_polygons() +
  tm_shape(spSample) +
  tm_dots(col="PM25AGG", palette = "YlOrRd", 
          title="Sampled PM2.5 \n(in ug/m3)", size=0.7) + 
  tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

spSampleMap

png("spSampleExploreMap.png")
spSampleMap
dev.off()

##########################

USE_LAND <- shapefile("LandDesignation")

map_LUExp <- tm_shape(USE_LAND) + 
  tm_polygons(col = "RGSDesigna", 
              title = "Land Use within Metro-Vancouver", 
              style = "fisher", 
              palette = "Set1", n = 8)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_LUExp

png ("LU_EXP_MAP.png")
map_LUExp
dev.off()


###Discriptive Stats

#Mean
meanPM <- mean(spSample$PM25AGG, na.rm = TRUE) #Use na.rm = TRUE to ignore NA values in calculation
meanINC <- mean(income.tracts$Income, na.rm = TRUE)

#Standard Deviation
sdPM<- sd(spSample$PM25AGG, na.rm = TRUE) #Calculate the SD, ignoring NA values
sdINC <- sd(income.tracts$Income, na.rm = TRUE) #Calculate the SD, ignoring NA values only for the summer months

#Mode
modePM <- as.numeric(names(sort(table(spSample$PM25AGG), decreasing = TRUE))[1]) #make frequency table of fire size variable and sort it in desending order and extract the first row (Most Frequent)
modeINC <- as.numeric(names(sort(table(income.tracts$Income), decreasing = TRUE))[1])

#Median
medPM <- median(spSample$PM25AGG, na.rm = TRUE)
medINC <- median(income.tracts$Income, na.rm = TRUE)

#Skewness
skewPM <- skewness(spSample$PM25AGG, na.rm = TRUE)[1]
skewINC <- skewness(income.tracts$Income, na.rm = TRUE)[1]

#Kurtosis
kurtPM <- kurtosis(spSample$PM25AGG, na.rm = TRUE)[1]
kurtINC <- kurtosis(income.tracts$Income, na.rm = TRUE)[1]

#CoV
CoVPM <- (sdPM / meanPM) * 100
CoVINC <- (sdINC / meanINC) * 100

#Normal distribution test
normPM_PVAL <- shapiro.test(spSample$PM25AGG)$p.value
normINC_PVAL <- shapiro.test(income.tracts$Income)$p.value

#####
#Create a table of descriptive stats

samples = c("PM2.5AGG", "Median Income") #Create an object for the labels
means = c(meanPM, meanINC) #Create an object for the means
sd = c(sdPM, sdINC) #Create an object for the standard deviations
median = c(medPM, medINC) #Create an object for the medians
mode <- c(modePM, modeINC) #Create an object for the modes
skewness <- c(skewPM, skewINC) #Create an object for the skewness
kurtosis <- c(kurtPM, kurtINC) #Create an object for the kurtosis
CoV <- c(CoVPM, CoVINC) #Create an object for the CoV
normality <- c(normPM_PVAL, normINC_PVAL) #Create an object for the normality PVALUE

data.for.table1 = data.frame(samples, means, median, mode, normality)
data.for.table2 = data.frame(samples, sd, skewness, kurtosis, CoV)

#Make table 1
table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 1: Discriptive Statistics PM2.5 and Median Income", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)


table2 <- tableGrob(data.for.table2, rows = c("",""))
t2Caption <- textGrob("Table 2: Discriptive Statistics PM2.5 and Median Income (Cont.)", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)



grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)

#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("Output_Table1.png") #Create an object to print the table to
grid.arrange(table1, newpage = TRUE)
dev.off() #Print table

png("Output_Table2.png") #Create an object to print the table to
grid.arrange(table2, newpage = TRUE) #Create table
dev.off()

#####
#Create and Print a histogram
png("Output_HistogramPM.png")
hist(spSample$PM25AGG, breaks = 30, main = "PM 2.5 Concentration HISTOGRAM", xlab = "concentration (ppm)") #Base R style
dev.off()

png("Output_HistogramINC.png")
hist(spSample$Income, breaks = 30, main = "Median Income HISTOGRAM", xlab = "Median Income") #Base R style
dev.off()

spSample.df <-data.frame(spSample)

histogramPM <- ggplot(spSample.df, aes(x = PM25AGG)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "white") + #make histogram with 30 bins, black outline, white fill
  labs(title = "PM 2.5 Concentration", x = "PM 2.5 Concentration", y = "Frequency", caption = "Figure 1:PM 2.5 Concentration HISTOGRAM for the Vancover Region") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0,30, by = 2))+scale_x_continuous(breaks = seq(0,10, by = 1)) # set y axis labels to 0 - 700 incrimenting by 100

histogramPM

png("Output_Histogram_ggplotPM.png")
histogramPM
dev.off()

histogramINC <- ggplot(spSample.df, aes(x = Income)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "white") + #make histogram with 30 bins, black outline, white fill
  labs(title = "Median Income", x = "Median Income", y = "Frequency", caption = "Figure 2:Median Income HISTOGRAM for the Vancover Region") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 18, by = 2)) # set y axis labels to 0 - 700 incrimenting by 100

histogramINC

png("Output_Histogram_ggplotINC.png")
histogramINC
dev.off()

#Lets put it all together
pdf("FinalProject_Figures_and_Tables.pdf", onefile = TRUE)
grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)
histogramINC
histogramPM
dev.off()


###################################################

### Step 1: Moran's I Median Income


crd.nb <- poly2nb(income.tracts) ## Queens Case
crd.net <- nb2lines(crd.nb,coords=coordinates(income.tracts))


#Queens <-tm_shape(income.tracts) + tm_borders(col='lightgrey') + tm_shape(crd.net) + tm_lines(col='blue')
#png ("IncomeQueenCase.png")
#Queens
#dev.off()

#Weight Matrix

crd.lw <- nb2listw(crd.nb, zero.policy = TRUE, style = "W") ##Queens Case
print.listw(crd.lw, zero.policy = TRUE)

#Lagged Means

income.tracts$IncLagMeans = lag.listw(crd.lw, income.tracts$Income, zero.policy = TRUE)


map_LagMean <- tm_shape(income.tracts) + 
  tm_polygons(col = "IncLagMeans", 
              title = "Median Income\nLagged Means", 
              style = "fisher", 
              palette = "YlOrRd", n = 7)+tm_layout(legend.outside = TRUE, legend.frame = TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_LagMean

png ("IncomeLaggedMeans.png")
map_LagMean
dev.off()

########################
mi <- moran.test(income.tracts$Income, crd.lw, zero.policy = TRUE)
mi

mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]

z <- (mI-eI)/sqrt(var)
zAct <- z

mI <- round(mI, digits = 5)
eI <- round(eI, digits = 5)
var <- round(var, digits = 5)
z <- round(z, digits = 5)


data.for.table3 = data.frame(mI, eI, var, z)


#Make table 1
table3 <- tableGrob(data.for.table3) #make a table "Graphical Object" (GrOb) 
t3Caption <- textGrob("Table 3: Median Income Global Moran's I Outputs", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table3 <- gtable_add_rows(table3, 
                          heights = grobHeight(t3Caption) + padding, 
                          pos = 0)

table3 <- gtable_add_grob(table3,
                          t3Caption, t = 1, l = 2, r = ncol(data.for.table3) + 1)


grid.arrange(table3, newpage = TRUE)

#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("MedianIncOutput_GlobalMoransI.png") #Create an object to print the table to
grid.arrange(table3, newpage = TRUE)
dev.off() #Print table


########################  

lisa.test <- localmoran(income.tracts$Income, crd.lw)

lisa.test

income.tracts$Ii <- lisa.test[,1]
income.tracts$E.Ii<- lisa.test[,2]
income.tracts$Var.Ii<- lisa.test[,3]
income.tracts$Z.Ii<- lisa.test[,4]
income.tracts$P<- lisa.test[,5]

########################

map_LISA <- tm_shape(income.tracts) + 
  tm_polygons(col = "Ii", 
              title = "Local Moran's I Median Income", 
              style = "fisher", 
              palette = "YlOrRd", n = 7)+tm_layout(legend.outside = TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_LISA

png ("MapLISA.png")
map_LISA
dev.off()


map_LISAP <- tm_shape(income.tracts) + 
  tm_polygons(col = "P", 
              title = "Median Income Local Moran's I P-Val", 
              style = "fixed", breaks =c(0,0.05,0.051,0.1,1), 
              palette = "Set2", n = 4)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_LISAP

png ("MapLISAP.png")
map_LISAP
dev.off()


########################

png ("LISAPlot.png")
moran.plot(income.tracts$Income, crd.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Median Income", 
           ylab="Spatially Lagged Median Income", quiet=NULL)
dev.off()

##########################################################

### Step 2: Spatial Interpolation Pm2.5

grd <- as.data.frame(spsample(income.tracts, "regular", n=70000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(spSample)

spSample$X <- coordinates(spSample)[,1] ## Storing X-coordinates
spSample$Y <- coordinates(spSample)[,2] ## Storing Y-coordinates

##Spatial Interpolation with Kriging (Universal) 
## Removes a defined trend, fits surface to residuals then adds the trend plus surface together to create a final output

f.1 <- as.formula(PM25AGG ~ X + Y) 
f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))


var.smpl <- variogram(f.1, spSample, cloud = FALSE) #, cutoff=1000000, width=89900)

dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = TRUE,
                          vgm(psill=2.3, model="Exp", range=15, nugget=0))

dat.fitOpt <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                            vgm(model="Exp"))

var.smpl.Two <- variogram(f.2, spSample, cloud = FALSE) #, cutoff=1000000, width=89900)


dat.fit.Two <- fit.variogram(var.smpl.Two, fit.ranges = FALSE, fit.sills = FALSE,
                             vgm(psill=0.7, model="Sph", range = 7, nugget = 0))
plot(var.smpl, dat.fit)

plot(var.smpl, dat.fitOpt)

plot(var.smpl.Two, dat.fit.Two)

png("UKVariogram.png")
plot(var.smpl, dat.fit)
dev.off()

png("UKOptVariogram.png")
plot(var.smpl, dat.fitOpt)
dev.off()

png("UKVariogram.Two.png")
plot(var.smpl.Two, dat.fit.Two)
dev.off()

###Changin the F.1 to F.2 changes the polynomial rom first order to second
### Sec.poly is a poor model

# Define the trend model
f.1 <- as.formula(PM25AGG ~ X + Y)  

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, spSample, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
rUK <- raster(dat.krg)
rUK.m <- mask(rUK, income.tracts)

# Plot the map
UKMAP<-(tm_shape(rUK.m) + 
          tm_raster(n=10,palette = "-RdYlBu",  
                    title="Predicted PM 2.5 \n(in ug/m3)") +
          tm_shape(spSample) + tm_dots(size=0.2) +
          tm_legend(legend.outside=TRUE))+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

UKMAP

png ("UKMAP.png")
UKMAP
dev.off()

##Minor Error in Predicted values - Possible negative values +
##made possible because the method uses probability to estimate values which may extent the maximum and minimum of the data set.

rUKV   <- raster(dat.krg, layer="var1.var")
rUKV.m <- mask(rUKV, income.tracts)

UKVAR<-(tm_shape(rUKV.m) + 
          tm_raster(n=7, palette ="Reds",
                    title="Variance map \n(in ug/m3)") +tm_shape(spSample) + tm_dots(size=0.2) +
          tm_legend(legend.outside=TRUE))+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

UKVAR

png ("UKVAR.png")
UKVAR
dev.off()

rUKC   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
rUKC.m <- mask(rUKC, income.tracts)
UKCI<-(tm_shape(rUKC.m) + 
         tm_raster(n=7, palette ="Reds",
                   title="95% CI map \n(in ug/m3)") +tm_shape(spSample) + tm_dots(size=0.2) +
         tm_legend(legend.outside=TRUE))+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

UKCI

png ("UKCI.png")
UKCI
dev.off()

######################################################################

### Step 3: Regression MedInc and PM2.5

#These steps will help you combine the outputs from your spatial interpolation with your income data.

### The use of the rUK b/c the PM25 appeared to exhibit an inverse linear trend from west to east.+
#There was a potential for a polynomial trend but the east side of the study was limited in representation. The use of this trend is to remain conservative.

#If you have too many cells, you can reduce the number by aggregating values
#step.1 <- aggregate(rUK.m, fact=1, fun=max) ## Can change the rUK.m to rOK.m for the OK raster

#Convert the raster dataset to points
step.2 <-  rasterToPoints(rUK.m,fun=NULL, spatial=TRUE, crs=crs(pm25.spatial))

class(step.2)

crs(step.2)

step.2 <- as.data.frame(step.2) #convert the point dataset to a spatial dataframe

Coords <- step.2[,c("x", "y")]  #assign coordinates to a new object

head(step.2)

crs <- crs(census.tracts) #utilize an existing projection

step.3 <- SpatialPointsDataFrame(coords = Coords, data = step.2, proj4string = crs) #create a spatial points dataframe

head(step.3)
step.4 <- aggregate(x=step.3,by=income.tracts, FUN=mean) #aggregate points into census tracts

crs(step.4)
st_crs(step.4)

step.5 <- intersect(step.4,income.tracts)  #get the intersection of step.4 with the income.tracts dataset (this will take a while) 

head(step.5)
class(step.5)

pm.income.poly <- step.5

pm.income.poly$PM25 <- pm.income.poly$var1.pred

sum(is.na(pm.income.poly$PM25))

pm.income.poly <- pm.income.poly[!is.na(pm.income.poly$PM25),]
##This line helps me remove the NA values so I can use the SPDF in the next step.  
sum(is.na(pm.income.poly$PM25))

#You are now ready to perform a regression

#Let's say your dataset with both PM2.5 and Income are stored in a dataset called pm.income.poly.

#Plot income and PM2.5 from the pm.income.poly dataset you created
plot(pm.income.poly$PM25~pm.income.poly$Income)

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
#pm.income.poly <-  pm.income.poly[pm.income.poly$PM25 != 0, ]
#Kept the zeros because there would be regions were the PM25 would be zero. The atmosphere is dynamic and it would introduce bias in to the trend.

#Now plot the data again
#plot(pm.income.poly$Income~pm.income.poly$PM25)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(pm.income.poly$PM25~pm.income.poly$Income,na.action = na.exclude) 
#income determines where sources are positioned in the landscape. PM2.5 is dependent on Median Income
#when na.exclude is used the residuals and predictions are padded to the correct length by inserting NAs for cases omitted by na.exclude

#Add the regression model to the plot you created
abline(lm.model)

#Get the summary of the results
summary(lm.model)

LR.Pval <- 2.2e-16
LR.R2 <- 0.02083

data.for.table11 = data.frame(LR.Pval, LR.R2)


#Make table 1
table11 <- tableGrob(data.for.table11) #make a table "Graphical Object" (GrOb) 
t11Caption <- textGrob("Table 11: Linear Regression Output", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table11 <- gtable_add_rows(table11, 
                          heights = grobHeight(t11Caption) + padding, 
                          pos = 0)

table11 <- gtable_add_grob(table11,
                          t11Caption, t = 1, l = 2, r = ncol(data.for.table11) + 1)


grid.arrange(table11, newpage = TRUE)

#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("LinearRegressionOutput.png") #Create an object to print the table to
grid.arrange(table11, newpage = TRUE)
dev.off() #Print table

#You want to determine if the model residuals are spatially clustered. 
#First obtain the residuals from the model
model.resids <- as.data.frame(residuals.lm(lm.model))
#Then add the residuals to your spatialpolygon dataframe
pm.income.poly$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(pm.income.poly)

#Now create choropleth map of residuals
resids <- pm.income.poly$residuals
#resids <- na.omit(resids)
shades <- auto.shading(resids, n=6, cols = brewer.pal(6, 'Greens'))
choropleth(income.tracts, resids, shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Residuals") #add a legend (you might need to change the location)

map_Res <- tm_shape(pm.income.poly) +
  tm_polygons(col = "residuals",
              title = "PM2.5 and Income Regression Residuals",
              style = "fisher",
              palette = "-RdBu", n = 6)+tm_layout(legend.outside = TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_Res

png ("MapNormalRes.png")
map_Res
dev.off()

png("RegressionResidualsChloro.png")

shades <- auto.shading(resids, n=6, cols = brewer.pal(6, 'Greens'))
choropleth(income.tracts, resids, shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Residuals") #add a legend (you might need to change the location)
dev.off()

#######################################################

### Step 4: Global/Local Moran's I Residuals

crd.nbR <- poly2nb(pm.income.poly) ## Queens Case
crd.netR <- nb2lines(crd.nbR,coords=coordinates(pm.income.poly))


#Weight Matrix

crd.lwR <- nb2listw(crd.nbR, zero.policy = TRUE, style = "W") ##Queens Case
print.listw(crd.lwR, zero.policy = TRUE)

########################
mi <- moran.test(pm.income.poly$residuals, crd.lwR, zero.policy = TRUE, na.action = na.exclude)
mi


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]

z <- (mI-eI)/sqrt(var)
zActR <- z

mI <- round(mI, digits = 5)
eI <- round(eI, digits = 5)
var <- round(var, digits = 5)
z <- round(z, digits = 5)


data.for.table4 = data.frame(mI, eI, var, z)


#Make table 1
table4 <- tableGrob(data.for.table4) #make a table "Graphical Object" (GrOb) 
t4Caption <- textGrob("Table 4: Residuals Global Moran's I Outputs", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table4 <- gtable_add_rows(table4, 
                          heights = grobHeight(t4Caption) + padding, 
                          pos = 0)

table4 <- gtable_add_grob(table4,
                          t4Caption, t = 1, l = 2, r = ncol(data.for.table4) + 1)


grid.arrange(table4, newpage = TRUE)

#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("ResidualsOutput_GlobalMoransI.png") #Create an object to print the table to
grid.arrange(table4, newpage = TRUE)
dev.off() #Print table


########################  

lisa.test <- localmoran(pm.income.poly$residuals, crd.lwR, na.action = na.exclude)

lisa.test

pm.income.poly$Ii <- lisa.test[,1]
pm.income.poly$E.Ii<- lisa.test[,2]
pm.income.poly$Var.Ii<- lisa.test[,3]
pm.income.poly$Z.Ii<- lisa.test[,4]
pm.income.poly$P<- lisa.test[,5]
########################

map_LISARes <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "Ii", 
              title = "Residual Local Moran's I", 
              style = "fisher", 
              palette = "YlOrRd", n = 8)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_LISARes

png ("MapLISARes.png")
map_LISARes
dev.off()

map_LISAPRes <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "P", 
              title = "Residuals Local Moran's I P-Val", 
              style = "fixed", breaks =c(0,0.05,0.051,0.1,1), 
              palette = "Set2", n = 4)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_LISAPRes

png ("MapLISAPRes.png")
map_LISAPRes
dev.off()
########################


png ("LISAPlotRes.png")
moran.plot(pm.income.poly$residuals, crd.lwR, zero.policy=NULL, spChk=NULL, labels=NULL,xlab="Residuals", 
           ylab="Spatially Lagged Residuals", quiet=NULL)
dev.off()

###############################################################

### Step 5: Geographically Weighted Regression - MedInc and PM2.5

#Let's say you are continuing with your data from the regression analysis. 

#The first thing you need to do is to add the polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the "coordinates" function from the sp library
pm.income.poly.coords <- sp::coordinates(pm.income.poly)
#Observe the result
head(pm.income.poly.coords)
#Now add the coordinates back to the spatialpolygondataframe
pm.income.poly$X <- pm.income.poly.coords[,1]
pm.income.poly$Y <- pm.income.poly.coords[,2]
head(pm.income.poly)

##Be Careful of the Lower Case x,y -> Not the Same as Upper Case X,Y

###Determine the bandwidth for GWR: this will take a while
GWRbandwidth <- gwr.sel(pm.income.poly$PM25~pm.income.poly$Income, 
                        data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model = gwr(pm.income.poly$PM25~pm.income.poly$Income, 
                data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model


#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Resdisuals
pm.income.poly$GWR.RES <- results$gwr.e

map_GWR_Res <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "GWR.RES", 
              title = "PM2.5 and Med.Income GWR Residuals", 
              style = "fisher", 
              palette = "-RdBu", n = 6)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_GWR_Res

png ("Map_GWR_RES.png")
map_GWR_Res
dev.off()


#Now for the magic. Let's add our local r-square values to the map
pm.income.poly$localr <- results$localR2

#Create choropleth map of r-square values
local.r.square <- pm.income.poly$localr
shades <- auto.shading(local.r.square, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.tracts, local.r.square, shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Local r-squared values") #add a legend (you might need to change the location)

map_GWRr2 <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "localr", 
              title = "PM2.5 and Med.Income GWR R2 Values", 
              style = "fisher", 
              palette = "viridis", n = 7)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_GWRr2

png ("Map_GWRr2.png")
map_GWRr2
dev.off()

png ("LocalR2GWRChloro.png")
choropleth(income.tracts, local.r.square, shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Local r-squared values") #add a legend (you might need to change the location)
dev.off()

head(results)

#Time for more magic. Let's map the coefficients
pm.income.poly$coeff <- results$pm.income.poly.Income

head(pm.income.poly)

#Create choropleth map of the coefficients
local.coefficient <- pm.income.poly$coeff
shades <- auto.shading(local.coefficient, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.tracts, local.coefficient, shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 1, title = "GWR Coefficent Values") #add a legend (you might need to change the location)

map_GWRcoeff <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "coeff", 
              title = "PM2.5 and Med.Income GWR Coefficient Values", 
              style = "fisher", 
              palette = "-RdBu", n = 8)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_GWRcoeff

png ("Map_GWRcoeff.png")
map_GWRcoeff
dev.off()

png ("CoefficentGWRChloro.png")
choropleth(income.tracts, local.coefficient, shades) #map the data with associated colours
choro.legend(-122.8, 49.495, shades, cex = 0.67, title = "Coefficent Values") #add a legend (you might need to change the location)
dev.off()

#Standard Error

pm.income.poly$coeff.SE <- results$pm.income.poly.Income_se

head(pm.income.poly)

map_GWRcoeff_SE <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "coeff.SE", 
              title = "PM2.5 and Med.Income GWR Coefficient Standard Error", 
              style = "fisher", 
              palette = "YlOrRd", n = 8)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_GWRcoeff_SE

png ("Map_GWRcoeff_SE.png")
map_GWRcoeff_SE
dev.off()

#Significance

dfree<-gwr.model$results$edf

pm.income.poly$t.ratio <- gwr.model$SDF$pm.income.poly.Income/gwr.model$SDF$pm.income.poly.Income_se

pm.income.poly$P.Val<-2*pt(-abs(pm.income.poly$t.ratio), dfree)

map_GWR_PVal <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "P.Val", 
              title = "PM2.5 and Med.Income GWR PVal", 
              style = "fixed", breaks =c(0,0.05,0.051,0.1,1), 
              palette = "Set2", n = 5)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
  tm_scale_bar(position=c("right", "top"))

map_GWR_PVal

png ("Map_GWR_PVal.png")
map_GWR_PVal
dev.off()

# sigTest = abs(gwr.model$SDF$pm.income.poly.Income) -2 * gwr.model$SDF$pm.income.poly.Income_se 
# 
# pm.income.poly$GWRMdIncSig<-sigTest
# 
# #pm.income.poly@data
# 
# map_GWR_SE <- tm_shape(pm.income.poly) + 
#   tm_polygons(col = "GWRMdIncSig", 
#               title = "PM2.5 and Med.Income GWR Standard Error", 
#               style = "fisher", 
#               palette = "-RdBu", n = 8)+tm_legend(legend.outside=TRUE)+tm_compass(position = "left")+
#   tm_scale_bar(position=c("right", "top"))
# 
# map_GWR_SE
# 
# png ("Map_GWR_SE.png")
# map_GWR_SE
# dev.off()


### Step 6: Point Pattern Analysis - Validate subset

##Spatial Pattern Analysis of Subset

head(spSample)

plot(spSample)

###Reminder(occurred in Poly Trends):
#spSample$X <- coordinates(spSample)[,1] ## Storing X-coordinates
#spSample$Y <- coordinates(spSample)[,2] ## Storing Y-coordinates

#remove duplicates
spSample <- remove.duplicates(spSample)

#create an "extent" object which can be used to create the observation window for spatstat
spSample.ext <- as.matrix(extent(income.tracts)) 

#observation window
window2 <- as.owin(list(xrange = spSample.ext[1,], yrange = spSample.ext[2,]))

#create ppp object from spatstat
spSample.ppp <- ppp(x = spSample$X, y = spSample$Y, window = window2)

plot(spSample.ppp)

##QUADRAT ANALYSIS###

##First, determine the number of qusdrats 
quads <- 11

qcount <- quadratcount(spSample.ppp, nx = quads, ny = quads)
nrow(qcount)

plot(spSample.ppp, pch = "+", cex = 0.5)
plot(qcount, add = T, col = "red")

qcount.df <- as.data.frame(qcount)

##Second, count the number of quadrats with a distinct number of points.
qcount.df <- plyr::count(qcount.df,'Freq')

##Change the column names so that x=number of points and f=frequency of quadrats with x point.
colnames(qcount.df) <- c("x","f")


sum.f.x2 <- sum((qcount.df$f)*((qcount.df$x)^2))
sum.f.x2  

M <- 11^2 

N <- 110 

sum.fx.2 <- (sum((qcount.df$f)*(qcount.df$x)))^2 

VAR <- (sum.f.x2-(sum.fx.2/M))/(M-1)

MEAN <- N/M

VMR <- VAR/MEAN 

##Finally, perform the test statistic to test for the existence of a random spatial pattern.
chi.square = VMR*(M-1)
p = 1 - pchisq(chi.square, (M - 1))

VAR <- round(VAR, digits = 5)

MEAN <- round(MEAN, digits = 5)

VMR <- round(VMR, digits = 5)

Quantity.of.Quadrats <- M
Quantity.of.Samples <-N


data.for.table5 = data.frame(M,N,sum.f.x2,sum.fx.2,VAR,MEAN,VMR)
data.for.table6 = data.frame(p)

#Make table 1
table5 <- tableGrob(data.for.table5) #make a table "Graphical Object" (GrOb) 
t5Caption <- textGrob("Table 5: SubSample Quadrat Analysis Outputs", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table5 <- gtable_add_rows(table5, 
                          heights = grobHeight(t5Caption) + padding, 
                          pos = 0)

table5 <- gtable_add_grob(table5,
                          t5Caption, t = 1, l = 2, r = ncol(data.for.table5) + 1)


grid.arrange(table5, newpage = TRUE)

table6 <- tableGrob(data.for.table6) #make a table "Graphical Object" (GrOb) 
t6Caption <- textGrob("Table 5: SubSample Quadrat Analysis Outputs (Cont.)", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table6 <- gtable_add_rows(table6, 
                          heights = grobHeight(t6Caption) + padding, 
                          pos = 0)

table6 <- gtable_add_grob(table6,
                          t6Caption, t = 1, l = 2, r = ncol(data.for.table6) + 1)


grid.arrange(table6, newpage = TRUE)

#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("Subset Quadrat Analysis Outputs.png") #Create an object to print the table to
grid.arrange(table5, newpage = TRUE)
dev.off() #Print table

png("Subset Quadrat Analysis Outputs2.png") #Create an object to print the table to
grid.arrange(table6, newpage = TRUE)
dev.off() #Print table

