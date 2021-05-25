#
# Author: Sandro O'Neill (aoneil02)
# IST 719
# Poster Project

# Data Sources:
# NYPD Data Set:
# https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc
# NYC Shapefiles:
# https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm



library(ggplot2)
library(dplyr)
#install.packages("rgdal")
library(rgdal)

###################
# Import and preprocessing
###################


# Define directory
my.dir <- "/Users/alexanderoneill/IST 719/Data/"
# Opens source file
nypd <- read.csv(paste0(my.dir,"NYPD_Arrest_Data__Year_to_Date_-2.csv"))
# subset into felony
felony <- nypd[nypd$LAW_CAT_CD=="F",]
nrow(felony) #65278
unique(felony$OFNS_DESC) #32 unique offenses (one blank offense description)
###################
# Heatmap
###################

# reading the new york boroughs boundary shapefile
boroughs <- readOGR(dsn=paste0(my.dir,"nybb.shp"),layer = "nybb")

# Create map using the shapefiles as polygons, then adds heatmap 
map <- ggplot() + 
  geom_polygon(data=boroughs,aes(x=long,y=lat,group=group),fill="grey40",alpha=1) +
  coord_equal(ratio=1) +
  geom_density2d(data=felony,aes(x=X_COORD_CD,y=Y_COORD_CD),size = 0.3) +
  stat_density2d(data=felony,aes(x=X_COORD_CD,y=Y_COORD_CD,fill=..level..,alpha=..level..),size=0.01,bins=16,geom="polygon") +
  scale_fill_gradient(low="green",high="red") +
  scale_alpha(range=c(0,0.3),guide=FALSE) +
  theme_minimal()
map

###################
# Gender Breakdown plot
###################


felony$PERP_SEX <- as.factor(felony$PERP_SEX)
gendertable <- table(felony$PERP_SEX)
genderpercent <- c(100*round(gendertable[1]/sum(gendertable),4),100*round(gendertable[2]/sum(gendertable),4))
genderpercent #percentage values for gender
pie(table(felony$PERP_SEX),col=c("Pink","Light Blue"),labels=genderpercent,main="Arrest Gender Breakdown")
# creates pie chart with percentage labels


###################
# Age breakdown plot
###################

#creating blue color scale
steelblues <- c("steelblue1","steelblue2","steelblue3","steelblue4","deepskyblue3")
felony$AGE_GROUP <- as.factor(felony$AGE_GROUP)
plot(felony$AGE_GROUP
     ,main = "Arrest Count by Age Group"
     ,xlab = "Age Group", ylab = "Arrest Count"
     ,ylim=c(0,10000)
     ,col=steeblues)
age.df <- as.data.frame(table(felony$AGE_GROUP))
age.df
colnames(age.df) <- c("Age_Group","Count")
# Curved bar plot version
ggplot(age.df) +
  aes(x=Age_Group,y=Count) +
  geom_bar(width=0.95,stat="identity",fill=steelblues) +
  coord_polar(theta="y") +
  ylim(c(0,46000)) + xlab("") + ylab("") +
  geom_text(data=age.df,hjust=1,size=6,aes(x=Age_Group,y=0,label=Age_Group)) +
  theme(legend.position="none",
        axis.text.y=element_blank()
        ,axis.ticks=element_blank()
       ,panel.background = element_blank())

###################
# Horizontal bar chart of arrests by borough
###################

blues <- c("deepskyblue3","steelblue1","steelblue2","steelblue3","steelblue4")
felony$ARREST_BORO <- as.factor(felony$ARREST_BORO)
table(felony$ARREST_BORO)
barplot(table(felony$LAW_CAT_CD,felony$ARREST_BORO),beside = T,horiz = T
        ,main = "Felony Arrests Across The Five Boroughs"
        ,xlab = "Number of Arrests",ylab = "Borough"
        ,col=blues)

###################
# Time series with monthly totals
###################

felony$ARREST_DATE <- as.Date(felony$ARREST_DATE,format="%m/%d/%y") #converts char to datetime
felony <- felony[order(felony$ARREST_DATE),] #orders by date
correct_order <-unique(months(felony$ARREST_DATE)) #saves correct monthly order in object

monthly <- as.data.frame(table(months(felony$ARREST_DATE))) #creates dataframe from monthly total tables

colnames(monthly) <- c("month","total") #changes column names
sum(monthly$total) == nrow(felony) #ensures total matches
monthly$month <- factor(monthly$month,levels=month.name)
monthly <- monthly[order(match(monthly$month,correct_order)),] #matches with correct_order object
monthly$month <- as.factor(monthly$month)
ts <- ggplot(monthly,aes(x=month,y=total,group=1)) + 
  geom_point() +
  geom_line(size=2,col="deepskyblue4") +
  ylim(2500,7000) +
  theme(panel.background = element_blank())
ts

