setwd("C:/NYBackup/Fish/Neversink&ADK_TempVariability_2016")
TempVar <- read.csv("C:/NYBackup/Fish/Neversink&ADK_TempVariability_2016/TempVarDataForR_2-22-18.csv")

table(TempVar$Site)


#Create new variables with shorter names
TempVar$CDensArea <- TempVar$Community.density..No.0.1ha..by.area 
TempVar$CDensLength <- TempVar$Community.density..No..100.m..by.length
TempVar$CBiomArea <- TempVar$Community.biomass..g.0.1ha..by.area
TempVar$CBiomLength <- TempVar$Community.biomass..g.100.m..by.length
TempVar$CRich <- TempVar$Community.richness
TempVar$StDensArea <- TempVar$ST.Density..No.0.1.ha..by.area
TempVar$StDensLength <- TempVar$brook.trout.Density..No.100m..by.length
TempVar$StBiomArea <- TempVar$ST.Biomass..g.0.1.ha..by.area
TempVar$StBiomLength <- TempVar$brook.trout.Biomass..g.100m..by.length
TempVar$AllSpFPDensArea <- TempVar$First.Pass.All.Species.density..No.0.1ha..by.area
TempVar$AllSpFPDensLength <- TempVar$First.Pass.All.Species.density..No.100m..by.length

#This code was adapted from my Honnedaga Inverts graphing script
#Helper function from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
#to produce summary stats (including SEs) for the dataset
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
#I use it here to create new dataframes that summarizes each candidate metric 
Sum_CDensArea <- summarySE(TempVar, measurevar="CDensArea", groupvars=c("Site"))
Sum_CDensLength <- summarySE(TempVar, measurevar="CDensLength", groupvars=c("Site"))
Sum_CBiomArea <- summarySE(TempVar, measurevar="CBiomArea", groupvars=c("Site"))
Sum_CBiomLength <- summarySE(TempVar, measurevar="CBiomLength", groupvars=c("Site"))
Sum_CRich <- summarySE(TempVar, measurevar="CRich", groupvars=c("Site"))
Sum_StDensArea <- summarySE(TempVar, measurevar="StDensArea", groupvars=c("Site"))
Sum_StDensLength <- summarySE(TempVar, measurevar="StDensLength", groupvars=c("Site"))
Sum_StBiomArea <- summarySE(TempVar, measurevar="StBiomArea", groupvars=c("Site"))
Sum_StBiomLength <- summarySE(TempVar, measurevar="StBiomLength", groupvars=c("Site"))
Sum_AllSpFPDensArea <- summarySE(TempVar, measurevar="AllSpFPDensArea", groupvars=c("Site"))
Sum_AllSpFPDensLength <- summarySE(TempVar, measurevar="AllSpFPDensLength", groupvars=c("Site"))


#Here I add the coefficient of variance to each data frame by dividing the SD of the mean by the mean itself
Sum_CDensArea$CDensAreaCV <- Sum_CDensArea$sd / Sum_CDensArea$CDensArea
Sum_CDensLength$CDensLengthCV <- Sum_CDensLength$sd / Sum_CDensLength$CDensLength
Sum_CBiomArea$CBiomAreaCV <- Sum_CBiomArea$sd / Sum_CBiomArea$CBiomArea 
Sum_CBiomLength$CBiomLengthCV <- Sum_CBiomLength$sd / Sum_CBiomLength$CBiomLength 
Sum_CRich$CRichCV <- Sum_CRich$sd / Sum_CRich$CRich
Sum_StDensArea$StDensAreaCV <- Sum_StDensArea$sd / Sum_StDensArea$StDensArea
Sum_StDensLength$StDensLengthCV <- Sum_StDensLength$sd / Sum_StDensLength$StDensLength
Sum_StBiomArea$StBiomAreaCV <- Sum_StBiomArea$sd / Sum_StBiomArea$StBiomArea
Sum_StBiomLength$StBiomLengthCV <- Sum_StBiomLength$sd / Sum_StBiomLength$StBiomLength
Sum_AllSpFPDensArea$AllSpFPDensAreaCV <- Sum_AllSpFPDensArea$sd / Sum_AllSpFPDensArea$AllSpFPDensArea
Sum_AllSpFPDensLength$AllSpFPDensLengthCV <- Sum_AllSpFPDensLength$sd / Sum_AllSpFPDensLength$AllSpFPDensLength


#Merges all the separate summary dataframes together on the field "site", warning message seems unimportant
  #because I'm not using any of the columns with the duplicated names
All <- merge(Sum_CDensArea, Sum_CDensLength, by="Site")
All <- merge(All, Sum_CBiomArea, by="Site")
All <- merge(All, Sum_CBiomLength, by="Site")
All <- merge(All, Sum_CRich, by="Site")
All <- merge(All, Sum_StDensArea, by="Site")
All <- merge(All, Sum_StDensLength, by="Site")
All <- merge(All, Sum_StBiomArea, by="Site")
All <- merge(All, Sum_StBiomLength, by="Site")
All <- merge(All, Sum_AllSpFPDensArea, by="Site")
All <- merge(All, Sum_AllSpFPDensLength, by="Site")



#pulls out the CVs for each variable into one dataframe, its annoying that it keeps the dataframe name in variable name
CVshort <- data.frame(All$Site, All$CDensAreaCV, All$CDensLengthCV, All$CBiomAreaCV, All$CBiomLengthCV, 
                        All$CRichCV, All$StDensAreaCV, All$StDensLengthCV, All$StBiomAreaCV, 
                          All$StBiomLengthCV, All$AllSpFPDensAreaCV, All$AllSpFPDensLengthCV)


#restructure data to long form for graphing (put all data in one column)
library(reshape)
CVs <- melt(CVshort, id=c("All.Site"))


#Boxplot showing CVs by metric across sites
par(mar=c(12,5,1,1))
boxplot(value ~ variable,
        data = CVs,
        ylab     = "Coefficient of Variance Across 13 sites", las=2)


#produces summary statistics for the CVs for each metric across sites
Sum_CVs <- summarySE(CVs, measurevar="value", groupvars=c("variable"))

#Plot of mean CV (+/- 1SE) for each metric across sites
library(ggplot2)
ggplot(Sum_CVs, aes(x=variable, y=value)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.15, size=0.1) +
  geom_point(size=2) +
  ylab("Average Coefficient of Variance from the 13 sites (mean +/- SE") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #remove gridlines
                     axis.text.x = element_text(angle = 30, hjust = 1)) #rotate text angle


####################################################################################################################
#################################################POWER ANALYSIS#####################################################
####################################################################################################################

library(simr)
library(lme4)

model1 <- lmer(CDensArea ~ year + (1|Site), data=TempVar)
summary(model1)

fixef(model1) ["year"]
fixef(model1) ["year"] <- 85 #Challenging to determine what fixed effect to enter. I used 10% of the mean of all 52 data points

powerSim(model1)




model2 <- lmer(CDensLength ~ year + (1|Site), data=TempVar)
summary(model2)

fixef(model2) ["year"]
fixef(model2) ["year"] <- 38 #Challenging to determine what fixed effect to enter. I used 10% of the mean of all 52 data points

powerSim(model2)
