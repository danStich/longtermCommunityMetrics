# Front-end needs -----

# ** STOP **
# Set a working directory if not in an R project
setwd("C:/NYBackup/Fish/Neversink&ADK_TempVariability_2016")

# Source the file that contains function definitions for 
# this script
  source('functions.R')

# Data manipulation -----
# Read in the data
  streamdata <- read.csv("TempVarDataForR_3-1-18.csv")

# Tabulate number of samples from each site
  table(streamdata$Site)

# Create new variables with shorter names
# Make a list of column names to replace
  oldnames <- c(
                'Community.density..No.0.1ha..by.area',
                'Community.biomass..g.0.1ha..by.area',
                'ST.Density..No.0.1.ha..by.area',
                'ST.Biomass..g.0.1.ha..by.area',
                'First.Pass.All.Species.density..No.0.1ha..by.area',
                'First.Pass.All.Species.Biomass..g.0.1ha..by.area',
                'Community.density..No..100.m..by.length',
                'Community.biomass..g.100.m..by.length',
                'brook.trout.Density..No.100m..by.length',
                'brook.trout.Biomass..g.100m..by.length',
                'First.Pass.All.Species.density..No.100m..by.length',
                'First.Pass.All.Species.Biomass..g.100.m..by.length',
                'Community.richness'
                )
  
# Make a list of names to use as newnamess
  newnames <- c(
                  'CDensArea',
                  'CBiomArea',
                  'StDensArea',
                  'StBiomArea',
                  'AllSpFPDensArea',
                  'AllSpFPBiomArea',
                  'CDensLength',
                  'CBiomLength',
                  'StDensLength',
                  'StBiomLength',
                  'AllSpFPDensLength',
                  'AllSpFPBiomLength',
                  'CRich'
                  )
  
  
# Replace the long column names with the shorter version
  names(streamdata)[names(streamdata) %in% oldnames] <- newnames

# Make a new dataframe with just the first few columns and the 
# columns that got new names
  TempVar <- 
    data.frame(
      streamdata[ , c(1:3,
                      which(names(streamdata) %in% newnames)
                      )])
  
# Summary statistics -----
# Use the summarySE function sourced in 'functions.R' to 
# calculate standard deviation, standard error of the mean,
# and a (default 95%) confidence interval

### DSS: would be nice to have the ability to wrap the 
### summarySE function in the apply family. Can loop
### through it to make the code shorter for now

# Pre-allocate memory
  out = vector(mode='list', length = ncol(TempVar)-3)
  
# Loop through the desired columns and put results in list
  for(i in 4:ncol(TempVar)){
    
    # Set up a temporary dataframe to pass to summarySE
      var <- TempVar[ , c(2:3, i)]
    
    # Calculate summary stats for the selected variable
      mids <-  summarySE(
               var, 
               assign('measurevar', names(var)[3]),
               groupvars = c('Site') 
               )

    # Assign name to list element based on the variable of interest
      names(out)[i-3] <- paste('Sum_', names(TempVar)[i])
    
    # Save the dataframe to the corresponding list element
      out[[i-3]] <- mids
      
    # Give the dataframe some names
      names(out[[i-3]]) <- c('Site', 'N', names(mids)[3],
                             paste0(
                               names(var)[3], 
                               '_',
                               names(mids)[4:ncol(mids)]
                             ))
  } # i

# Calculate the coefficient of variation (cv) for each dataframe in the list
# and add it as a column. Probably a better way to do this with
# mapply or lapply functions
  for(i in 1:length(out)){
    out[[i]]$cv = out[[i]][,4]/out[[i]][,3]
    names(out[[i]])[names(out[[i]])=='cv'] <- paste0(
      names(out[[i]])[3],
      '_cv'
    )
  }

# Merge all dataframes in list through a
# series of left joins on 'Site' and 'N' 
  library(dplyr)
  comps = out %>% Reduce(
    function(dtf1,dtf2) left_join(dtf1, dtf2,by=c("Site", "N")), .
    )
  detach("package:dplyr", unload=TRUE)

# Extract CVs for each metric with Site
  CVshort_unorg = comps[ , c(1, grep('_cv', names(comps)))]
  
# Reorder columns to produce a more interpretable graph (and to compare to graph I made with original script to see if new script had errors)  
  CVshort <- CVshort_unorg[c(1, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 14)]
  
  
  
# Data screening -----  
# Restructure data to long form for 
# graphing (put all data in one column)
  library(reshape)
  CVs <- melt(CVshort, id=c("Site"))

  
# Boxplot showing log-transformed CVs by metric across sites
  par(mar=c(12,5,1,1))
  boxplot(log(value) ~ variable,
          data = CVs,
          ylab = "Coefficient of variation across sites", las=2)

  
  # Boxplot showing untransformed CVs by metric across sites
  par(mar=c(12,5,1,1))
  boxplot(value ~ variable,
          data = CVs, ylim=c(0,1),
          ylab = "Coefficient of variation across sites", las=2)  
  
  
  # Boxplot showing CVs by site across metrics
  boxplot(value ~ Site,
          data = CVs,
          ylab = "Coefficient of variation", las=2)
  

# produces summary statistics for the CVs for each metric across sites
  Sum_CVs <- summarySE(CVs,
                       measurevar="value",
                       groupvars=c("variable"))

  
# Plot of mean CV (+/- 1SE) for each metric across sites
  library(ggplot2)
  ggplot(
    Sum_CVs,aes(x=variable, y=value)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.15, size=0.1) + 
    geom_point(size=2) + 
    ylab("Average Coefficient of Variance from the 13 sites (mean +/- SE") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), #remove gridlines
          axis.text.x = element_text(angle = 30, hjust = 1)) #rotate text angle

  
  #################################################################################################
  ##################################Mixed Model to Analyze CV######################################
  #################################################################################################
  
  #Read in new CSV file with 4 factor columns added to the exported file above, note that this file excludes richness
  CVs_Cat <- read.csv("C:/NYBackup/Fish/Neversink&ADK_TempVariability_2016/CVs_categories_3-2-18.csv")
  
  
  #Create series of boxplots looking at CVs across the 4 factors
  par(mfrow = c(2, 2), mar = c(3, 4, 1, 1))
  boxplot(CV ~ DensBiom,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ LengthArea,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ CommunitySt,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ MultipassFirstPass,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  
  
  hist(CVs_Cat$CV)
  
  
  #Mixed model to assess the effect of 4 factors (types of metrics) on CV
  library(nlme)
  M1 <- lme(CV ~ DensBiom + LengthArea + CommunitySt + MultipassFirstPass, random =~ 1 | Site, data=CVs_Cat)
  summary(M1)
  
  
  #Plot residuals versus fitted values to look for heterogeniety (cone shape)
  E1 <- resid(M1, type = "n")
  F1 <- fitted(M1)
  par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
  plot(x = F1, 
       y = E1,
       xlab = "Fitted values",
       ylab = "Residuals")
  abline(h = 0, lty = 2, col = 1)
  

  #Plot residuals by site, if we had individual boxes way above or below mean 
  #we would have a problem. If all similar then it means the random effect did its job
  par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
  boxplot(E1 ~ Site, data = CVs_Cat, las=2)
  abline(h = 0)
  
  
  
  
  
  
  
#########################################################################################  
# POWER ANALYSIS -----
#########################################################################################
  
library(simr)
library(lme4)

# Fit a mixed model with linear effect of year (continuous)
# and random effect of site (group)
  model1 <- lmer(CDensArea ~ year + (1|Site), data=TempVar)
  summary(model1)

### DSS: Should probably log transform the response here because
### it has mass near zero, but is positive definitive. This
### could result in prediction intervals that include negative
### numbers.
  
  fixef(model1) ["year"]
  
### DSS: Left off here. Tough to make out what is going on
### with some of this. After running and looking through
### documentation, it looks like we are fitting a lmm to 
### test linear time effects and random effects of site.
### Looks like the default for the powerSim() fxn is to
### simulate parameters from the fitted model objects and 
### the data, generate a dataset, and then run an lmm to 
### compare the fit to a null model with a likelihood
### ratio test. When used in that way it is basically using
### simulation to do a post-hoc power analysis.
  
  # fixef(model1) ["year"] <- 85 #Challenging to determine what fixed effect to enter. I used 10% of the mean of all 52 data points

  # powerSim(model1)
  # 
  # model2 <- lmer(CDensLength ~ year + (1|Site), data=TempVar)
  # summary(model2)
  # 
  # fixef(model2) ["year"]
  # fixef(model2) ["year"] <- 38 #Challenging to determine what fixed effect to enter. I used 10% of the mean of all 52 data points
  # 
  # powerSim(model2)
