# Front-end needs -----

# ** STOP **
# Set a working directory if not in an R project

# Source the file that contains function definitions for 
# this script
  source('functions.R')

# Data manipulation -----
# Read in the data
  streamdata <- read.csv("TempVarDataForR_2-22-18.csv")

# Tabulate number of samples from each site
  table(streamdata$Site)

# Create new variables with shorter names
# Make a list of column names to replace
  oldnames <- c(
                'Community.density..No.0.1ha..by.area',
                'Community.density..No..100.m..by.length',
                'Community.biomass..g.0.1ha..by.area',
                'Community.biomass..g.100.m..by.length',
                'Community.richness',
                'ST.Density..No.0.1.ha..by.area',
                'brook.trout.Density..No.100m..by.length',
                'ST.Biomass..g.0.1.ha..by.area',
                'brook.trout.Biomass..g.100m..by.length',
                'First.Pass.All.Species.density..No.0.1ha..by.area',
                'First.Pass.All.Species.density..No.100m..by.length'
                )
  
# Make a list of names to use as newnamess
  newnames <- c(
                  'CDensArea',
                  'CDensLength',
                  'CBiomArea',
                  'CBiomLength',
                  'CRich',
                  'StDensArea',
                  'StDensLength',
                  'StBiomArea',
                  'StBiomLength',
                  'AllSpFPDensArea',
                  'AllSpFPDensLength'
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
  CVshort = comps[ , c(1, grep('_cv', names(comps)))]
  
# Data screening -----  
# Restructure data to long form for 
# graphing (put all data in one column)
  library(reshape)
  CVs <- melt(CVshort, id=c("Site"))

# Boxplot showing CVs by metric across sites
  par(mar=c(12,5,1,1))
  boxplot(log(value) ~ variable,
          data = CVs,
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

# POWER ANALYSIS -----
library(simr)
library(lme4)

# Fit a mixed model with linear effect of year (continuous)
# and random effect of site (group)
  model1 <- lmer(CDensArea ~ year + (1|Site), data=TempVar)
  summary(model1)

### Should probably log transform the response here because
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
### compare the fit to a null model. When used in that way
### it is basically using simulation to do a post-hoc
### power analysis.
  
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
