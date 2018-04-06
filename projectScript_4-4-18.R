# Required packages -----

# The following packages must be installed before
# this script can be run:
libraries = c(
  'akima',
  'ggplot2',
  'plyr',
  'reshape',
  'nlme',
  'xlsx'
  )

lapply(libraries, require, character.only = TRUE)

# Function declaration -----
# . summarySE -----
# This code was adapted from SG's Honnedaga Inverts graphing script
# Helper function from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# to produce summary stats (including SEs) for the dataset
  summarySE <- function(data=NULL,
                        measurevar,
                        groupvars=NULL,
                        na.rm=FALSE,
                        conf.interval=.95,
                        .drop=TRUE) {
    
    # New version of length which can handle NA's:
    # if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
    }
    
    # This does the summary. For each group's
    # data frame, return a vector with
    # N, mean, and sd
    datac <- plyr::ddply(.data = data,
                         .variables = groupvars,
                         .drop=.drop,
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
    # Calculate standard error of the mean
    datac$se <- datac$sd / sqrt(datac$N)  
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
      ciMult <- qt(conf.interval/2 + .5, datac$N-1)
      datac$ci <- datac$se * ciMult
    
    return(datac)
  }


# . ceiling_dec-----
# Function to round all decimal values up
# to the next whole number. Code ripped from
# answer at: https://stackoverflow.com/questions/35807523/r-decimal-ceiling
  ceiling_dec <- function(x, level=1){
    round(x + 5*10^(-level-1), level)
  }


# Data prep -----

# ** STOP **
# Set a working directory if not in an R project or
# if you haven't set one yet.
  #setwd("C:/NYBackup/Fish/Neversink&ADK_TempVariability_2016")


# . Data manipulation -----
# Read in the data
  streamdata <- read.csv("TempVarDataForR_4-3-18.csv")

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
                'ST.First.Pass.Density.by.Area..No.0.1ha.',
                'ST.First.Pass.Biomass.by.Area..g.0.1ha.',
                'Community.density..No..100.m..by.length',
                'Community.biomass..g.100.m..by.length',
                'brook.trout.Density..No.100m..by.length',
                'brook.trout.Biomass..g.100m..by.length',
                'First.Pass.All.Species.density..No.100m..by.length',
                'First.Pass.All.Species.Biomass..g.100.m..by.length',
                'ST.First.Pass.Density.by.Length..No.100.m.',
                'ST.First.Pass.Biomass.by.Length..g.100.m.',
                'All.fish.density.first.pass.CPUE..No.h.',
                'All.fish.biomass.first.pass.CPUE..g.h.',
                'ST.density.first.pass.CPUE..No.h.',
                'ST.biomass.first.pass.CPUE..g.h.',
                'Community.richness'
                )
  
# Make a list of names to use as newnamess
  newnames <- c(
                  'CDensArea',
                  'CBiomArea',
                  'StDensArea',
                  'StBiomArea',
                  'CFPDensArea',
                  'CFPBiomArea',
                  'StFPDensArea',
                  'StFPBiomArea',
                  'CDensLength',
                  'CBiomLength',
                  'StDensLength',
                  'StBiomLength',
                  'CFPDensLength',
                  'CFPBiomLength',
                  'StFPDensLength',
                  'StFPBiomLength',
                  'CDensCPUE',
                  'CBiomCPUE',
                  'STDensCPUE',
                  'STBiomCPUE',
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
  
  
# . Summary statistics -----
# Use the summarySE function sourced in 'functions.R' to 
# calculate standard deviation, standard error of the mean,
# and a (default 95%) confidence interval

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
### DSS: can't figure out how to do this without
### loading dplyr here.
  library(dplyr)
  comps = out %>% Reduce(
    function(dtf1,dtf2) left_join(dtf1, dtf2,by=c("Site", "N")), .
    )
  detach("package:dplyr", unload=TRUE)

# Extract CVs for each metric with Site
  CVshort = comps[ , c(1, grep('_cv', names(comps)))]
  
# Reorder columns to produce a more interpretable graph
# (and to compare to graph I made with original script to
# see if new script had errors)  
 # Currently (4/3/18) not using since we added the new metrics - not sure we need it
  # CVshort <- CVshort_unorg[c(1, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 14)]
  
# . Data screening -----  
# Restructure data to long form for 
# graphing (put all data in one column)
  CVs <- reshape::melt(CVshort, id=c("Site"))
  
# Dump dataframe into xlsx file to designate metric classes manually.
  #write.xlsx(x = CVs, file = "CVs_4-3-18.xlsx", row.names = FALSE)

# Produces summary statistics for the CVs for each metric 
# across sites
  Sum_CVs <- summarySE(CVs,
                       measurevar="value",
                       groupvars=c("variable"))  
  
# Boxplot showing log-transformed CVs by metric across sites
  par(mar=c(12,5,1,1))
  boxplot(log(value) ~ variable,
          data = CVs,
          ylab = "Coefficient of variation across sites", las=2)

  # Boxplot showing untransformed CVs by metric across sites
    par(mar=c(12,5,1,1))
    boxplot(value ~ variable,
            data = CVs, ylim=c(0,1),
            ylab = "Coefficient of variation (CV) across 13 sites", las=2)  
    #add means from Sum_CVs dataframe
    points(Sum_CVs$value,col="black",pch=17)
    
    
  # Boxplot showing CVs by site across metrics
    boxplot(value ~ Site,
            data = CVs,
            ylab = "Coefficient of variation", las=2)
  

# Plot of mean CV (+/- 1SE) for each metric across sites
  ggplot2::ggplot(
    Sum_CVs,aes(x=variable, y=value)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.15, size=0.1) + 
    geom_point(size=2) + 
    ylab("Average Coefficient of Variance from the 13 sites (mean +/- SE") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), #remove gridlines
          axis.text.x = element_text(angle = 30, hjust = 1)) #rotate text angle


  
# Mixed model to analyze CV -----
# . Data read and screening -----
  # Read in new CSV file with 4 factor columns (plus 'Park') added to
  # the exported file above, note that this file excludes richness
  CVs_Cat <- read.csv("CVs_categories_4-3-18.csv")
  
  #Create a boxplot to show how CV differs between Adirondack and Catskill sites
  boxplot(CV ~ Park, data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")

  #Create series of boxplots looking at CVs across the 4 factors
  par(mfrow = c(2, 2), mar = c(3, 4, 1, 1))
  boxplot(CV ~ DensBiom,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ LengthArea,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ CommunitySt,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ MultipassFirstPass,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  
# . Model -----  
# Mixed model to assess the effect of 4 factors (types of metrics) on CV
  #4-3-18: experimented with adding 'Park' to the model - it did not even approach significance
  M1 <- lme(CV ~ DensBiom + LengthArea + CommunitySt + MultipassFirstPass,
            random =~ 1 | Site, data=CVs_Cat)
  summary(M1)
  
  # Plot residuals versus fitted values to look for heterogeniety (cone shape)
    E1 <- resid(M1, type = "n")
    F1 <- fitted(M1)
    par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
    plot(x = F1, 
         y = E1, 
         xlab = "Fitted values", 
         ylab = "Residuals")
    abline(h = 0, lty = 2, col = 1)
    
  # Histogram of residuals to look for non-normality.
    hist(E1)
    
  # Plot residuals by site, if we had individual boxes way above or below mean 
  # we would have a problem. If all similar then it means the random effect did its job
    par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
    boxplot(E1 ~ Site, data = CVs_Cat, las=2)
    abline(h = 0)
  
  # Plot residuals by ADKs or Catskills.
    par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
    boxplot(E1 ~ Park, data = CVs_Cat, las=2)
    abline(h = 0)
    
    
    
# Power analysis -----
# . Descriptive statistics for observed data -----
    
# Define metric of interest and save to var    
  #var='CBiomLength'
 
# . Simulation ----- 
# DSS: This is really gross, but we are 99% of the way there
# with it at this point. Otherwise, we also could randomly select
# Temp$var and save that info to save a loop...
    
# Vector pre-allocation for results of whole simulation,
# for each of 12 metrics
  outputs = vector(mode='list', length=12) 

# Number of runs for each metric
  nruns = 20000   
  
# Set up a progress bar
  pb <- txtProgressBar(min = 0, max = nruns*12, style=3, char="><> ")
    
# Repeat the simulation for each of the metrics in TempVar        
  for(j in 4:24){

# Create a new column in the TempVar df to hold the metric
# of interest. This "new" column can be used responsively
# in the data summary below
  TempVar$var = TempVar[,j]
    
# Summarize sampling means and sds for each factor level
# of interest on the log scale. We use the log scale here
# to avoid simulating negative values for positive-
# definitive metrics. **NOTE**: Need to see if we can make this one
# responsive, so we just need to specify the variable at the
# start of the script.
  sites = ddply(TempVar,
                'Site',
                summarize,
                mu = mean(log(var)),
                sd = sd(log(var))
                )

# Set aside the site and column of interest in a new
# dataframe that also contains the baseline vs future
# grouping variable.
  obs = data.frame(TempVar[ , c('Site', 'var')],
                   group=0
                   )  
    
# Inner loop settings & inputs
  # Decide which sample sizes to work with for each site during
  # future sampling
    n = seq(1, 61, 3)
    
  # Decide which effect size you are interested in as a percent
  # difference in estimated means between baseline and future sample
    delta = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)
    ### Potential bug with the seq fxn. When I try to define
    ### delta as seq(0, 0.30, 0.05), R cannot see that some
    ### of the elements are equal to 0.15 (all other values 
    ### are totally fine). This behavior carries through for
    ### any object created from the original, as well!
      
# Memory pre-allocation for output from inner loop   
  # Vector to hold sample sizes used for each run
    N = vector(mode='numeric', length=nruns)    
    
  # Vector to effect size
    d = vector(mode='numeric', length=nruns)  
    
  # Vector to hold p-values from lmer
    p = vector(mode='numeric', length=nruns)   
      
# Inner loop   
  # Run the simulation for the selected variable (total of 12)
    for(i in 1:nruns){  
    
    # Choose sample size
      simN = sample(n, 1, replace=TRUE)
      
    # Choose effect size (difference you want to detect) based on
    # values of delta
      eff = sample(delta, 1, replace=TRUE)
      
    # Pre-allocate memory for output
      # List to hold random samples
        samps = vector(mode='list', length=length(unique(sites$Site)))
        names(samps) = unique(sites$Site)
  
    # Randomly sample response based on groups, and add in the 
    # difference between sampling periods. These data will 
    # represent a sample in the future, and will be compared to 
    # the data collected 2012-2017. The direction of the change
    # is irrelevant to the power analysis, but in this case we
    # expect the response to increase due to recovery from 
    # acidification. We sample from a log normal to avoid 
    # drawing negative values from a positive definitive variable.
      for(t in 1:length(samps)){
        samps[[t]] = rlnorm(simN, 
                            mean=(log(exp(sites$mu[t]) + exp(sites$mu[t])*eff)),
                            sd=sites$sd[t])       
        }
    
    # Make a dataframe holding sites and simulated response
      # Compile samples
        prep = do.call(rbind, samps)
        
      # Convert to long form for lmer and add a group var
        molten = data.frame(melt(prep)[ , c(1,3)], group=1)
        names(molten) = names(obs)
        
    # Sanity check to make sure the simulated data (boxes) line
    # up with our observed means. To use this code, you need to
    # change the value of delta to 0 or understand that the
    # blue dots should be delta% different than the median line
    # in the boxplots on average.
      # boxplot(var~Site, data=molten)
      # points(exp(sites$mu), pch=21, col='blue', bg='blue')
    
    # Combine the simulated data with the observed data
      combo = rbind(obs, molten)
        
    # Fit a linear mixed model to test for differences between
    # the baseline data (2012-2017) compared to future data
      test = tryCatch(
        lme(var~group, random=~1|Site, data=combo),
        error = function(x) x=1
      )
    # Store the p-value for the group effect to see if the 
    # sample resulted in rejection of the null hypothesis that
    # there is no difference between sampling periods assuming
    # a Type-I error rate (alpha) of 0.05
      p[i] = summary(test)$tTable["group","p-value"]
  
      
    # Fill in any of the objects that haven't been filled in above
      N[i] = simN
      d[i] = eff
      
    # System time out for progress bar update
      Sys.sleep(0.00001)
      setTxtProgressBar(pb, (i+nruns*(j-3-1)))        
  }    

# Gather results into a single dataframe for this metric
  res = data.frame(N, d, p)
  
# Create an indicator for whether or not null hypothesis
# was rejected. Initialize with a zero, replace successes
# with a 1
  res$ind = 0
  res$ind[res$p<0.05] = 1
  
# Create another column that can be used to count the number
# of reps for each combination of N and d. Initialize to one so 
# we can sum them up.
  res$totes = 1
 
# Calculate the percent of tests that resulted in a successful
# rejection for each combination of N and d
  outs = ddply(res,
               c('N', 'd'),
               summarize,
               success=sum(ind)/sum(totes)
               )
  
# Summarize by each combination of N and d
# Note that the elements of this list need not be the same length
# as the number of simulations (nruns), but they will be if the
# number of runs is sufficiently large.
  outputs[[j-3]] = outs
  
}
# Close the progress meter
  close(pb)
    
# . Results -----   
# Print the output from the simulation
  #outputs   

# Assign names to the outputs list so we know
# which metric they correspond to
  names(outputs) = names(TempVar)[4:24]

# Bind the list into a dataframe and add the metric names
# to a new column called 'metric'
  sim.res = dplyr::bind_rows(outputs, .id = 'metric')  
  
# Rename the first column of the dataframe  
# Save the output data to a compressed r data file  
  save(sim.res, file='sim.res.rda')  

  
# . Plotting code -----  
  
# Load the data file that contains the simulation results 
  load('sim.res.rda')
  
# Set up an image file we can write to. Change to the type, pointsize,
# resolution, etc. that you are happy with and let it rip
  tiff(filename = "sim.results.tif",
    width = 1650, height = 2000, units = "px", pointsize = 9,
    compression = "none",
    res = 400)  
  
# Set graphical parameters
  par(mfrow=c(7,3), oma=c(3,4,0,0), mar=c(1,1,.5,.5))

# For each metric:
# Subset the data, interpolate values of effect size over
# sample sizes(x) and effect sizes (y), and plot the contour
# for a power (z, 1-beta) of 0.80
  
  for(i in 1:length(unique(sim.res$metric))){
    
    # Choose a variable to plot (this will change)
      sims = sim.res[sim.res$metric==unique(sim.res$metric)[i], ]
      
    # Contour plot of power by N and delta
    # Make a new dataframe that will retain three
    # columns from our data
    	persp.test = data.frame(x=sims$N,	y=sims$d,	z=sims$success)
    
    # Order the data frame
    	persp.test = persp.test[with(persp.test, order(x, y)), ]
    
    # Interpolate z across x and y
    	im = with(persp.test,
    	          interp(x, y, z, duplicate='mean', nx=50, ny=10)
    	          )
    
    # Make the contour plot
      # Draw the contour plot
        contour(                            
          x = im$x,                             
          y = im$y,                             
          z = im$z,                             
          levels = seq(0, 1, 0.10),              
          drawlabels = FALSE,               
          col = c(rep(rgb(0, 0, 0, alpha=0), 8),
                  'black',
                  rep(rgb(0, 0, 0, alpha=0), 10)
                  ),
          lwd = c(rep(1, 8), 1, rep(1, 10)),
          lty = c(rep(1, 8), 1, rep(1, 10)),
          axes = FALSE,
          frame.plot = TRUE,
          xlim = c(0, round(max(im$x), -1)),
          ylim = c(0, round(max(im$y), 1))
        )         
        
    # Add the name of the metric to the plots  
      text(x = 0, y = 0.05, labels = unique(sim.res$metric)[i], adj = 0)
      
  	# Add x(side=1) and y (side=2) tick marks to all plots 
      axis(side = 1, labels = FALSE, tick = TRUE)
      axis(side = 2,
           at = seq(0, 1, 0.1),
           labels = FALSE,
           tick = TRUE)
      
    # Add x-axis tick labels only if plot 11 or 12
      if((i==19) || (i==20)|| (i==21)){
        axis(side = 1, at = seq(0, round(max(im$x), -1), 10),
             labels = seq(0, round(max(im$x), -1), 10))  
      }     
      
    # Add y-axis tick labels only if plot number is even  
      if((i %in% c(1,4,7,10,13,16,19))) {
        axis(side = 2, at = seq(0, max(im$y), 0.1), labels = seq(0, max(im$y), 0.1), las=2)  
      }

  }
  
# Add x and y-axis labels to the plot    
  mtext(text="Effect size", side=2, line=2.5, cex=1, adj=.5, outer=TRUE)
  mtext(text="Sample size (N)", side=1, line=1.5, cex=1, adj=.5, outer=TRUE)

dev.off()  

# . Calculate necessary sample size for power by effect size -----
# Extract the x and y coordinates for power of 0.80
# at effect size of interest

# Create a list to hold x and y coordinates for
# the 0.80 power line that results from contour fxn
  nFor2080 = vector(mode='list',
                    length=length(unique(sim.res$metric)))

# Loop through each of the metrics to determine
# extract info from each set of interpolations
# and get x, y coords for z=0.80
  for(i in 1:length(unique(sim.res$metric))){
  
    # Choose a variable to query
      sims = sim.res[sim.res$metric==unique(sim.res$metric)[i], ]
      
    # Order by observations
    	persp.test = data.frame(x=sims$N,	y=sims$d,	z=sims$success)
    
    # Order the data frame
    	persp.test = persp.test[with(persp.test, order(x, y)), ]
    
    # Interpolate z across x and y
    	im = with(persp.test,
    	          interp(x, y, z, duplicate='mean', nx=10, ny=10)
    	          )  
  
    # Save the x,y coords for each metric to the list
    # that we defined above
      nFor2080[[i]] = contourLines(
        im$x,
        im$y,
        im$z,
        nlevels=1,
        levels=0.80
        )[[1]][2:3] # This keeps only the output we want
  }

# Assign names to the outputs list so we know
# which metric they correspond to
  names(nFor2080) = names(TempVar)[4:24]

# Bind the list into a dataframe and add the metric names
# to a new column called 'metric'
  nFor2080_r = dplyr::bind_rows(nFor2080, .id = 'metric')  
  # A 'tibble' that can be converted to a dataframe
  nFor2080_r = data.frame(nFor2080_r)
  
# Select the rows for the data where the effect size is 
# 0.30. Note that we need to set CRich (richness) aside
# because it has high enough power to detect change at 
# the lowest N
  # Set N=1 aside for richness
    rich = nFor2080_r[nrow(nFor2080_r), ]
  # Select rows where effect was detected
    nFor2080_r = nFor2080_r[round(nFor2080_r$y, 2)>=0.3,]
  # Put rich on at the end of nFor2080
    nFor2080_r = rbind(nFor2080_r, rich)
    
# Select the first (minimum) sample size for each metric
# for some reason I had trouble with multiple conditions,
# so doing it separately here. 
  # Select first N for each metric  
    nFor2080_r = nFor2080_r[(!duplicated(nFor2080_r$metric)), ]

# Round sample sizes up because we can't take part
# part of a sample and rounding down could result
# in lower power than specified
  nFor2080_r$x = ceiling_dec(nFor2080_r$x, 0.0001)
  
# Replace values of 61 (or more) with NA because this just means
# we reached max sample size without detecting effect.
  nFor2080_r$x[nFor2080_r$x > 60] <- NA
  
# Rename the first column of the dataframe  
# Save the output data to a csv  
  write.table(nFor2080_r, file='80pctCoords.csv',
              row.names=FALSE, quote=FALSE, sep=',')  

# . Compare CV to statistical power at desired delta -----  
# Do a regression of mean CV on the number of samples
# required to detect 0.20
  means = plyr::ddply(CVs_Cat, 'variable', 
                      summarize, means=mean(CV))
  names(means)[1] ='metric'
  
# Need to add the CV for Richness into this because
# it was not included in the LMM analysis of CV
  richCV = data.frame(metric='CRich', means=Sum_CVs$value[Sum_CVs$variable=='CRich_cv'])
  means = rbind(means, richCV)
  
# Match up the mean CVs wtih the minimum sample size
  tester = merge(means, nFor2080_r)

# Regression to check CV-power relationship
# We log-transform to prevent inclusion of 
# negative values in our prediction interval
  powcv = lm(log(x)~means, data=tester)
  summary(powcv)
  
# Make presentation quality scatterplot w/ ggplot
# with regression line (Figure 4)
  library(ggplot2)
  ggplot(tester, aes(x=means, y=x)) + geom_point() + geom_smooth(method = "lm", se = FALSE, col='black', size=0.75) +
    theme_bw() + ylim(0,15) + xlim(0.25, 0.45) + xlab("Metric Coefficient of Variation") + ylab("N to detect 30% change at POWER=80") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave("Figure4.jpg", dpi = 600, height = 4, width = 4) #to insert in paper
  ggsave("Figure4.eps", dpi = 600, height = 4, width = 4) #for journal submission
  
# Make plot with prediction intervals on log scale
  # Make predictions from linear model
  # and plot them against the simulation results
    # Create new values for CV from range observed
      newCV = data.frame(
        means=seq(min(tester$means), max(tester$means), .001)
        )
      
    # Make predictions from the model
      pred = predict(powcv, newdata = newCV,
                     interval = 'prediction')
      
    # Exponentiate predictions to get them
    # back on the real scale
      pred = apply(pred, 2, exp)
  
  # Plot the predictions  
    # Set up an image file we can write to. Change to the type, pointsize,
    # resolution, etc. that you are happy with and let it rip
      tiff(filename = "Figure4.tif",
        width = 2000, height = 2000, units = "px", pointsize = 6,
        compression = "none",
        res = 600)    
      
    # Set graphical parameters
      par(mar = c(5,5,1,1))
      
    # Make a plot to set up the plotting
    # window. Points are white, so they
    # will not show up yet.
      plot(x=tester$means, y=tester$x,
           ylim=c(0,20), xlim=c(0.1, 0.45),
           yaxt='n', pch=21, bg='white', col='white',
           cex=2, xlab='Metric coefficient of variation',
           ylab=expression(
             paste('N to detect ', delta, ' = 0.30 | power = 0.80',sep = '')
           )
      )
      
    # Add a polygon for the prediction interval
      # Set up vertices for the polygon
        xx = c(newCV$means, rev(newCV$means))
        yy = c(pred[,2], rev(pred[,3]))
      # Draw the polygon
        polygon(xx, yy, col='gray87', border='gray87')
    
    # Add data points to the plot
      points(x=tester$means, y=tester$x, pch=21, lwd=1, bg='black')
      
    # Add lines for the mean and 95% prediction intervals  
      lines(x=newCV$means, y=pred[,1], lty=1, lwd=1, col='black')
      lines(x=newCV$means, y=pred[,2], lty=2, lwd=1, col='black')
      lines(x=newCV$means, y=pred[,3], lty=2, lwd=1, col='black')
    
    # Add y-axis ticks and labels
      axis(side=2, las=2)
      
    # Add a dark box around the outside
      box()
      
    # Close the graphics device
      dev.off()
  
  
  
  
  