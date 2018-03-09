# Required packages -----

# The following packages must be installed before
# this script can be run:
libraries = c(
  'akima',
  'ggplot2',
  'lme4',
  'lmerTest',
  'plyr',
  'reshape',
  'nlme'
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


# Data prep -----

# ** STOP **
# Set a working directory if not in an R project or
# if you haven't set one yet.
  #setwd("C:/NYBackup/Fish/Neversink&ADK_TempVariability_2016")


# . Data manipulation -----
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
  CVshort_unorg = comps[ , c(1, grep('_cv', names(comps)))]
  
# Reorder columns to produce a more interpretable graph (and to compare to graph I made with original script to see if new script had errors)  
  CVshort <- CVshort_unorg[c(1, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 14)]
  
# . Data screening -----  
# Restructure data to long form for 
# graphing (put all data in one column)
  CVs <- reshape::melt(CVshort, id=c("Site"))

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
  
# Produces summary statistics for the CVs for each metric 
# across sites
  Sum_CVs <- summarySE(CVs,
                       measurevar="value",
                       groupvars=c("variable"))

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
  # Read in new CSV file with 4 factor columns added to
  # the exported file above, note that this file excludes richness
  CVs_Cat <- read.csv("CVs_categories_3-2-18.csv")

  #Create series of boxplots looking at CVs across the 4 factors
  par(mfrow = c(2, 2), mar = c(3, 4, 1, 1))
  boxplot(CV ~ DensBiom,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ LengthArea,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ CommunitySt,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  boxplot(CV ~ MultipassFirstPass,data = CVs_Cat, ylim=c(0,1), ylab = "Coefficient of variation (CV)")
  
  #Histogram of 
  hist(CVs_Cat$CV)
  
  
# . Model -----  
# Mixed model to assess the effect of 4 factors (types of metrics) on CV
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
  
  # Plot residuals by site, if we had individual boxes way above or below mean 
  # we would have a problem. If all similar then it means the random effect did its job
    par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
    boxplot(E1 ~ Site, data = CVs_Cat, las=2)
    abline(h = 0)
  
  
# Power analysis -----

    
# . Descriptive statistics for observed data -----
  var='CBiomLength'

# Summarize sampling means and sds for each factor level
# of interest on the log scale. We use the log scale here
# to avoid simulating negative values for positive-
# definitive metrics. **NOTE**: Need to see if we can make this one
# responsive, so we just need to specify the variable at the
# start of the script.
  sites = ddply(TempVar,
                'Site',
                summarize,
                mu = mean(log(CBiomLength)),
                sd = sd(log(CBiomLength))
                )

# Set aside the site and column of interest in a new
# dataframe that also contains the baseline vs future
# grouping variable.
  obs = data.frame(TempVar[ , c('Site', 'CBiomLength')],
                   group=0
                   )  
  
# Calculate grand mean and sd in case it is needed later on
  grand = c(mean(log(TempVar$CBiomLength)), sd(log(TempVar$CBiomLength)))
  
  
# . Simulation settings & inputs -----
# Decide which sample sizes to work with for each site during
# future sampling
  n = seq(1, 100, 5)
  
# Decide which effect size you are interested in as a percent
# difference in estimated means between baseline and future sample
  delta = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
### Potential bug with the seq fxn. When I try to define
### delta as seq(0, 0.30, 0.05), R cannot see that some
### of the elements are equal to 0.15 (all other values 
### are totally fine). This behavior carries through for
### any object created from the original, as well!
  
# Number of runs for the simulation
  nruns = 10000
    
  
# . Memory pre-allocation for simulation output -----   
# Vector to hold sample sizes used for each run
  N = vector(mode='numeric', length=nruns)    
  
# Vector to effect size
  d = vector(mode='numeric', length=nruns)  
  
# Vector to hold p-values from lmer
  p = vector(mode='numeric', length=nruns)   
    
    
# . Simulation ----- 
# Set up a progress bar
  pb <- txtProgressBar(min = 0, max = nruns, style=3, char="><> ")

# Run the simulation
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
      samps[[t]] = rlnorm(simN, mean=(sites$mu[t]), sd=sites$sd[t]) + eff*(exp(sites$mu[t])) 
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
    # boxplot(CBiomLength~Site, data=molten)
    # points(exp(sites$mu), pch=21, col='blue', bg='blue')
  
  # Combine the simulated data with the observed data
    combo = rbind(obs, molten)
      
  # Fit a linear mixed model to test for differences between
  # the baseline data (2012-2017) compared to future data
    test = lmer(CBiomLength~group+(1|Site), data=combo)
    
  # Store the p-value for the group effect to see if the 
  # sample resulted in rejection of the null hypothesis that
  # there is no difference between sampling periods assuming
  # a Type-I error rate (alpha) of 0.05
    p[i] = c(summary(test)$coefficients["group", "Pr(>|t|)"])
    
  # Fill in any of the objects that haven't been filled in above
    N[i] = simN
    d[i] = eff
    
  # System time out for progress bar update
    Sys.sleep(0.0001)
    setTxtProgressBar(pb, i)
    
  }    
  
# Close the progress meter
  close(pb)


# . Results -----   
# Gather results into a single dataframe
  res = data.frame(N, d, p)
  
# Create an indicator for whether or not null hypothesis
# was rejected. Initialize with a zero, replace successes
# with a 1
  res$ind = 0
  res$ind[res$p<0.05] = 1
  
# Create another column that can be used to count the number
# of reps for each combination of N and d
  res$totes = 1
 
# Calculate the percent of tests that resulted in a successful
# rejection for each combination of N and d
  outs = ddply(res, c('N', 'd'), summarize, success=sum(ind)/sum(totes))
  
# Contour plot of p-value by N and delta
  # Now, we will make a new dataframe that will retain three
  # columns from our data.
  	persp.test = data.frame(x=res$N,	y=res$d,	z=res$p)

  # Order the data frame
  	persp.test = persp.test[with(persp.test, order(x, y)), ]

  # Interpolate z across x and y
  	im = with(persp.test, interp(x, y, z,
  	                             duplicate='mean', nx=50, ny=50))

  # Make the contour plot
      par(mar=c(5, 5.2, 1, 10))
    # filled.contour is the function that actually makes the contour plot
  	  filled.contour(
  	    im$x,                                 # The variable to be displayed on the x-axis
  	    im$y,                                 # The variable to be displayed on the y-axis
  	    im$z,                                 # The response variable you wish to plot
  	    levels=seq(0,.5,.05),
  	    col=rev(gray.colors(11)),             # Could also choose 'grey.colors' or 'topo.colors'. If you want the ramp to go the other way, just delete the 'rev'. Note that you will need to change the 20 in parentheses to match the number of levels that you actually have or want to display.
  	    main = '',                            # I don't like in-figure titles. You can add one, though. You will, however, need to change the 'mar' argument in the call to par above.
  	    ylim=c(min(im$y), max(im$y)), # Set max and min of y-axis to your data range
  	    xlim=c(min(im$x), max(im$x)),         # Set max and min of x-axis to your data range
  	    xlab="Number of samples",              # Change the words in the quotes to change the x-axis label
  	    cex.lab=1.5,                          # This makes the labels 1.5x larger than default
  	    plot.axes = {                         # This argument tells R to print the axes, but increas the size
  	      contour(                            # This is the line that adds the contour lines
  	        im$x,                             # The variable to be displayed on the x-axis
  	        im$y,                             # The variable to be displayed on the y-axis
  	        im$z,                             # The response variable you wish to plot
  	        levels=seq(0,.5,.05),                   # This number needs to match the one in 'col' on line 102
  	        drawlabels = FALSE,               # The labels are realy ugly
  	        col = c(rep(rgb(0,0,0, alpha=0.05), 1),'black', rep(rgb(0,0,0, alpha=0), 10)),
  	        lwd = c(1,2,rep(1, 10)),
  	        lty = c(1,2,rep(1, 10)),
  	        add = TRUE                        # Add the lines to the current plot
  	      );                                  # Close the call to the contour line function
  	      axis(1, cex.axis=1.25);             # X axis tick marks & tick labels
  	      axis(2, cex.axis=1.25)              # Y axis tick marks & tick labels
  	    }                                     # Close the argument plot.axes
  	  )                                       # Close the call to filled.contour
  	# Finally, add a label for the y-axis
      mtext(side = 2, "Effect size", line=4, cex.lab=1.5, cex=1.5)
      mtext(side=4, "P-value",
            line=7.5, cex=1.25)  
      
# Contour plot of power by N and delta
  # Now, we will make a new dataframe that will retain three
  # columns from our data.
  	persp.test = data.frame(x=outs$N,	y=outs$d,	z=outs$success)

  # Order the data frame
  	persp.test = persp.test[with(persp.test, order(x, y)), ]

  # Interpolate z across x and y
  	im = with(persp.test, interp(x, y, z,
  	                             duplicate='mean', nx=10, ny=10))

  # Make the contour plot
      par(mar=c(5, 5.2, 1, 10))
    # filled.contour is the function that actually makes the contour plot
  	  filled.contour(
  	    im$x,                                 # The variable to be displayed on the x-axis
  	    im$y,                                 # The variable to be displayed on the y-axis
  	    im$z,                                 # The response variable you wish to plot
  	    levels=seq(0,1,.05),
  	    col=rev(gray.colors(21)),             # Could also choose 'grey.colors' or 'topo.colors'. If you want the ramp to go the other way, just delete the 'rev'. Note that you will need to change the 20 in parentheses to match the number of levels that you actually have or want to display.
  	    main = '',                            # I don't like in-figure titles. You can add one, though. You will, however, need to change the 'mar' argument in the call to par above.
  	    ylim=c(min(im$y), max(im$y)),         # Set max and min of y-axis to your data range
  	    xlim=c(min(im$x), max(im$x)),         # Set max and min of x-axis to your data range
  	    xlab="Number of samples",             # Change the words in the quotes to change the x-axis label
  	    cex.lab=1.5,                          # This makes the labels 1.5x larger than default
  	    plot.axes = {                         # This argument tells R to print the axes, but increas the size
  	      contour(                            # This is the line that adds the contour lines
  	        im$x,                             # The variable to be displayed on the x-axis
  	        im$y,                             # The variable to be displayed on the y-axis
  	        im$z,                             # The response variable you wish to plot
  	        levels=seq(0,1,.05),              # This number needs to match the one in 'col' on line 102
  	        drawlabels = FALSE,               # The labels are realy ugly
  	        col = c(rep(rgb(0,0,0, alpha=0.05), 16),'black', rep(rgb(0,0,0, alpha=0), 10)),
  	        lwd = c(rep(1, 16),2,rep(1, 10)),
  	        lty = c(rep(1, 16),2,rep(1, 10)),
  	        add = TRUE                        # Add the lines to the current plot
  	      );                                  # Close the call to the contour line function
  	      axis(1, cex.axis=1.25);             # X axis tick marks & tick labels
  	      axis(2, cex.axis=1.25)              # Y axis tick marks & tick labels
  	    }                                     # Close the argument plot.axes
  	  )                                       # Close the call to filled.contour
  	# Finally, add a label for the y-axis
      mtext(side = 2, "Effect size", line=4, cex.lab=1.5, cex=1.5)
      mtext(side=4, expression(paste("Power (1 - ", beta, ")")),
            line=8, cex=1.25)        
      
