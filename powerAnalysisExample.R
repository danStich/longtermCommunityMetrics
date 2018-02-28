# T-test power analysis -----

# Specify sampling means
# This means that we want to detect a 20% difference
  mu.1 = 20
  mu.2 = 0.80*mu.1

# Now, we need to know something about the variance
# So, we make it up- for now, assume they are equal
# Start with a CV if you want to use that metric
# Here, we'll assume they're the same for convenience
  cv = 2
  sigma.1 = (mu.1*cv)/100
  sigma.2 = 5

# Number of simulations to run. We'll do 10k for now
  nruns = 10000  

# Containers to hold simulation results
  n = vector(mode='numeric', length=nruns)
  p = vector(mode='numeric', length=nruns)    

# Set up a progress bar
  pb <- txtProgressBar(min = 0, max = nruns, style=3, char="><> ")

# Run the simulation
  for(i in 1:nruns){  
  # Let's sample each group
  # We will take "n" samples
    # First, we'll tell R what n is. Here, we consider sample sizes from
    # 3 to 30.
      n[i] = sample(seq(from=3, to=50, by=1), 1)
    # Now, we can randomly sample each of our groups
      one = rnorm(n=n[i], mean=mu.1, sd=sigma.2)
      two = rnorm(n=n[i], mean=mu.2, sd=sigma.2)
  # Oops...let's make our respons and our explanatory variable
    # Response variable
      y = c(one, two)
    # Explanatory variable
      x = c( rep(x = "group1", times=n[i]),
             rep(x = "group2", times=n[i]) )
  # Now, we will do a null hypothesis significance test using
  # a t-test. Here our null hypothesis is that there is no
  # difference in the response between groups. This is a two-tail
  # test.
    mod = t.test(y~x, var.equal=TRUE)
    p[i] = mod$p.value
    
  # System time out for progress bar update
    Sys.sleep(0.0001)
    setTxtProgressBar(pb, i)
    
  }    
  
# Close the progress meter
  close(pb)

# Put the simulation results into a dataframe containing sample sizes and 
# p-values. If p < 0.05, then we can say that we successfully detected a
# difference of 20% between groups
  results = data.frame(n, p)
  head(results, 10)

# Plot the results as boxplots so we can roughly gauge how many samples (x)
# we need to achieve p < 0.05 (red line)
  boxplot(p~n, col='gray87', ylab='P-value', xlab='Sample size', outline=FALSE)  
# Add a line for p = 0.05  
  abline(h=0.05, lwd=2, lty=2, col='red')  
  