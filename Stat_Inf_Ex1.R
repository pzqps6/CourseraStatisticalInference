# Statistical Inference - Excercise 1 - Part 1
# Theoretical vs Actual Simulation

rm ( list = ls( )) # remove all env variables

set.seed(4)                 # added so that random functions called will provide reproducable results
sample_size  <-  40         # provided in the question
lambda <- 0.2               # provided in the question
num_sample_sets <- 1000     # provided in the question

# generate a set of exp randrom vars of sample size 40 and calc its exp-mean, store array of 1000 means
exp_rand_variables  <- rexp( num_sample_sets*sample_size, lambda )       # gen 1000 * 40 randorm numbers at rate = lambda
sample_matrix       <- matrix( exp_rand_variables, num_sample_sets, sample_size )   # form a matrix with 1 sample set per row
mean_sample_set     <- rowMeans ( sample_matrix )                        # get average of each row

# GRAPH PLOT

# plot the mean sample set generated
# BLUE line = actual
# RED  line = theoretical 
hist( mean_sample_set, breaks = 100, prob=TRUE,
     main="Theoretical vs Actual Simulation of Means of Exponentialial Data Sets of Size 40 Each",
     xlab="Mean of Exponential Data Sets", col=351, border = 2)
# plot the line of mean for the given range
lines(density(mean_sample_set), col="blue") 
abline(v = mean(mean_sample_set), col="blue")

# mean exp density plot theoretical
abline(v = 1/lambda, col="red")    # therotetical plot of mean density
# theoretical density of the averages of samples
x_vals <- seq(min(mean_sample_set), max(mean_sample_set), length=100)
y_vals <- dnorm(x_vals, mean=1/lambda, sd=(1/lambda/sqrt(sample_size)))
lines(x_vals, y_vals, col="red")

# below command may be used to plot the variance
qqnorm( mean_sample_set, col = "cyan"); 
qqline( mean_sample_set)

# below command may be used to plot the variance vs sd of the 40 samples
sd_vals <- apply( sample_matrix,1, sd)
plot ( mean_sample_set, sd_vals , col="green")