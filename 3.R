
set.seed(1)

Population = numeric(length = 365)
# out of 365 days of years there are approximatly 261 weekdays and 104 weekend days
# so we will arrange the sequential order 1st 261 days weekdays, and last 104 days as weekends
# so that in case we need to do startified sampling it would be easy

x = 1:261
Population[x] = rpois(261, 75)

x = 262:365
Population[x] = rpois(104, 120)

pop_mean = mean(Population)

#Population mean = 87.99452

SRSWOR = function(n){
  vec = sample(365, n, replace = FALSE)
  samp = numeric(length = n)
  x = 1:n
  samp[x] = Population[vec[x]]
  return(samp)
}

SRSWR = function(n){
  vec = sample(365, n, replace = TRUE)
  samp = numeric(length = n)
  x = 1:n
  samp[x] = Population[vec[x]]
  
  return(samp)
}


# d) Stratified Sampling  n_i ∝ N_i

Straitifed_sample_1 = function(n){
  
  n1 = round(5*n/7)
  n2 = n-n1
  #doing it without Replacement
  vec1 = sample(261, n1, replace = FALSE)
  vec2 = sample(104, n2, replace = FALSE)
  
  samp = numeric(length =n)
  x1 = 1:n1
  x2 = 1:n2
  samp[x1] = Population[vec1[x1]]
  samp[n1 + x2] = Population[261 + vec2[x2]]
  
  return(samp)
  
}


# e) Stratified Sampling  n_i ∝ sigma_i * N_i
Straitifed_sample_2 = function(n){
  
  # using the fact that variance of poisson distribution is lamba_i, so sigma_i is sqrt(lamda_i)
  #required ratio
  
  # r = 5*sqrt(75)/(5*sqrt(75) + 2* sqrt(120))
  
  #using precomputed value to make it more efficeint
  r =  0.664026309641209
  
  
  n1 = round(r*n)
  n2 = n-n1
  #doing it without Replacement
  vec1 = sample(261, n1, replace = FALSE)
  vec2 = sample(104, n2, replace = FALSE)
  
  samp = numeric(length =n)
  x1 = 1:n1
  x2 = 1:n2
  samp[x1] = Population[vec1[x1]]
  samp[n1 + x2] = Population[261 + vec2[x2]]
  
  return(samp)
  
}

#A function to modularise the repeating process
my_repeat = function(n, foo, samp_size){
  vec = numeric(length=n)
  for(i in 1:n){
    vec[i] = mean(foo(samp_size))
  }
  
  return(c(mean(vec), var(vec)))
}




srswr_25 = my_repeat(1000, SRSWR, 25)  #88, 20

srswr_50 = my_repeat(1000, SRSWR, 50)  #88, 10.9

srswr_100 = my_repeat(1000, SRSWR, 100) #88, 4.8



srswor_25 = my_repeat(1000, SRSWOR, 25) #87.9, 19.1

srswor_50 = my_repeat(1000, SRSWOR, 50) #87.75, 9.17

srswor_100 = my_repeat(1000, SRSWOR, 100) #88.13, 3.56




strat1_25 = my_repeat(1000, Straitifed_sample_1, 25) #87.86, 3.23

strat1_50 = my_repeat(1000, Straitifed_sample_1, 50) #87.77, 1.58

strat1_100 = my_repeat(1000, Straitifed_sample_1, 100) #88.25 0.71



strat2_25 = my_repeat(1000, Straitifed_sample_2, 25) #89.68, 3.62

strat2_50 = my_repeat(1000, Straitifed_sample_2, 50) #90.5, 1.56

strat2_100 = my_repeat(1000, Straitifed_sample_2, 100) #90.477, 0.623


# we can observe that mean, of srswr, srsrwor, stratified sampling 1, are very close 
# to the population mean, but for startified sampling 2, mean is slightly deviated from
# the poulation mean value

#For all the schemese we can observe that variance of means reduces as we increase the sample size


# we can also see that variance for among all schemes folllows this trend
# startified sampling 2 < stratified sampling 1 < SRSWOR < SRWR

# The results points that stratified sampling is very much better than simple random sampling
# since its variance is very less compared to SRS

# Stratified sampling 2 has less variance compared to Stratified Sampling 1
# But Stratifed Sampling 2 is deviated from actual population mean 
# but whereas Stratfied Sampling 1 is very close to sample mean

#In Comparing in SRSWOR and SRSWR, SRWOR is obviously better beacuse same expected value of mean
# but less variance



