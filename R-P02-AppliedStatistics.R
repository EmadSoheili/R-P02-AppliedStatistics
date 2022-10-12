# R-P02-AppliedStatistics

    # Step 1: Preparing Data

setwd("E:/Mine/Maktab Khooneh/Data Analysis/2 - Applied Statistics/Extras/Final Project/Stat4Business_Final_Project")
ak = read.csv("Atlas_Khodro.csv",header = T)
head(ak)
colnames(ak) = c("time","ins","comp","wait","gas","impt")

        # Seperating differenet columns because they are not identical in type and number
        # Delete NAs

aktime = ak$time

akins = ak$ins
akins = akins[-which(is.na(akins))]

akcomp = ak$comp
akcomp = akcomp[-which(is.na(akcomp))]

akwait = ak$wait
akgas = ak$gas
akimpt = ak$impt

    # Step 2:  Qestions

        #Question 1: Time - Distribution type

            # Final answer: Normal Distribution

            # Sample size is large enough to assume the distribution is normal, but I will test for illustration of the testing steps.

hist(aktime,breaks = 20,probability = T)
lines(density(aktime))
            # It seems normal up to an extent. Further testing is needed.

qqnorm(aktime, pch = 20)
qqline(aktime)
            # It seems normal up to an extent. Further testing is needed.
            # Shapiro test is not OK for this case since the sample size is almost large (50).

library("moments")
jarque.test(aktime)
anscombe.test(aktime)
            # Both test show that the distribution is Normal.



        # Question 2: Time - Estimation

            # Final Answer: Point Estimate: 14.7788, Interval Estimate: [14.0202,15.5374]

            # Point estimate:
            # According to the Central Limit Theorem, sample size is larger than 30 (50), so we can assume that the best estimation for the population mean is the sample mean.

mtime = mean(aktime)

            # Interval estimate (Population variance is unknown)
            # Alpha = 5%

ntime = 50
dftime = ntime-1
stime = sd(aktime)
t0.5alpha = abs(qt(.025,dftime))
intmin = mtime - t0.5alpha*stime/sqrt(ntime)
intmax = mtime + t0.5alpha*stime/sqrt(ntime)

            # Estimated interval: [14.0202,15.5374]



        # Question 3: Time > 15min
            # Final Answer: Point estimate: 46%, Interval Estimate: [32.1853%,59.8146%]

timeo15 = sum(aktime > 15)
ntime = length(aktime)
pstime15 = timeo15/ntime

            # P in the sample is 46%. This is an acceptable point estimation for the population, but for further investigation, I will do the interval estimation too.

z0.5 = qnorm(.025)
pstime15 + z0.5 * sqrt((pstime15*(1-pstime15)/ntime))
pstime15 - z0.5 * sqrt((pstime15*(1-pstime15)/ntime))



        # Question 4: Insurance - Percentage
            # Final Answer: P in the sample is 61.12% and that can be known as P in the population.

akinsp = akins/250
pins = mean(akinsp)



        # Question 5: Insurance - P 2 out of 2
            # Final Answer: The answer is 37.3565%

dbinom(2,2,pins)



        # Question 6: Insurance - P min 5 out of 10
            # Final Answer: The Answer is 85.1875%

pbinom(4,10,pins,lower.tail = F)



        # Question 7: Question 5 and 6 distribution type
            # I have used binomial distribution, which is following the rules of Bernoulli experiment.
            # Rules are:
                # 1. There are only two possible outcome, win or lose, accept or decline etc.
                # 2. During the experiment, the probabilities would not change.
                # 3. Experiments are independent.
            # In question 5 and 6 all of these rules are applied.



        # Question 8: Complaint - 4 out of 20
            # Final Answer is: 1.3550%

pcom = mean(akcomp)
        #pcom is Landa

            # The probability of receiving complaints per 20 individuals is 0.96
poisson.test(x = sum(akcomp), T= length(akcomp))
            #The Poisson test interval is [0.6150,1.4284], which contains 0.96 (data mean)

dpois(4,pcom)



        # Question 9: Complaint - Min 5 out of 20
            # Final Answer is: 0.3083%

ppois(4,pcom,lower.tail = F)



        # Question 10: Question 8 and 9 distribution type
            # Since we are looking for the number of complaints per 20 individuals, the Poisson distribution is the answer. The rules are:
                # 1. Outcomes' probabilities are identical.
                # 2. Outcomes are independent. (The number of a day's complaints will not affect other days' complaints)
                # 3. The probability of having complaints are too low. (0.96 per 20 individuals)
                # 4. The Poisson test is OK. (The data mean is in the Poisson test's interval)



        # Question 11: Wait for bus - Point Estimate
            # Final Answer is: 9.2898 minutes

length (akwait)
            # Since the sample size is 50 (>30), we can assume that the best point for population mean estimate is the sample's mean.
waitmean = 10 + mean(akwait)



        # Question 12: Wait for bus - Over 15 minutes
            # Final Answer is: 8.0096%

            # I have used variable transformation. The reason and procedure will be explained in question 13.
            # I will transform x to log(x).
vtakwait = log(akwait+10)
            # It is important to transform the criterion (15 minutes) as well as other data.
vtx = log(15)
pnorm(vtx, mean = mean(vtakwait), sd = sd(vtakwait), lower.tail = F)



        # Question 13: Explanation of question 12 variable transformation
            # Firstly, we have to test the Normality

hist(akwait, probability = T)
lines(density(akwait))
            # Seems like there is skweness towards right. I guess it is not normal.

qqnorm(akwait, pch = 20)
qqline(akwait)
            # There are numerous deviated data especially at the higher quantiles.
            # Shapiro test is not OK for the sample size of 50. I will skip this step.
quant
library("moments")

jarque.test(akwait)
anscombe.test(akwait)

            # Jarque test is showing the distribution is not normal, and the anscombe is illustrating larger p-value than 0.05 but it's marginal.
            # I believe the distribution is not Normal. We need variable transformation.
            # I will transform x to log(x). Then I have to test the normality for new data set.

hist(vtakwait, probability = T)
lines(density(vtakwait))
qqnorm(vtakwait, pch = 20)
qqline(vtakwait)
            # It seems more normal to me.

jarque.test(vtakwait)
anscombe.test(vtakwait)
            # Both show the distribution is normal.
            # The distribution with the chosen variable transformation is normal.



        # Question 14: Gas - Point Estimate
            # Final Answer is: 18.52
                # Since the sample size is 50 (>30), we can assume that the best point for population mean estimate is the sample's mean.

gasmean = mean(akgas)



        # Question 15: Gas - Probability of over 40
            # Final Answer is: 11.5345%

po40 = pexp(40, rate = 1/gasmean, lower.tail = F)



        # Question 16: Gas - Probability of 2 in row over 40
            # Final Answer is: 1.0769%

            # We are seeking 2 over 40 outcome (in row) and 1 under 40 outcome after that.
            # According to Question 15, the probability of over 40 is 11.5345%.
            # We have Geometric distribution
            # P for this question is the probability of under 40 (Win case in 3rd round of experiment). Therefore, the p here is 1-po40.


dgeom(2,.89)



        # Question 17: Explanation of question 15 and 16 distribution
                
            #Firstly we have to make guess based on Histogram chart.        

hist(akgas, probability = T)
lines(density(akgas))
        #It seems like Exponential distribution. I will test it with Shapiro test.

install.packages("exptest")
library("exptest")

shapiro.exp.test(akgas)
        # p-value is 0.267 (>0.05) then the distribution can assumed as Exponential.



        # Question 18: Improved Time - Testing Hypothesis

            # Step 1: H0: mean(Time) <= mean(Improved Time)
            # Step 2: HA: mean(Time) > mean(Improved Time) ==> mean(Time) - mean(Improved Time) > 0
            # Step 3: alpha = 0.05 as usual
            # Step 4: Data gathering. We have aktime and akimpt vectors, containing data.
            # Step 5: Calculate test statistics
                # Firstly, we have to determine what sort of question we are dealing with.
                    # Test Testin Hypothesis for Mean
                    # Two population
                    # Not paired (different persons, not the same individuals)
                    # Sample size = 50 > 30, we assume akimpt is normal (aktime has been checked in question 2)
                    # Two sided
                    # Variance, need to be checked.

var.test(aktime,akimpt)

                    # p-value is 8.656e-05 < 0.05 then their variances are different.
                # Now we now what sort of question we are dealing with.

xbar = mean(aktime)
ybar = mean(akimpt)
                    #Mux = Muy ==> (Mux - Muy) = 0
varx = var(aktime)
vary = var(akimpt)
n = 50
m = 50

t = ((xbar - ybar) - 0)/sqrt((varx/n)+(vary/m))

            # Step 6: The case is two sided and the alpha is 0.05
            # Step 7: Conclusion

t.test(aktime,akimpt, var.equal = F, alternative = "greater")
            # p-value = 8.77e-12 < 0.05
            # So mean(Time) > mean(Improved Time) which means there is a meaningful difference. The improved time is statically and meaningfully better than original time (irrespective of Delta which needs professional's opinion)


        # Question 19: Improved Insurance - Testing Hypothesis

            # Step 1: H0: P(Insurance after) = P(Insurance before)
            # Step 2: HA: P(Insurance after) # P(Insurance before)
            # Step 3: Alpha = 0.05
            # Step 4: Our data is akins and 67% of 250
            # Step 5: we will use R functions here so we do not need to calculate test statistics manually.
            # Step 6: Two sided
            # Steo 7: Conclusion
                #Two Sample
                # First Sample: Number of samples: 30, Sample size: 250
                # Second Sample: Number of samples: 1, Sample size: 250

x1 = sum(akins)
n1 = 250*30
x2 = 0.67*250
n2 = 250

prop.test(x = c(x1,x2), n = c(n1,n2), alternative = "two.sided", conf.level = 0.95)
            # p-value = 0.07001 > 0.05 then H0 is approved.
            # There is no meaningful difference between insurance number before and after launching the incentive package.
            # In other words, there were no improvement because of the incentive package.
