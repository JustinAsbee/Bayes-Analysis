#install packages if you do not already have them
install.packages("Bolstad")
install.packages("rstanarm")
install.packages("apaTables")
install.packages("bayestestR")
install.packages("ggplot2")
install.packages("insight")
install.packages("interactions")
install.packages("LaplacesDemon")
install.packages("sjPlot")

#load packages for use
library(Bolstad)
library(rstanarm)
library(apaTables)
library(bayestestR)
library(ggplot2)
library(insight)
library(interactions)
library(LaplacesDemon)
library(sjPlot)

######################################################
#loading dataset and setting seed
######################################################

#set random number gen to get same results each time
set.seed(8675309)

#load data
data <- read.csv("C://Users//Documents//data.csv")

######################################################
#Correlations
######################################################

#does a correlation analysis
corr <- data[,c("Outcome", "Predictor1", "Predictor2", "Predictor3")]

#Creates a correlation word doc, remove filename = "C://Users//Documents//correlation_table.doc" to just print to console
apa.cor.table(corr, filename = "C://Users//Documents//correlation_table.doc")

######################################################
#Main effects - Frequentist Approach
######################################################

#Standard Regression
summary(test1<-lm(Outcome ~ Predictor1 + Predictor2 + Predictor3, data))

#creates a table of the results
tab_model(test1, file = "C://Users//Documents//Regression_output.doc")

#can be used to create figures for interactions
plot_model(test1, type = "pred", terms = c("Predictor1", "Predictor2"))
#another method for creating figures
fit1 <- lm(Outcome ~ Predictor1*Predictor2, data=data)
interact_plot(fit1, pred = Predictor1, modx = Predictor2, interval = TRUE)

######################################################
#Main effects - Bayesian Approach
######################################################

#https://easystats.github.io/bayestestR/articles/example1.html
#default of weakly informative priors
test_b1<-stan_glm(Outcome ~ Predictor1 + Predictor2 + Predictor3, data = data)

#information on priors used
prior_summary(test_b1)

#information for the posterior (aka output)
#estimates are based on median values for the posteriors
describe_posterior(test_b1)

#additional information included in the output
describe_posterior(test_b1, test = c("p_direction","rope","bayesfactor"))


#can show different credible interval using tab_model(test_b1, show.ci89 = TRUE)
tab_model(test_b1, file = "C://Users//Documents//bayes model 1.doc")

#longer chain
test_b1_long<-stan_glm(Outcome ~ Predictor1 + Predictor2 + Predictor3, data = data, iter = 100000)


##################
### Traceplots ###
##################
#from https://rdrr.io/cran/rstanarm/src/R/plots.R
# NOTE: rstanarm doesn't store the warmup draws (to save space because they
# are not so essential for diagnosing the particular models implemented in
# rstanarm) so the iterations in the traceplot are post-warmup iterations


plot(test_b1, "trace")

#pars argumenet used for getting specific traceplots
#trace <- plot(test_b1, "trace", pars = "(Intercept)")

#shows the trace plot for Predictor1
plot(test_b1, "trace", pars = "Predictor1")


#samples the posterior for the creation of the posterior distribution figure
posteriors <- insight::get_parameters(test_b1)
head(posteriors)  # Show the first 6 rows

ggplot(posteriors, aes(x = Predictor1)) +
  geom_density(fill = "orange")+
  # The mean in blue
  geom_vline(xintercept=mean(posteriors$Predictor1), color="blue", size=1) +
  # The median in red
  geom_vline(xintercept=median(posteriors$Predictor1), color="red", size=1) +
  # The MAP in purple
  geom_vline(xintercept=map_estimate(posteriors$Predictor1), color="purple", size=1)


#z scores should be within 1.96 if not try doubling the number of iterations for the mcmc 
Geweke.Diagnostic(test_b1)

####################################################
### Intervals and point estimates for posteriors ###
####################################################

#for all predictors
p1 <- plot(test_b1, prob = 0.5, prob_outer = 0.9)
p1 + ggplot2::ggtitle("Posterior medians \n with 50% and 90% intervals")

#for specific predictors
p <- plot(test_b1, prob = 0.5, prob_outer = 0.9, regex_pars = "Predictor1")
p + ggplot2::ggtitle("Posterior medians \n with 50% and 90% intervals")

# can plot density curves for predictors posterior distribution
#Shaded areas under densities
plot(test_b1, "areas", regex_pars = "Predictor1", prob = 0.5, prob_outer = 0.9)

##################################
### Histograms & density plots ###
##################################

#Plots should not have gaps, may be affected by bin width can change (see below)
plot_title <- ggplot2::ggtitle("Posterior Distributions")
plot(test_b1, "hist") + plot_title

#MCMC chain info
plot(test_b1, "dens_overlay", pars = "(Intercept)") + plot_title

#changing number of bins (may not be needed)
plot(test_b1, "hist", binwidth = 0.1) + plot_title

#######################
### autocorrelation ###
#######################
#see https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html

# autocorrelation by chain
#high autocorrelation can be a sign that there was a problem with the functioning of the MCMC sampling algorithm or in the initial setup of the model
#Positive autocorrelation is bad (it means the chain tends to stay in the same area between iterations) and you want it to drop quickly to zero with increasing lag. Negative autocorrelation is possible and it is useful as it indicates fast convergence of sample mean towards true mean.
#line chart
plot(test_b1, "acf")


#should give convergence info
#the mean_PPD provided should be a plausible value for the mean of the DV, if not could be (severe model misspecification, problems with the data, computational issues, etc.)
summary(test_b1)


#creates regression line
ggplot(data, aes(x=AGE, y=tst)) +
  geom_point() +  # This adds the points
  geom_smooth(method="lm") # This adds a regression line

#changed ci to 95% may want to include 89% as well
hdi(posteriors$AGE, ci=0.95)
mean(posteriors$AGE)
median(posteriors$AGE)
map_estimate(posteriors$AGE)


#################################
##informed priors
#################################
#https://easystats.github.io/bayestestR/articles/bayes_factors.html
#http://mc-stan.org/rstanarm/articles/priors.html

#info on currently used priors
prior_summary(test_b1)
sd_y1 = sd(data$Outcome)

#example has 2 variables Predictor1 has mean -10 and SD of 5, Predictor2 has M 0 and SD 1
#my_prior <- normal(location = c(-10, 0), scale = c(5, 2))
#stan_glm(Outcome ~ Predictor1 + Predictor2, data = data, prior = my_prior)

#create priors (default priors in rstanarm are mean (i.e. location) 0 and sd (i.e. scale) 2.5)
#autoscale allows rstanarm to make adjustments and transformations to the priors (if autoscale set to true it will adjust scale to use unstandardized beta weight errors need to use standard deviations for the beta weights)
#autoscale will not convert the effect size itself (aka location) to unstandardized units need to convert by taking standardized beta and multiplying by (SDy/SDx) can be obtained from your data

my_prior_b1 <- normal(location = c(1, 2, 3), scale = c(1, 2, 3), autoscale = TRUE)
test_infob1<-stan_glm(Outcome ~ Predictor1 + Predictor2 + Predictor3, data = data, prior = my_prior_b1)

prior_summary(test_b1)
prior_summary(test_infob1)

#if your analysis has missing data you may want to run the following lines it will include only the values used in the analysis
#getting the priors to calculate the SD for the variables
og_SD_infob1 <- test_infob1[["prior.info"]][["prior"]][["scale"]]
adj_SD_infob1 <- test_infob1[["prior.info"]][["prior"]][["adjusted_scale"]]
og_mean_infob1 <- test_infob1[["prior.info"]][["prior"]][["location"]]
adj_valueb1 <- (og_SD_infob1/adj_SD_infob1)
locationb1 <- (og_mean_infob1/adj_valueb1)


describe_posterior(test_infob1, test = c("p_direction","rope","bayesfactor"))

prior_summary(test_b1)

test_b1_prior<-stan_glm(Outcome ~ Predictor1 + Predictor2 + Predictor3, data = data, prior_PD = TRUE)
plot(test_b1_prior, "hist")

posterior_vs_prior(test_b1)
posterior_vs_prior(test_b1,pars="alpha")
posterior_vs_prior(test_b1_prior,pars="beta")

#when betas are standardized
#peterson & Brown 2005 on the use of beta coefficients in meta analysis
#r = 0.98*beta + 0.5*Z when beta is positive Z = 1 when beta is negative Z = 0
#convert d to r (http://www.psychometrica.de/effect_size.html)
#standardized beta -0.1206 from g = -0.238
#Beta(standardized) = B(unstandard) *(SD(X)/SD(Y))

#http://mc-stan.org/rstanarm/articles/priors.html
#link for information on default priors
#intercept is centered
#default priors used for beta weight are prior = normal(0, 2.5, autoscale = TRUE)

#creating new priors
p2b1 <- normal(location = locationb1, scale = c(1, 2, 3), autoscale = TRUE)

#use new priors
np_b1<-stan_glm(Outcome ~ Predictor1 + Predictor2 + Predictor3, data = data, prior = p2b1)

#show results
describe_posterior(np_b1, test = c("p_direction","rope","bayesfactor"))

#create an output table
tab_model(np_b1, file = "C://Users//Documents//bayes model informed priors.doc")