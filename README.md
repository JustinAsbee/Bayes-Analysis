# Bayes-Analysis

Project was created using R version 4.1.0 (2021-05-18)
Code was last updated 5/2/22
Please use the citation file to cite this code

Within the file you may use find and replace for the variables you will be using in your analysis
  Replace outcome with your outcome or dependent variable
  Replace Predictor1, Predictor2, and Predictor3 with your predictors.
    For example if you were trying to use age and gender to predict shoe size
      Code before replacement
        summary(test1<-lm(Outcome ~ Predictor1 + Predictor2 + Predictor3, data))
      Code after replacement
        summary(test1<-lm(ShoeSize ~ Age + Gender, data))
      Code containing an interaction between age and gender
         summary(test1<-lm(ShoeSize ~ Age + Gender + Age*Gender, data))
  
This repository uses the MIT License (please see the LICENSE file for more details)
