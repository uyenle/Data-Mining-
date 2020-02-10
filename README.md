# Data-Mining-

MODEL SELECTING 

Shrinkage. We fit a model involving all p predictors, but the estimated coefficients are shrunk towards zero 
relative to the least squares estimates. This shrinkage (also known as regularization) has the effect of 
reducing variance (at the cost of some bias) and can also perform variable selection. 


Dimension Reduction. We project the p predictors into an M-dimensional subspace, where M < p. 
The M projections are used as predictors to fit a linear regression model by least squares. 

**** CROSS-VALIDATION
We randomly divide the available set of samples into two parts: a training set and a validation or hold-out set.
The model is fit on the training set, and the fitted model is used to predict the 
responses for the observations in the validation set.

The resulting validation-set error gives an estimate of test error: MSE/misclassification rate for competing models.
The validation set error may tend to overestimate the test error for the model fit on the entire data set.



BOOTSTRAP 

Rather than repeatedly obtaining independent data sets from the population, we instead obtain distinct data sets 
by repeatedly sampling observations from the original data set with replacement.

Each of these “bootstrap data sets” is created by sampling with replacement, and is the same size as our original dataset. 
As a result some observations may appear more than once in a given bootstrap data set and some not at all.
The point is that we use the empirical distribution of the sample as estimate for the population distribution.
