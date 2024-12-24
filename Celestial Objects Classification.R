# In this project, I used a variety of classification methods to classify celestial objects using their spectral properties. The data set is from the Sloan Digital Sky Survey. A formatted version of the data was used for this project.

library(caret)

data = read.csv("stars_train.csv") # 1000x9


# First, I will train a K-NN classifier with 10-fold CV using caret. The model will be trained to automatically select the number of neighbours k ∈ {1,2,3,...,20}.
set.seed(2002) # reproducibility
formula = class ~ . # output variable is class, all other variables are used as input variables
ctrl = trainControl(method = "cv", number = 10) # 10-fold cross-validation 
tune_knn = data.frame(k = 1:20) #tuning parameters
fit_knn = train(
  formula,
  data = data,
  method = "knn",
  preProc = c("center", "scale"),
  trControl = ctrl,
  tuneGrid = tune_knn
) # training K-NN classifier 
# Now I'll plot the estimated accuracy of the classifier against the number of neighbours k. By visually inspecting this plot, the predictive accuracy is highest for k = 3 so this is the best value of the tuning parameter for this data set. You should choose whichever value of k leads to the highest accuracy.

# For example, if I chose k = 15 the algorithm would underfit the data because for large k k-NN generates more linear classification boundaries. Underfitting leads to high bias and low predictive performance.
ggplot(fit_knn)

# Now I'll train an LDA classifier and a QDA classifier. Before training a model, clearly describing each parameter is a necessary component of analysis. 

# LDA classifier has the following parameters: 
  
  # Prior class probability $\pi_k$ in each class k. This has length K =3 but $\pi_3$ = 1 - $\pi_2$ -              $\pi_1$ therefore there are only two parameters.

# Mean vector of inputs $\mathbf{\mu}_k$ in each class k. Each vector has length 8 thus leading to 24            parameters (3x8 = 24).

# Covariate matrix of inputs $\mathbf{\Sigma}$. This is a square matrix with 8 rows and 8 columns. Covariance     matrices are symmetric thus there are 36 parameters (8x9/2 = 36).

# Therefore there are 62 parameters to estimate.

# QDA classifier has the following parameters:
  
  # Prior class probability $\pi_k$ in each class k. This has length K =3 but $\pi_3$ = 1 - $\pi_2$ -              $\pi_1$ therefore there are only two parameters (same as LDA).

# Mean vector of inputs $\mathbf{\mu}_k$ in each class k. Each vector has length 8 thus leading to 24            parameters (same as LDA).

# Covariate matrix of inputs $\mathbf{\Sigma}$. QDA has one covariance matrix $\mathbf{\Sigma}_k$ for each       class. Thus there are 108 parameters (3x36).

# Therefore there are 134 parameters to estimate. 


fit_lda = train(
  formula,
  data = data,
  method = "lda",
  trControl = ctrl
) # training LDA classifier

fit_qda = train(
  formula,
  data = data,
  method = "qda",
  trControl = ctrl
) # training QDA classifier

# Now I'll calculate the predictive accuracy of the two classifiers.

accuracy_lda = fit_lda$results$Accuracy
cat("The mean predictive accuracy was approximately", round(accuracy_lda*100), "% for LDA.")
accuracy_qda = fit_qda$results$Accuracy
cat("\nThe mean predictive accuracy was approximately", round(accuracy_qda*100), "% for QDA.")

# QDA performs significantly better which suggests that the LDA assumption (the classification boundaries are linear) is not satisfied. 

# Now I will make predictions given the spectral properties of three unidentified celestial bodies. 
# According to LDA, the predicted classes for the three unidentified objects are galaxy (probability = 0.65), galaxy (probability = 0.88), and quasar (probability = 0.998). According to QDA, the predicted classes are star (probability = 0.99), star (probability = 0.97), and quasar (probability = 1).

newdata = read.csv("stars_new.csv") # unidentified celestial bodies

predict(fit_lda, newdata = newdata, type = "prob") # most likely class according to LDA

predict(fit_qda, newdata = newdata, type = "prob") # most likely class according to QDA

# Finally, I'll train a naive Bayes classifier and make predictions on the three unidentified celestial bodies from earlier. When the conditional distributions of input variables are assumed to be normal, the naive Bayes classifier assumes the conditional independence of the input variables while QDA estimates their dependence. The naive Bayes classifier with normal input distributions is a unique case of the QDA classifier where the covariance matrices are assumed to be diagonal.

# The predicted classes are star (probability = 0.998), star (probability = 0.993), and quasar (probability = 0.999).

fit_nb = train(
  formula,
  data = data,
  method = "naive_bayes",
  trControl = ctrl
) # training naive Bayes classifier

predict(fit_nb, newdata = newdata, type = "prob") # most likely class according to naive Bayes