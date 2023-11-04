library(ISLR2)
library(keras)
library(tensorflow)

View(Default)

##split the data into training and validation sets
set.seed(1)
n <- nrow(Default)
ntest <- trunc(n/5)
testid <- sample(1:n, ntest)
y_test <- Default$default[testid] == 'Yes'

##linear regression
lr <- glm(default~student+balance+income,family="binomial",data=Default[-testid,])
lr_pred <- predict(lr, data=Default[testid,], type='response') > 0.5
lr_accuracy = mean(lr_pred == y_test)
lr_accuracy
##accuracy is 0.95225

##neural network
x = model.matrix(default ~. -1, data=Default)

x_train <- x[-testid,]
y_train <- Default$default[-testid]=='Yes'
x_test <- x[testid,]
y_test <- Default$default[testid] == 'Yes'

nn <- keras_model_sequential() %>% 
  layer_dense(units=10, activation='relu', input_shape=ncol(x)) %>%
  layer_dropout(rate=0.4) %>% 
  layer_dense(units = 1, activation='sigmoid')

nn %>% compile(
  optimizer=optimizer_rmsprop(), 
  loss='binary_crossentropy', 
  metrics='accuracy')
  
history <- nn %>% fit(
  x = x_train, 
  y = y_train, 
  epochs=30, 
  batch_size=128)
nn_pred <- predict(nn, x_test) > 0.5
nn_accuracy <- mean(nn_pred == y_test)
nn_accuracy
##accuracy is 0.9647
##neural network performers better than logistic regression