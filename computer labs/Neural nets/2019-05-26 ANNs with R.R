## ---- include=FALSE------------------------------------------------------
# overall knitr options
knitr::opts_chunk$set(cache = FALSE, echo = TRUE, warning = FALSE, message = FALSE)

## ----get_keras_github, eval = F------------------------------------------
## install.packages("devtools")
## library(devtools)
## devtools::install_github("rstudio/keras")

## ----reproducible_keras--------------------------------------------------
library(keras)
use_session_with_seed(42)

## ----load_boston_housing-------------------------------------------------
boston_housing <- dataset_boston_housing()

c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

## ----print_size_data-----------------------------------------------------
paste0("Training entries: ", nrow(train_data), ", labels: ", nrow(train_labels))

## ----inspect_feature_scale-----------------------------------------------
train_data[1, ]
summary(train_data)

## ----inspect_features----------------------------------------------------
str(train_data)
column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')
train_df <- data.frame(train_data)
colnames(train_df) <- column_names
str(train_df)

## ----inspect_targets-----------------------------------------------------
train_labels[1:10]

## ----normalize_features_training_test------------------------------------
# test data is *not* used when calculating the mean and std

# normalize training data
train_data <- scale(train_data) 

# use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

train_data[1, ] # first training sample, normalized

## ----create_base_model---------------------------------------------------
model <- keras_model_sequential()

## ----add_dense_layer-----------------------------------------------------
model %>%
  layer_dense(units = 2, input_shape = c(1)) %>%
  layer_activation(activation = "relu") %>%
  layer_dense(units = 1)

## ----summary_model-------------------------------------------------------
model # or do: summary(model)
model$layers
model$input
model$output

## ----compile_model-------------------------------------------------------
model %>% compile(
  loss = 'mse',
  optimizer = optimizer_rmsprop(),
  metrics = c('mean_absolute_error')
)

## ----fit_model-----------------------------------------------------------
history <- model %>%
  fit(train_data[ , 1], train_labels, epochs = 5,
      validation_split = 0.2) # fraction of training data used as validation set

## ----plot_fit------------------------------------------------------------
plot(history)

## ----build_fit_plot------------------------------------------------------
# Plot the model loss of the validation data
plot(history$metrics$loss, main = "Model Loss", xlab = "epoch", ylab = "loss", col = "blue", type = "l")

# Plot the model loss of the test data
lines(history$metrics$val_loss, col = "green")

# Add legend
legend("topright", c("train", "test"), col = c("blue", "green"), lty = c(1,1))

## ----predict-------------------------------------------------------------
test_data_pred <- model %>% predict(test_data[ , 1])
library(ggplot2)
qplot(test_data[ , 1], test_data_pred)

## ------------------------------------------------------------------------


## ----build_model_function------------------------------------------------
build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, 
                input_shape = dim(train_data)[2]) %>%
    layer_activation(activation = "relu") %>%
    layer_dense(units = 64) %>%
    layer_activation(activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()

## ------------------------------------------------------------------------
epochs <- 500

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2, # fraction of training data used as validation set
  verbose = 0, # choose between 0 = silent, 1 = progress bar and 2 = one line per epoch
  callbacks = list(print_dot_callback)
)

## ------------------------------------------------------------------------
library(ggplot2)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 5))

## ------------------------------------------------------------------------
# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

model <- build_model()
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 5))

## ------------------------------------------------------------------------
c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))

## ------------------------------------------------------------------------
test_predictions <- model %>% predict(test_data)
test_predictions[ , 1]

