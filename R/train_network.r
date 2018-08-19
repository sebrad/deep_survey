# libs
library(keras)

# Load Data -----------------------------------------------------

# load simulation array
ARR <- readRDS("data/03_simulation_data/simulation1_image_array.rds")/255

# load lables 
y_data <- readRDS("data/03_simulation_data/simulation1_label_data_frame.rds")
y_data$class_int <- as.numeric(y_data$label)-1
y <- y_data$class_int

# Data Preparation -----------------------------------------------------

batch_size <- 64
num_classes <- 40
epochs <- 50

# Input image dimensions
img_rows <- dim(ARR)[2]
img_cols <- dim(ARR)[3]

# Train and Testset
trainFrac <- 0.85

set.seed(42)
tr <- sample(1:dim(ARR)[1], floor(trainFrac * dim(ARR)[1]), replace = F)

xtr <- ARR[tr,,]
xte <- ARR[-tr,,]
ytr <- y[tr]
yte <- y[-tr]
rm(ARR)

# Redefine  dimension of train/test inputs
xtr <- array_reshape(xtr, c(nrow(xtr), img_rows, img_cols, 1))
xte <- array_reshape(xte, c(nrow(xte), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

cat('x_train_shape:', dim(xtr), '\n')
cat(nrow(xtr), 'train samples\n')
cat(nrow(xte), 'test samples\n')

# Convert class vectors to binary class matrices
ytr <- to_categorical(ytr, num_classes)
yte <- to_categorical(yte, num_classes)

# Define Model -----------------------------------------------------------

gc()

# Define model
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',
                input_shape = input_shape) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  
  layer_conv_2d(filters = 256, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>% 
  layer_dense(units = 512, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_dense(units = num_classes, activation = 'softmax')

# Compile model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adagrad(),
  metrics = c('accuracy')
)

# Train model
model %>% fit(
  xtr, ytr,
  batch_size = batch_size,
  epochs = epochs,
  validation_split = 0.2
)


scores <- model %>% evaluate(
  xte, yte, verbose = 1
)

# Output metrics
cat('Test loss:', scores[[1]], '\n')
cat('Test accuracy:', scores[[2]], '\n')


# ------------------------------
# Fehlerhafte Prognosen plotten
# ------------------------------

y_data_te <- y_data[-tr,]
y_data_te$preds <- predict_classes(model, xte, verbose = T)

bool_fail <- y_data_te$class_int != y_data_te$preds
y_fail <- y_data_te[bool_fail,]
x_fail <- xte[bool_fail,,,,drop = F]
class_info <- unique(y_data)
par(mfrow = c(1, 5))
for(i in 1:dim(x_fail)[1]){
  fail_pred <- class_info[class_info$class_int == y_fail[i,]$preds & class_info$frage == y_fail[i,]$frage,]$label
  image(t(x_fail[i,,,]), main = paste0("Pred: ", fail_pred, "\nObs: ", y_fail[i,]$label), col = grey(0:255/255))
}
par(mfrow = c(1, 1))



# ------------------------------
# Fehlerhafte Prognosen plotten
# ------------------------------
save_model_hdf5(model, '04_models/02_model.h5')
model <- load_model_hdf5('04_models/02_model.h5')


