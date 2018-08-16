# libs
library(imager)
library(data.table)
library(keras)

# Load Data -----------------------------------------------------

# images
imgs <- grep(".tiff", list.files("03_trainings_frageboegen/train_1/"), value = T)
N <- 1000
MAT <- list()
for(i in 1:N){
  print(i)
  MAT[[i]] <- as.matrix(load.image(paste0("03_trainings_frageboegen/train_1/sample", i,".tiff")))
}

# combine to array 
library(abind)
ARR <- do.call(abind,c(MAT,list(along=0)))
rm(MAT)

# load lables 
y <- fread("03_trainings_frageboegen/train_1/labels.csv")
y <- y$fr_y
y <- y[1:N]

# Data Preparation -----------------------------------------------------

batch_size <- 64
num_classes <- 11
epochs <- 20

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
  
  layer_flatten() %>% 
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = num_classes, activation = 'softmax')

# Compile model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adadelta(),
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
  xte, yte, verbose = 0
)

# Output metrics
cat('Test loss:', scores[[1]], '\n')
cat('Test accuracy:', scores[[2]], '\n')


# SAVE
save_model_hdf5(model, '04_models/02_model.h5')
model <- load_model_hdf5('04_models/02_model.h5')


