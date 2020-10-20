#######################################
#           Neural Network            #
#######################################
library(tensorflow)
library(keras)
# Download and prepare the CIFAR10 dataset
# The CIFAR10 dataset contains 60,000 color images in 10 classes, with 6,000 images in each class. 
# The dataset is divided into 50,000 training images and 10,000 testing images. 
# The classes are mutually exclusive and there is no overlap between them.
cifar <- dataset_cifar10()

# Verify the data
# To verify that the dataset looks correct, let's plot the first 25 images from the training set 
# and display the class name below each image.
class_names <- c('airplane', 'automobile', 'bird', 'cat', 'deer',
                 'dog', 'frog', 'horse', 'ship', 'truck')

index <- 1:30

par(mfcol = c(5,6), mar = rep(1, 4), oma = rep(0.2, 4))
cifar$train$x[index,,,] %>% 
  purrr::array_tree(1) %>%
  purrr::set_names(class_names[cifar$train$y[index] + 1]) %>% 
  purrr::map(as.raster, max = 255) %>%
  purrr::iwalk(~{plot(.x); title(.y)})

# Create the convolutional base
# The 6 lines of code below define the convolutional base using a common pattern: a stack of Conv2D and MaxPooling2D layers.
# As input, a CNN takes tensors of shape (image_height, image_width, color_channels), ignoring the batch size. 
# If you are new to these dimensions, color_channels refers to (R,G,B). 
# In this example, you will configure our CNN to process inputs of shape (32, 32, 3), which is the format of CIFAR images. 
# You can do this by passing the argument input_shape to our first layer.
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(32,32,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")

summary(model)


# Add Dense layers on top
# To complete our model, you will feed the last output tensor from the convolutional base (of shape (3, 3, 64)) into one or more Dense layers to perform classification. 
# Dense layers take vectors as input (which are 1D), while the current output is a 3D tensor. 
# First, you will flatten (or unroll) the 3D output to 1D, then add one or more Dense layers on top. 
# CIFAR has 10 output classes, so you use a final Dense layer with 10 outputs and a softmax activation.

model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")

summary(model)




# Compile and train the model
model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

history <- model %>% 
  fit(
    x = cifar$train$x, y = cifar$train$y,
    epochs = 10,
    validation_data = unname(cifar$test),
    verbose = 2
  )

plot(history)

evaluate(model, cifar$test$x, cifar$test$y, verbose = 0)