library(keras)
library(EBImage)
library(abind)

read.imags.CNN <- function(path = "./") {
  fns <- list.files(path)
  res<-array(dim=c(length(fns),32,32,3))
  k=0
  for(i in fns) {
    k=k+1
    fn <- paste(path, i, sep = "")
    im <- readImage(fn)
    im <- resize(im,32,32)
    res[k,,,] <- im
  }
  return(res)
}

car.images <- read.imags.CNN("./car/")
cat.images <- read.imags.CNN("./cat/")
flower.images <- read.imags.CNN("./flower/")

nombre.car<-dim(car.images)[1]
nombre.cat<-dim(cat.images)[1]
nombre.flower<-dim(flower.images)[1]
nombre<-nombre.car+nombre.cat+nombre.flower

(sam.count <- ceiling(min(nombre.car * 0.8, nombre.cat * 0.8, nombre.flower* 0.8)))
train.car <- sample(1:nombre.car,sam.count,replace = F)
train.cat <- sample(1:nombre.cat,sam.count,replace = F)
train.flower <- sample(1:nombre.flower,sam.count,replace = F)

x_train<-array(dim=c((3*sam.count),32,32,3))
x_train[1:sam.count,,,] <- car.images[train.car,,,]
x_train[(sam.count+1):(2*sam.count),,,]<-cat.images[train.cat,,,]
x_train[(2*sam.count+1):(3*sam.count),,,]<-flower.images[train.flower,,,]
y_train <- c(rep(0, sam.count), rep(1,sam.count),rep(2,sam.count))
y_train<-to_categorical(y_train,num_classes = 3)

a<-nombre.car-sam.count
b<-nombre.cat-sam.count
c<-nombre.flower-sam.count
x_test<-array(dim=c(a+b+c,32,32,3))
x_test[1:a,,,] <- car.images[-train.car,,,]
x_test[(a+1):(a+b),,,] <-cat.images[-train.cat,,,]
x_test[(a++b+1):(a+b+c),,,]<-flower.images[-train.flower,,,]
y_test<- c(rep(0,a), rep(1,b),rep(2,c))
y_test<-to_categorical(y_test,num_classes = 3)

model<-keras_model_sequential()
model %>%
#start with hidden 20 convolutional layer being fed 32*32 pixel image
layer_conv_2d(
  filter = 32, kernel_size=c(3,3),padding="same",
  input_shape=c(32,32,3)
) %>%
  layer_activation("relu") %>%
  
#second hidden layer
layer_conv_2d(filter = 32, kernel_size=c(3,3)) %>%
layer_activation("relu") %>%
  
# Use max pooling
layer_max_pooling_2d(pool_size=c(2,2)) %>%
layer_dropout(0.25) %>%

#2 additionnal hidden 2D convolutional layers
layer_conv_2d(filter = 32, kernel_size=c(3,3),padding="same") %>%
layer_activation("relu") %>%
layer_conv_2d(filter = 32, kernel_size=c(3,3)) %>%
layer_activation("relu") %>%
  

#Use max pooling once more
layer_max_pooling_2d(pool_size=c(2,2)) %>%
layer_dropout(0.25) %>%

#flatten max filtered output into feature vectore and feed into dense layer
layer_flatten() %>%
layer_dense(512) %>%
layer_activation("relu") %>%
layer_dropout(0.5) %>%
  
#output from dense layer are prejected onto 3 unit output layer
layer_dense(3) %>%
layer_activation("softmax")

opt<-optimizer_rmsprop(lr=0.0001,decay=1e-6)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = opt,
  metrics = c('accuracy')
)

model %>% fit(
  x_train, 
  y_train, 
  epochs = 200, 
  batch_size = 32, 
  validation_data=list(x_test,y_test),
  shuffle=TRUE
)

model%>% save_model_hdf5("CNNModel.h5")
new_model<-load_model_hdf5("CNNModel.h5")
model<-load_model_hdf5("CNNModel.h5")
