rm(list = ls(all=T))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(3)

##########################

pull_shapiro=function(row, data_set){
  return(as.numeric(shapiro.test(as.numeric(data_set[row,]))$statistic))
}

create_shapiro_set = function(sub_folder,manual_indices,indices){
  
  data_path = paste('/Users/Billy/PycharmProjects/GALR/data/',sub_folder, sep = '')
  
  read_indices = c()
  read_indices[1] = 1
  read_indices[2] = as.numeric(suppressWarnings(
    read.table('/Users/Billy/PycharmProjects/GALR/GAN-game-adaptive-learning-rates/recently_completed_trial.txt')))
  
  if(manual_indices){
    read_indices = indices
  }
  
  collected_data = data.frame()
  
  for(trial in read_indices[1]:read_indices[2]){
    results = read.csv(
      paste(paste(data_path,paste('/results',trial,sep=''),'.csv',sep = ''), sep = '')
    )
    
    gamma = read.csv(
      paste(paste(paste(data_path,'/gamma',sep=''),trial,sep=''),'.csv',sep = '')
    )
    
    phi = read.csv(
      paste(paste(paste(data_path,'/phi',sep = ''),trial,sep=''),'.csv',sep = '')
    )
    
    trial_data = cbind(cbind(gamma,phi),results)
    colnames(trial_data) = c('Gamma','Phi',3:1002)
    collected_data = rbind(collected_data,trial_data)
    
  }
  
  collected_data = na.omit(collected_data)
  
  normal_data = collected_data[,3:ncol(collected_data)]
  normal_data = normal_data - 6 # de-mean
  
  shapiro_stats = 1-sapply(1:nrow(normal_data),pull_shapiro,data_set = normal_data)
  
  shapiro_data = data.frame(x = collected_data$Gamma, y = collected_data$Phi,z = shapiro_stats)
  
  return(shapiro_data)
  
}

sgd = create_shapiro_set('scope_1_SGD',TRUE,c(1,56))
momentum = create_shapiro_set('scope_Momentum',TRUE,c(57,126)) 
adam = create_shapiro_set('',FALSE,c(NA,NA)) 





size_of_grid = 100

fit_loess_sgd = loess(z~x*y,data =sgd, span = 0.2)
fit_loess_momentum = loess(z~x*y,data =momentum, span = 0.2)
fit_loess_adam = loess(z~x*y,data =sgd, span = 0.2)

g_p_grid = expand.grid(list(x = seq(0, 0.1, length.out = size_of_grid), 
                            y = seq(0, 0.1, length.out = size_of_grid)))


predicted_sgd = predict(fit_loess_sgd, newdata = g_p_grid)
predicted_momentum = predict(fit_loess_momentum, newdata = g_p_grid)
predicted_adam = predict(fit_loess_adam, newdata = g_p_grid)


par(mfrow = c(2,2))
for(theta_1 in c(45,135,225,315)){
  
  
  phi_1 = 30
  
  persp(seq(0, 0.1, length.out = size_of_grid),
        y = seq(0, 0.1, length.out = size_of_grid),
        predicted_sgd, theta = theta_1,phi = phi_1, # theta = -45,phi = 20,
        xlab = "Gamma", ylab = "Phi",zlab = 'Shapiro-Wilk Statistic', main = "Shapiro-Wilk Statistic Surface",
        col = 'lightblue')
  par(new = TRUE)
  
  persp(seq(0, 0.1, length.out = size_of_grid),
        y = seq(0, 0.1, length.out = size_of_grid),
        predicted_momentum, theta = theta_1,phi = phi_1, # theta = -45,phi = 20,
        xlab = "Gamma", ylab = "Phi",zlab = 'Shapiro-Wilk Statistic', main = "Shapiro-Wilk Statistic Surface",
        col = 'red')
  par(new = TRUE)
  persp(seq(0, 0.1, length.out = size_of_grid),
        y = seq(0, 0.1, length.out = size_of_grid),
        predicted_adam, theta = theta_1,phi = phi_1, # theta = -45,phi = 20,
        xlab = "Gamma", ylab = "Phi",zlab = 'Shapiro-Wilk Statistic', main = "Shapiro-Wilk Statistic Surface")

  
}




library(plotly)

x_1 = seq(0, 0.1, length.out = size_of_grid)
y_1 = seq(0, 0.1, length.out = size_of_grid)

cols = list(c(0,1,2),c("rgb(107,184,214)","rgb(0,90,124)","rgb(30,30,30)"))

plot_ly(showscale = TRUE) %>%
  add_surface(x = x_1, y =y_1 , z = ~predicted_sgd, opacity = 0.98,colorscale =cols ) %>%
  add_surface(x = x_1,y =y_1 ,z = ~predicted_momentum, opacity = 0.97,colorscale =cols) %>%
  add_surface(x = x_1,y =y_1 ,z = ~predicted_adam, opacity = 0.6, colorscale =cols)


 