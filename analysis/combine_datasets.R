
##################################################
sub_folder = ''
manual_indices = FALSE
indices = c(1,56)

##################################################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(2)

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