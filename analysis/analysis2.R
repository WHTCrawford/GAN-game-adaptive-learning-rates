# Packages ################################################## 
rm(list = ls(all=T))
suppressMessages(suppressWarnings(library(reshape2)))
library(plyr)
suppressMessages(suppressWarnings(library(ggplot2)))
library(scatterplot3d)
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(plotly)))
library(lattice)
library(FNN)
library(pbapply)
source('/Users/Billy/PycharmProjects/GALR/mulitplot.R')


# Which plots to show ################################################## 
phi_against_gamma = T
loess_plot_KL = F
plot_best_histogram_KL = T
qqplot = T
plot_med_histogram_KL = F
qqplot_median = F

recalc_KL = T

# Sub folder ################################################## 
sub_folder = 'gd'

save_picture_name = function(name){
  dir = paste(c('/Users/Billy/Documents/Uni/cam/GAN/essay tex/',sub_folder,'_',name,'.jpeg'),
              collapse = '')
  return(dir)
}


# Make colours and font size  ################################################## 

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(2)

export_theme = theme(text=element_text(size=20),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                     axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

rstudio_theme = theme(text=element_text(size=8),
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

# Collect data ################################################## 

data_path = paste('/Users/Billy/PycharmProjects/GALR/data3/',sub_folder, sep = '')
setwd(data_path)

collected_data = fread('output.csv', header = F, sep = ',')
collected_data = data.matrix(collected_data)
collected_data = na.omit(collected_data)
collected_data= data.frame(Gamma =collected_data[,1] ,
                           Phi= collected_data[,2] ,collected_data[,3:ncol(collected_data)])


standardized_data = collected_data
standardized_data[,3:ncol(standardized_data)] = standardized_data[,3:ncol(standardized_data)] -6

nrow(collected_data)


gamma_boundaries= c(min(collected_data$Gamma)+
                      (max(collected_data$Gamma)-min(collected_data$Gamma))/3 ,
                    min(collected_data$Gamma)+
                      2*(max(collected_data$Gamma)-min(collected_data$Gamma))/3)

phi_boundaries = c(min(collected_data$Phi),
                   min(collected_data$Phi)+
                     (max(collected_data$Phi)-min(collected_data$Phi))/2)


# Samples plot  ################################################## 

if(phi_against_gamma){
  cols = gg_color_hue(2)
  p1 = ggplot(collected_data,aes(x = Gamma, y = Phi))+geom_point(col=cols[1])+
    geom_hline(yintercept = phi_boundaries[1]+0.001, col=cols[2], lty = 2, lwd = 3) +
    geom_hline(yintercept = phi_boundaries[2], col=cols[2], lty = 2, lwd = 3)+
    geom_vline(xintercept = gamma_boundaries[1], col=cols[2], lty = 2, lwd = 3) +
    geom_vline(xintercept = gamma_boundaries[2], col=cols[2], lty = 2, lwd = 3)+
    xlab(bquote(gamma))+ylab(bquote(phi))+export_theme
  print(p1)
  jpeg(save_picture_name('samples'), units="in", width=16, height=12, res=300)
  print(p1)
  dev.off()
}

# Calc KL Divergence and save to file -------

if(recalc_KL){
  
  real_dist = rnorm(10000 , 6,1)
  
  kl_to_real = function(vec){
    vec_1 = as.numeric(vec)
    return(KL.divergence(real_dist,vec_1,k=10)[10])
  }
  
  kl_from_real = function(vec){
    vec_1 = as.numeric(vec)
    return(KL.divergence(vec_1,real_dist,k=10)[10])
  }
  
  KL_vec = pbapply(collected_data[,3:ncol(collected_data)],MARGIN = 1,kl_to_real)
  
  KL_vec2 = pbapply(collected_data[,3:ncol(collected_data)],MARGIN = 1,kl_from_real)
  
  KL_dataframe <<- data.frame(Gamma = collected_data[,1],
                              Phi = collected_data[,2],
                              KL_real_gen = KL_vec,
                              KL_gen_real = KL_vec2)
  
  write.csv(x = KL_dataframe, 'KL_divergence.csv',row.names = F)
} else{
  KL_dataframe <<- fread('KL_divergence.csv',header = T)
}

# LOESS KL plot ------


theme.novpadding <- list(
  layout.heights = list(
    top.padding = 0,
    main.key.padding = 0,
    key.axis.padding = 0,
    axis.xlab.padding = 0,
    xlab.key.padding = 0,
    key.sub.padding = 0,
    bottom.padding = 0
  ),
  layout.widths = list(
    left.padding = 0,
    key.ylab.padding = 0,
    ylab.axis.padding = 0,
    axis.key.padding = 0,
    right.padding = 0
  ),
  box.3d = list(col=c(1,1,NA,NA,1,NA,1,1,1))
)


if(loess_plot_KL){
  
  size_of_grid = 80
  
  fit_loess = loess(KL~Gamma*Phi,data =KL_dataframe, span = 0.15)
  
  g_p_grid = expand.grid(list(Gamma = seq(min(KL_dataframe$Gamma), max(KL_dataframe$Phi),
                                      length.out = size_of_grid), 
                              Phi = seq(min(KL_dataframe$Gamma),  max(KL_dataframe$Phi), 
                                      length.out = size_of_grid)))
  predicted_shapiro = predict(fit_loess, newdata = g_p_grid)
  
  new_data = melt(predicted_shapiro)
  new_data$Gamma = sapply(new_data$Gamma,function(x) as.numeric(gsub("Gamma=", "", x)))
  new_data$Phi = sapply(new_data$Phi,function(x) as.numeric(gsub("Phi=", "", x)))
  trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
  p1 =wireframe(value ~ Gamma * Phi, data = new_data, xlab = expression(gamma), par.settings = theme.novpadding,
                ylab = expression(phi), zlab = list('Estimated KL Divergence',rot = 90), col = 'black',
                shade = F,pretty=T)
  print(p1)
  jpeg(save_picture_name('loess'), units="in", width=7, height=7, res=300)
  trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
  print(p1)
  dev.off()
}  




# Split dataframe function -------
split_data_by_boundaries = function(df){
  
  panel_1 = df[df$Gamma<= gamma_boundaries[1] & 
                df$Phi == phi_boundaries[1],]
  
  panel_2 = df[gamma_boundaries[1]< df$Gamma &  
                             df$Gamma <= gamma_boundaries[2] &
                             df$Phi == phi_boundaries[1],]
  
  panel_3 = df[gamma_boundaries[2] < df$Gamma & 
                             df$Phi == phi_boundaries[1],]
  
  panel_4 = df[df$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[1] < df$Phi &
                             df$Phi <= phi_boundaries[2],]
  
  panel_5 = df[gamma_boundaries[1]< df$Gamma &  
                             df$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[1] < df$Phi &
                             df$Phi <= phi_boundaries[2],]
  
  panel_6 = df[gamma_boundaries[2] < df$Gamma & 
                             phi_boundaries[1] < df$Phi &
                             df$Phi <= phi_boundaries[2],]
  
  panel_7 = df[df$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[2]< df$Phi,]
  
  panel_8 = df[gamma_boundaries[1]< df$Gamma &  
                             df$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[2]< df$Phi,]
  
  panel_9 = df[gamma_boundaries[2] < df$Gamma &  
                             phi_boundaries[2]< df$Phi,]
  
  return(list(panel_1,panel_2,panel_3,panel_4,panel_5,panel_6,panel_7,panel_8,panel_9))
}

collected_split = split_data_by_boundaries(collected_data)
standardized_split = split_data_by_boundaries(standardized_data)
KL_split = split_data_by_boundaries(KL_dataframe)


# Find best in each panel ----
which_best_kl = function(df){
  return(which.min(as.numeric(df[,3])))
}
  
best_row_indexes = lapply(KL_split,which_best_kl)

# Titles for each panel -------

titles = list(bquote(gamma~ '<' ~.(round(gamma_boundaries[1],5))~', '
                     ~phi~'='~.(round(phi_boundaries[1],5))),
              bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'
                     ~.(round(gamma_boundaries[2],5))~', '
                     ~phi~'='~.(round(phi_boundaries[1],5))),
              bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~phi~'='~
                       .(round(phi_boundaries[1],5))),
              bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '
                     ~.(round(phi_boundaries[1],5))~'<'
                     ~phi~'<'~.(round(phi_boundaries[2],5))),
              bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'
                     ~.(round(gamma_boundaries[2],5))~', '~
                       .(round(phi_boundaries[1],5))~'<'~phi~'<'
                     ~.(round(phi_boundaries[2],5))),
              bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~
                       .(round(phi_boundaries[1],5))~'<'~phi~'<'
                     ~.(round(phi_boundaries[2],5))),
              bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '
                     ~.(round(phi_boundaries[2],5))~'<'~phi),
              bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'
                     ~.(round(gamma_boundaries[2],5))~
                       ', '~.(round(phi_boundaries[2],5))~'<'~phi),
              bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~
                       ', '~.(round(phi_boundaries[2],5))~'<'~phi))


# Best Histogram ----


plot_individual_hist = function(vec, title = '', max_density = 0.5, theme_choice){
  p = ggplot(data.frame(x=vec), aes(x)) + 
    geom_histogram(fill = cols[2],aes(y = ..density..),bins = 40)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ 
    xlab('')+ ylab('')+theme_choice+
    labs(title=title)+
    ylim(0,max_density)+xlim(2,10)
  return(p)
}


if(plot_best_histogram_KL){ 
  
  best_hist_data = list()
  for(i in 1:9){
    best_hist_data[[i]] = as.numeric(collected_split[[i]][best_row_indexes[[i]],3:ncol(collected_split[[i]])])
  }
  
  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,plot_individual_hist(best_hist_data[[i]], titles[[i]],theme_choice = export_theme))
  }
  
  for(i in 1:9){
    name = paste('q',i,sep = '')
    assign(name,plot_individual_hist(best_hist_data[[i]], titles[[i]],theme_choice = rstudio_theme))
  }

  jpeg(save_picture_name('best_histograms_KL'), units="in", width=16, height=12, res=300)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}

# Best QQ plot -------

vec = 1:10
quantile(vec,probs = 1:100/100)


qqplot_individual <- function(vec,title = '',xlab ='Theoretical Quantiles',
                           ylab = 'Actual Quantiles', theme_choice = rstudio_theme){ 
  qq = data.frame(name = replicate(length(vec),paste('row.',i,sep='')),resids = vec)
  line_colour = cols[2]
  ggplot(qq) +
    stat_qq(aes(sample = resids, color = factor(name)))+
    geom_abline(slope = 1, intercept = 0, col = line_colour, lty = 2, lwd = 2)+
    theme(legend.position="none")+labs(title=title)+
    xlab(xlab)+ylab(ylab)+
    ylim(-3,3)+
    xlim(-3,3)+
    theme_choice
}

if(qqplot){ 
  
  best_qq_data = list()
  for(i in 1:9){
    best_qq_data[[i]] = as.numeric(standardized_split[[i]][best_row_indexes[[i]],3:ncol(standardized_split[[i]])])
  }

  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,qqplot_individual(best_qq_data[[i]], titles[[i]],xlab ='',ylab = '',theme_choice = export_theme))
  }
  
  for(i in 1:9){
    name = paste('q',i,sep = '')
    assign(name,qqplot_individual(best_qq_data[[i]], titles[[i]],xlab ='',ylab = '',theme_choice = rstudio_theme))
  }
  
  
  jpeg(save_picture_name('QQplot'), units="in", width=16, height=12, res=300)
  multiplot_QQ(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot_QQ(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}

# Median Histogram ----

which.median = function(x) {
  if (length(x) %% 2 != 0) {
    which(x == median(x))
  } else if (length(x) %% 2 == 0) {
    a = sort(x)[c(length(x)/2, length(x)/2+1)]
    which(x == a[1])
  }
}

median_of_third_col = function(df){
  return(which.median(df[,3]))
}

median_row_indexes = lapply(KL_split,median_of_third_col)

if(plot_med_histogram_KL){ 
  
  med_hist_data = list()
  for(i in 1:9){
    med_hist_data[[i]] = as.numeric(collected_split[[i]][median_row_indexes[[i]],3:ncol(collected_split[[i]])])
  }
  
  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,plot_individual_hist(med_hist_data[[i]], titles[[i]],theme_choice = export_theme))
  }
  
  for(i in 1:9){
    name = paste('q',i,sep = '')
    assign(name,plot_individual_hist(med_hist_data[[i]], titles[[i]],theme_choice = rstudio_theme))
  }
  
  jpeg(save_picture_name('med_histograms_KL'), units="in", width=16, height=12, res=300)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}

# Median QQ plot -------


if(qqplot_median){ 
  
  med_qq_data = list()
  for(i in 1:9){
    med_qq_data[[i]] = as.numeric(standardized_split[[i]][median_row_indexes[[i]],3:ncol(standardized_split[[i]])])
  }
  
  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,qqplot_individual(med_qq_data[[i]], titles[[i]],xlab ='',ylab = '',theme_choice = export_theme))
  }
  
  for(i in 1:9){
    name = paste('q',i,sep = '')
    assign(name,qqplot_individual(med_qq_data[[i]], titles[[i]],xlab ='',ylab = '',theme_choice = rstudio_theme))
  }
  
  
  jpeg(save_picture_name('QQplot_med'), units="in", width=16, height=12, res=300)
  multiplot_QQ(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot_QQ(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}
