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
source('/Users/Billy/PycharmProjects/GALR/GAN-game-adaptive-learning-rates/analysis/gg_QQ_plot.R')

# Which plots to show ################################################## 
phi_against_gamma = F
plot_histograms = F
plot_QQs = F
plot_best_histogram = F
random_histogram = F
interactive_plot = F
scatter_3D = F
loess_plot = F
plot_mean_sd = F
phi_as_factor = F
gamma_as_factor = F
compare_means = F
plot_best_histogram_KL = T
plot_best_QQ_KL = T
loess_plot_KL = T
phi_as_factor_KL = T

# Sub folder ################################################## 
sub_folder = 'gd'

# Make colours and font size  ################################################## 

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(2)

theme_jose=theme(text=element_text(size=40),
                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


theme_jose_qq=theme(text=element_text(size=20),
                 axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                 axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

# Collect data ################################################## 

data_path = paste('/Users/Billy/PycharmProjects/GALR/data2/',sub_folder, sep = '')
setwd(data_path)

collected_data = fread('output.csv', header = F, sep = ',')
collected_data = data.matrix(collected_data)
collected_data = na.omit(collected_data)
collected_data= data.frame(Gamma =collected_data[,1] ,
                           Phi= collected_data[,2] ,collected_data[,3:ncol(collected_data)])


nrow(collected_data)
mean(collected_data$Phi == min(collected_data$Phi)) # proportion phi zeroes have we collected


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
        xlab(bquote(gamma))+ylab(bquote(phi))+theme_jose
  print(p1)
  jpeg(paste('/Users/Billy/Documents/Uni/cam/GAN/essay tex/',paste(sub_folder,'samples.jpeg',sep = ''),sep = '')
       , units="in", width=16, height=12, res=300)
  print(p1)
  dev.off()
}

# filter out far means and sd--------

old_data = collected_data

rmeans = rowMeans(collected_data[,3:ncol(collected_data)])
accuracy_required = 0.5
collected_data = collected_data[6-accuracy_required<=rmeans & rmeans <=6+accuracy_required,]



accuracy_required = 0.2
sds = apply(collected_data[,3:ncol(collected_data)], 1, sd)
collected_data = collected_data[(1-accuracy_required<=sds & sds <=1+accuracy_required),]

nrow(collected_data)
nrow(old_data)


(sum(collected_data$Phi == phi_boundaries[1]) - sum(old_data$Phi == phi_boundaries[1]))/sum(old_data$Phi == phi_boundaries[1])
(sum(phi_boundaries[1] < collected_data$Phi &
       collected_data$Phi <= phi_boundaries[2]) - sum(phi_boundaries[1] < old_data$Phi &
                                                        old_data$Phi <= phi_boundaries[2]))/sum(phi_boundaries[1] < old_data$Phi &
        old_data$Phi <= phi_boundaries[2])
(sum(phi_boundaries[2] < collected_data$Phi) - sum( old_data$Phi > phi_boundaries[2]))/sum( old_data$Phi > phi_boundaries[2])


# Plot histograms --------
if(plot_histograms){
  
      bin_number = 40
      
      entries_per_group = 1
      
      panel_1 = collected_data[collected_data$Gamma<= gamma_boundaries[1] & 
                                 collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
      
      panel_2 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                                 collected_data$Gamma <= gamma_boundaries[2] &
                                 collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
      
      panel_3 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                                 collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
      
      panel_4 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                                  phi_boundaries[1] < collected_data$Phi &
                                 collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
      
      panel_5 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                                 collected_data$Gamma <= gamma_boundaries[2] &
                                 phi_boundaries[1] < collected_data$Phi &
                                 collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
      
      panel_6 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                                 phi_boundaries[1] < collected_data$Phi &
                                 collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
      
      panel_7 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                                  phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
      
      panel_8 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                                 collected_data$Gamma <= gamma_boundaries[2] &
                                 phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
      
      panel_9 = collected_data[gamma_boundaries[2] < collected_data$Gamma &  
                                 phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
      
      # take a sample of the rows
      min_rows = min(c(nrow(panel_1),nrow(panel_2),nrow(panel_3),nrow(panel_4),nrow(panel_5),
                       nrow(panel_6),nrow(panel_7),nrow(panel_8),nrow(panel_9)))
    
      sample_size = min(entries_per_group,min_rows)
      
      panel_1 = data.frame(x = melt(panel_1[sample(1:nrow(panel_1),size = sample_size,replace = F),])[,2])
      panel_2 = data.frame(x = melt(panel_2[sample(1:nrow(panel_2),size = sample_size,replace = F),])[,2])
      panel_3 = data.frame(x = melt(panel_3[sample(1:nrow(panel_3),size = sample_size,replace = F),])[,2])
      panel_4 = data.frame(x = melt(panel_4[sample(1:nrow(panel_4),size = sample_size,replace = F),])[,2])
      panel_5 = data.frame(x = melt(panel_5[sample(1:nrow(panel_5),size = sample_size,replace = F),])[,2])
      panel_6 = data.frame(x = melt(panel_6[sample(1:nrow(panel_6),size = sample_size,replace = F),])[,2])
      panel_7 = data.frame(x = melt(panel_7[sample(1:nrow(panel_7),size = sample_size,replace = F),])[,2])
      panel_8 = data.frame(x = melt(panel_8[sample(1:nrow(panel_8),size = sample_size,replace = F),])[,2])
      panel_9 = data.frame(x = melt(panel_9[sample(1:nrow(panel_9),size = sample_size,replace = F),])[,2])
      
      
      # adjust so the scales are the same
                  
      pull_max_density =function(panel){
        min_break = round_any(min(panel$x), diff(range(panel$x))/bin_number, floor)
        max_break = round_any(max(panel$x), diff(range(panel$x))/bin_number, ceiling)
        breaks = seq(min_break, max_break, diff(range(panel$x/bin_number)))
        histo = hist(panel$x, breaks=breaks, plot=F)
        return(max(histo$density))
      }
      
      max_density = max(c(pull_max_density(panel_1),pull_max_density(panel_2),pull_max_density(panel_3),
                        pull_max_density(panel_4),pull_max_density(panel_5),pull_max_density(panel_6),
                        pull_max_density(panel_7),pull_max_density(panel_8),pull_max_density(panel_9)))
                         
                      
      
      # chart titles
      
      title1 = bquote(gamma~ '<' ~.(round(gamma_boundaries[1],5))~', '~phi~'='~.(round(phi_boundaries[1],5)))
      
      title2 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~', '
                      ~phi~'='~.(round(phi_boundaries[1],5)))
      
      title3 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~phi~'='~
                        .(round(phi_boundaries[1],5)))
      
      title4 = bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '~.(round(phi_boundaries[1],5))~'<'
                      ~phi~'<'~.(round(phi_boundaries[2],5)))
      
      title5 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~', '~
                        .(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5)))
      
      title6 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~
                        .(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5)))
      
      title7 = bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '~.(round(phi_boundaries[2],5))~'<'~phi)
      
      title8 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~
                        ', '~.(round(phi_boundaries[2],5))~'<'~phi)
      
      title9 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~
                        ', '~.(round(phi_boundaries[2],5))~'<'~phi)
      
      # ggplot 
      x_limits = c(2,10)
      
      p1 = ggplot(data.frame(x=panel_1), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ xlab('')+ ylab('')+
          labs(title=title1) +theme_jose+ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      p2 = ggplot(data.frame(x=panel_2), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ xlab('')+ ylab('')+
        labs(title=title2)+theme_jose+ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      p3 = ggplot(data.frame(x=panel_3), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title3)+theme_jose+
        ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      p4 = ggplot(data.frame(x=panel_4), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title4)+theme_jose+
        ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      p5 = ggplot(data.frame(x=panel_5), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title5)+theme_jose+
        ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      p6 = ggplot(data.frame(x=panel_6), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title6)+theme_jose+
        ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      p7 = ggplot(data.frame(x=panel_7), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title7)+theme_jose+
        ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      p8 = ggplot(data.frame(x=panel_8), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title8)+theme_jose+
        ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      p9 = ggplot(data.frame(x=panel_9), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title9)+theme_jose+
        ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
      
      multiplot(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
      print(paste('Sample size:',sample_size))
}

# Shapiro ################################################## 
collected_data[,3:ncol(collected_data)] = collected_data[,3:ncol(collected_data)] - 6 # de-mean
normal_data = collected_data[,3:ncol(collected_data)]

non_collapsed_indices = ! apply(normal_data,1,var) == 0

phi_gamma_data = collected_data[non_collapsed_indices, 1:2] # remove complete mode collapse
normal_data = normal_data[non_collapsed_indices, ] # remove complete mode collapse
collected_data = collected_data[non_collapsed_indices, ]

shapiro_stats = apply(normal_data,1,shapiro.test)
shapiro_stats = -as.numeric(lapply(shapiro_stats, function(l) l[[1]]))

shapiro_data = data.frame(x = phi_gamma_data$Gamma, y = phi_gamma_data$Phi,z = shapiro_stats)

plot(1:nrow(shapiro_data),shapiro_data$z,xlab = 'Trial number', ylab = '-Shapiro')
abline(lm(shapiro_data$z~c(1:nrow(shapiro_data))), col = 'red', lwd = 6)

# QQ plots ################################################## 
if(plot_QQs){ 
  
  entries_per_group = 1
  
  panel_1 = collected_data[collected_data$Gamma<= gamma_boundaries[1] & 
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_2 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_3 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_4 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_5 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_6 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_7 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  panel_8 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  panel_9 = collected_data[gamma_boundaries[2] < collected_data$Gamma &  
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
 
  
  # take the best shapiros
  shapiro_1 = shapiro_data[collected_data$Gamma<= gamma_boundaries[1] & 
                             collected_data$Phi == phi_boundaries[1],]
  shapiro_2 = shapiro_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             collected_data$Phi == phi_boundaries[1],]
  shapiro_3 = shapiro_data[gamma_boundaries[2] < collected_data$Gamma & 
                             collected_data$Phi == phi_boundaries[1],]
  shapiro_4 = shapiro_data[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],]
  shapiro_5 = shapiro_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],]
  shapiro_6 = shapiro_data[gamma_boundaries[2] < collected_data$Gamma & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],]
  shapiro_7 = shapiro_data[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[2]< collected_data$Phi,]
  shapiro_8 = shapiro_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[2]< collected_data$Phi,]
  shapiro_9 = shapiro_data[gamma_boundaries[2] < collected_data$Gamma &  
                             phi_boundaries[2]< collected_data$Phi,]
  
  
  panel_1 = panel_1[order(as.numeric(shapiro_1$z), decreasing = T)[1:entries_per_group],]
  panel_2 = panel_2[order(as.numeric(shapiro_2$z), decreasing = T)[1:entries_per_group],]
  panel_3 = panel_3[order(as.numeric(shapiro_3$z), decreasing = T)[1:entries_per_group],]
  panel_4 = panel_4[order(as.numeric(shapiro_4$z), decreasing = T)[1:entries_per_group],]
  panel_5 = panel_5[order(as.numeric(shapiro_5$z), decreasing = T)[1:entries_per_group],]
  panel_6 = panel_6[order(as.numeric(shapiro_6$z), decreasing = T)[1:entries_per_group],]
  panel_7 = panel_7[order(as.numeric(shapiro_7$z), decreasing = T)[1:entries_per_group],]
  panel_8 = panel_8[order(as.numeric(shapiro_8$z), decreasing = T)[1:entries_per_group],]
  panel_9 = panel_9[order(as.numeric(shapiro_9$z), decreasing = T)[1:entries_per_group],]
  
  # chart titles
  
  title1 = bquote(gamma~ '<' ~.(round(gamma_boundaries[1],3))~', '~phi~'='~.(round(phi_boundaries[1],3)))
  
  title2 = bquote(.(round(gamma_boundaries[1],3))~'<'~gamma~'<'~.(round(gamma_boundaries[2],3))~', '
                  ~phi~'='~.(round(phi_boundaries[1],3)))
  
  title3 = bquote(.(round(gamma_boundaries[2],3))~'<'~gamma~', '~phi~'='~
                    .(round(phi_boundaries[1],3)))
  
  title4 = bquote(gamma~'<'~.(round(gamma_boundaries[1],3))~', '~.(round(phi_boundaries[1],3))~'<'
                  ~phi~'<'~.(round(phi_boundaries[2],3)))
  
  title5 = bquote(.(round(gamma_boundaries[1],3))~'<'~gamma~'<'~.(round(gamma_boundaries[2],3))~', '~
                    .(round(phi_boundaries[1],3))~'<'~phi~'<'~.(round(phi_boundaries[2],3)))
  
  title6 = bquote(.(round(gamma_boundaries[2],3))~'<'~gamma~', '~
                    .(round(phi_boundaries[1],3))~'<'~phi~'<'~.(round(phi_boundaries[2],3)))
  
  title7 = bquote(gamma~'<'~.(round(gamma_boundaries[1],3))~', '~.(round(phi_boundaries[2],3))~'<'~phi)
  
  title8 = bquote(.(round(gamma_boundaries[1],3))~'<'~gamma~'<'~.(round(gamma_boundaries[2],3))~
                    ', '~.(round(phi_boundaries[2],3))~'<'~phi)
  
  title9 = bquote(.(round(gamma_boundaries[2],3))~'<'~gamma~
                    ', '~.(round(phi_boundaries[2],3))~'<'~phi)
  
  # ggplot 
  
  p1 = qqplot_by_row2(panel_3,title3,xlab = '',ylab = '')
  p2 = qqplot_by_row2(panel_2,title2,xlab = '',ylab = '')
  p3 = qqplot_by_row2(panel_1,title1,xlab = '',ylab = '')
  p4 = qqplot_by_row2(panel_6,title6,xlab = '',ylab = '')
  p5 = qqplot_by_row2(panel_5,title5,xlab = '',ylab = '')
  p6 = qqplot_by_row2(panel_4,title4,xlab = '',ylab = '')
  p7 = qqplot_by_row2(panel_9,title9,xlab = '',ylab = '')
  p8 = qqplot_by_row2(panel_8,title8,xlab = '',ylab = '')
  p9 = qqplot_by_row2(panel_7,title7,xlab = '',ylab = '')
  jpeg(paste('/Users/Billy/Documents/Uni/cam/GAN/essay tex/',paste(sub_folder,'QQ.jpeg',sep = ''),sep = '')
             , units="in", width=16, height=12, res=300)
  multiplot_QQ(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
}


# plot best histograms ----------



if(plot_best_histogram){ 
  bin_number = 50
  
  collected_data[,3:ncol(collected_data)] = collected_data[,3:ncol(collected_data)] + 6
  
  # mean_close
 
  entries_per_group = 1
  
  
  panel_1 = collected_data[collected_data$Gamma<= gamma_boundaries[1] & 
                               collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_2 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                               collected_data$Gamma <= gamma_boundaries[2] &
                               collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_3 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                               collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_4 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                               phi_boundaries[1] < collected_data$Phi &
                               collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_5 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                               collected_data$Gamma <= gamma_boundaries[2] &
                               phi_boundaries[1] < collected_data$Phi &
                               collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_6 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                               phi_boundaries[1] < collected_data$Phi &
                               collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_7 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                               phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  panel_8 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                               collected_data$Gamma <= gamma_boundaries[2] &
                               phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  panel_9 = collected_data[gamma_boundaries[2] < collected_data$Gamma &  
                               phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  
  
  # take the best shapiros
  shapiro_1 = shapiro_data[collected_data$Gamma<= gamma_boundaries[1] & 
                               collected_data$Phi == phi_boundaries[1],]
  shapiro_2 = shapiro_data[gamma_boundaries[1]< collected_data$Gamma &  
                               collected_data$Gamma <= gamma_boundaries[2] &
                               collected_data$Phi == phi_boundaries[1],]
  shapiro_3 = shapiro_data[gamma_boundaries[2] < collected_data$Gamma & 
                               collected_data$Phi == phi_boundaries[1],]
  shapiro_4 = shapiro_data[collected_data$Gamma <= gamma_boundaries[1] & 
                               phi_boundaries[1] < collected_data$Phi &
                               collected_data$Phi <= phi_boundaries[2],]
  shapiro_5 = shapiro_data[gamma_boundaries[1]< collected_data$Gamma &  
                               collected_data$Gamma <= gamma_boundaries[2] &
                               phi_boundaries[1] < collected_data$Phi &
                               collected_data$Phi <= phi_boundaries[2],]
  shapiro_6 = shapiro_data[gamma_boundaries[2] < collected_data$Gamma & 
                               phi_boundaries[1] < collected_data$Phi &
                               collected_data$Phi <= phi_boundaries[2],]
  shapiro_7 = shapiro_data[collected_data$Gamma <= gamma_boundaries[1] & 
                               phi_boundaries[2]< collected_data$Phi,]
  shapiro_8 = shapiro_data[gamma_boundaries[1]< collected_data$Gamma &  
                               collected_data$Gamma <= gamma_boundaries[2] &
                               phi_boundaries[2]< collected_data$Phi,]
  shapiro_9 = shapiro_data[gamma_boundaries[2] < collected_data$Gamma &  
                               phi_boundaries[2]< collected_data$Phi,]
  

  
  panel_1 = data.frame(x = melt(panel_1[order(as.numeric(shapiro_1$z), decreasing = T)[1:entries_per_group],])[,2])
  panel_2 = data.frame(x = melt(panel_2[order(as.numeric(shapiro_2$z), decreasing = T)[1:entries_per_group],])[,2])
  panel_3 = data.frame(x = melt(panel_3[order(as.numeric(shapiro_3$z), decreasing = T)[1:entries_per_group],])[,2])
  panel_4 = data.frame(x = melt(panel_4[order(as.numeric(shapiro_4$z), decreasing = T)[1:entries_per_group],])[,2])
  panel_5 = data.frame(x = melt(panel_5[order(as.numeric(shapiro_5$z), decreasing = T)[1:entries_per_group],])[,2])
  panel_6 = data.frame(x = melt(panel_6[order(as.numeric(shapiro_6$z), decreasing = T)[1:entries_per_group],])[,2])
  panel_7 = data.frame(x = melt(panel_7[order(as.numeric(shapiro_7$z), decreasing = T)[1:entries_per_group],])[,2])
  panel_8 = data.frame(x = melt(panel_8[order(as.numeric(shapiro_8$z), decreasing = T)[1:entries_per_group],])[,2])
  panel_9 = data.frame(x = melt(panel_9[order(as.numeric(shapiro_9$z), decreasing = T)[1:entries_per_group],])[,2])
  
  
  # adjust so the scales are the same
  
  pull_max_density =function(panel){
    min_break = round_any(min(panel$x), diff(range(panel$x))/bin_number, floor)
    max_break = round_any(max(panel$x), diff(range(panel$x))/bin_number, ceiling)
    breaks = seq(min_break, max_break, diff(range(panel$x/bin_number)))
    histo = hist(panel$x, breaks=breaks, plot=F)
    return(max(histo$density))
  }
  
  max_density = max(c(pull_max_density(panel_1),pull_max_density(panel_2),pull_max_density(panel_3),
                      pull_max_density(panel_4),pull_max_density(panel_5),pull_max_density(panel_6),
                      pull_max_density(panel_7),pull_max_density(panel_8),pull_max_density(panel_9)))
  
  
  
  # chart titles
  
  title1 = bquote(gamma~ '<' ~.(round(gamma_boundaries[1],5))~', '~phi~'='~.(round(phi_boundaries[1],5)))
  
  title2 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~', '
                  ~phi~'='~.(round(phi_boundaries[1],5)))
  
  title3 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~phi~'='~
                    .(round(phi_boundaries[1],5)))
  
  title4 = bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '~.(round(phi_boundaries[1],5))~'<'
                  ~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title5 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~', '~
                    .(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title6 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~
                    .(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title7 = bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  title8 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~
                    ', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  title9 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~
                    ', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  # ggplot 
  x_limits = c(2,10)
  
  p1 = ggplot(data.frame(x=panel_1), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ xlab('')+ ylab('')+
    labs(title=title1)+theme_jose_qq+ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p2 = ggplot(data.frame(x=panel_2), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ xlab('')+ ylab('')+
    labs(title=title2)+theme_jose_qq+ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p3 = ggplot(data.frame(x=panel_3), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title3)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p4 = ggplot(data.frame(x=panel_4), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title4)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p5 = ggplot(data.frame(x=panel_5), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title5)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p6 = ggplot(data.frame(x=panel_6), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title6)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p7 = ggplot(data.frame(x=panel_7), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title7)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p8 = ggplot(data.frame(x=panel_8), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title8)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p9 = ggplot(data.frame(x=panel_9), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title9)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  
  
  
  jpeg(paste('/Users/Billy/Documents/Uni/cam/GAN/essay tex/',paste(sub_folder,'best_histograms.jpeg',sep = ''),sep = ''), units="in", width=16, height=12, res=300)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  collected_data[,3:ncol(collected_data)] = collected_data[,3:ncol(collected_data)] - 6
  
  
}

# Random histogram ----------------


if(random_histogram){ 
  sample_size = 1
  collected_data[,3:ncol(collected_data)] = collected_data[,3:ncol(collected_data)] + 6
  # only look at mean = 6 data points to give a nice visual picture
  rowmeans = rowMeans(collected_data[,3:ncol(collected_data)])
  use_data = collected_data[rowmeans>=5.8 & rowmeans<=6.2,]
  row_take = sample(1:nrow(use_data),size = sample_size,replace = F)
  panel_1 = data.frame(x = melt(use_data[row_take, 3:ncol(use_data)])[,2])
  x_limits = c(2,10)
  bin_number = 50
  title1 = ''
  print(ggplot(data.frame(x=panel_1), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 3, col = cols[1])+ xlab('G(x)')+ ylab('Density')+
    labs(title=title1)+xlim(x_limits[1],x_limits[2])+ theme_jose)
  
  
  collected_data[,3:ncol(collected_data)] = collected_data[,3:ncol(collected_data)] - 6
  
  print(collected_data[row_take,1:2])
  
}
# LOESS ################################################## 

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


if(loess_plot){
   
    par(mfrow = c(1,1))
    
    size_of_grid = 80
    
    fit_loess = loess(z~x*y,data =shapiro_data, span = 0.15)
    
    g_p_grid = expand.grid(list(x = seq(min(shapiro_data$x), max(shapiro_data$x), length.out = size_of_grid), 
                                     y = seq(min(shapiro_data$y),  max(shapiro_data$y), length.out = size_of_grid)))
    predicted_shapiro = predict(fit_loess, newdata = g_p_grid)
  
    new_data = melt(predicted_shapiro)
    new_data$x = sapply(new_data$x,function(x) as.numeric(gsub("x=", "", x)))
    new_data$y = sapply(new_data$x,function(x) as.numeric(gsub("y=", "", x)))
    trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
    p1 =wireframe(value ~ x * y, data = new_data, xlab = expression(gamma), par.settings = theme.novpadding,
              ylab = expression(phi), zlab = list('-Shapiro-Wilk Statistic',rot = 90), col = 'black',
              shade = F,pretty=T)
    print(p1)
    jpeg(paste('/Users/Billy/Documents/Uni/cam/GAN/essay tex/',paste(sub_folder,'loess.jpeg',sep = ''),sep = ''), units="in", width=7, height=7, res=300)
    trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
    print(p1)
    dev.off()
}  





# Mean and sd planes ################################################## 

mean_sd_data_set = data.frame(gamma = phi_gamma_data$Gamma , phi = phi_gamma_data$Phi,
                              mu = rowMeans(collected_data[,3:ncol(collected_data)])+6, 
                              sd = apply(collected_data[,3:ncol(collected_data)],1,sd))

if(plot_mean_sd){
    cols = gg_color_hue(2)
    
    size_of_grid = 500
    
    fit_loess = loess(mu~gamma*phi,mean_sd_data_set, span = 0.1)
    
    gamma = seq(min(mean_sd_data_set$gamma), max(mean_sd_data_set$gamma), length.out = size_of_grid)
    phi = seq(min(mean_sd_data_set$phi),  max(mean_sd_data_set$phi), length.out = size_of_grid)
    
    g_p_grid = expand.grid(list(gamma = seq(min(mean_sd_data_set$gamma), max(mean_sd_data_set$gamma), length.out = size_of_grid), 
                                phi = seq(min(mean_sd_data_set$phi),  max(mean_sd_data_set$phi), length.out = size_of_grid)))
    g_p_grid$mu = as.numeric(predict(fit_loess, newdata = g_p_grid))
    
    print(ggplot(g_p_grid, aes(x = gamma, y = phi, z = mu)) +geom_raster(aes(fill = mu)) +
      geom_contour(aes(z = mu, colour = factor(..level.. == 6, 
                                       levels = c(F, T))),breaks = -10:10) + 
      scale_colour_manual(values = c("black", 'lightblue')) +
      # scale_fill_gradientn(colours=c('black',"white"))+
      # scale_fill_gradientn(colours=c('black',"lightblue"))+
      guides(colour = FALSE)+theme_jose)
    
    
    
    fit_loess = loess(sd~gamma*phi,mean_sd_data_set, span = 0.1)
    
    gamma = seq(min(mean_sd_data_set$gamma), max(mean_sd_data_set$gamma), length.out = size_of_grid)
    phi = seq(min(mean_sd_data_set$phi),  max(mean_sd_data_set$phi), length.out = size_of_grid)
    
    g_p_grid = expand.grid(list(gamma = seq(min(mean_sd_data_set$gamma), max(mean_sd_data_set$gamma), length.out = size_of_grid), 
                                phi = seq(min(mean_sd_data_set$phi),  max(mean_sd_data_set$phi), length.out = size_of_grid)))
    g_p_grid$sd = as.numeric(predict(fit_loess, newdata = g_p_grid))
    
    print(ggplot(g_p_grid, aes(x = gamma, y = phi, z = sd)) +geom_raster(aes(fill = sd)) +
      geom_contour(aes(z = sd, 
                       colour = factor(..level.. == 1, 
                                       levels = c(F, T))),
                   breaks = seq(0,1.1, length.out = 80)) + 
      scale_colour_manual(values = c("black", 'lightblue')) +
      guides(colour = FALSE)+theme_jose)
  
}  



# 3d scatter ################################################## 

if(scatter_3D){
    with(shapiro_data, {
      scatterplot3d(x,   # x axis
                    y,     # y axis
                    z,    # z axis
                    xlab = bquote(gamma),
                    ylab = bquote(phi),
                    zlab = bquote('Shapiro')
      )
    })
}

# Interactive ################################################## 

if(interactive_plot){
  plot_ly(shapiro_data, x = ~x, y = ~y, z = ~z) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'Gamma'),
                        yaxis = list(title = 'Phi'),
                        zaxis = list(title = 'Inverese of Shapiro-Wilk Statistic')),
           annotations = list(
             x = 1.13,
             y = 1.05,
             xref = 'paper',
             yref = 'paper',
             showarrow = FALSE
           ))
}



# Factor plots ################################################## 


shapiro_data$gamma_factors = cut(shapiro_data$x, breaks = c(-Inf, gamma_boundaries, Inf), 
                                 labels = c('low','medium', 'high'), right = TRUE)


shapiro_data$phi_factors = cut(shapiro_data$y, breaks = c(-Inf, phi_boundaries, Inf), 
                               labels = c('zero','low', 'high'), right = TRUE)

if(phi_as_factor){
  shapiro_min = min(shapiro_data$z)
  shapiro_max = max(shapiro_data$z)
  colours = gg_color_hue(3)
  mean_1 = data.frame( x = c(-Inf, Inf), y = mean(shapiro_data$z[shapiro_data$phi_factors == 'zero']), 
                       cutoff = factor(mean(shapiro_data$z[shapiro_data$phi_factors == 'zero'])))
  mean_2 = data.frame( x = c(-Inf, Inf), y = mean(shapiro_data$z[shapiro_data$phi_factors == 'low']), 
                       cutoff = factor(mean(shapiro_data$z[shapiro_data$phi_factors == 'low'])))
  mean_3 = data.frame( x = c(-Inf, Inf), y = mean(shapiro_data$z[shapiro_data$phi_factors == 'high']), 
                       cutoff = factor(mean(shapiro_data$z[shapiro_data$phi_factors == 'high'])))
    
  p1 = ggplot(shapiro_data[shapiro_data$phi_factors == 'zero',], aes(x=x, y=z))+
    geom_point(col = colours[1])+ xlab(bquote(gamma))+ theme(legend.position="none")+
    ylab('-Shapiro-Wilk Statistic')+ggtitle(bquote(phi~'='~'0'))+theme_jose+
    ylim(shapiro_min,shapiro_max)+geom_line(aes( x, y, linetype = cutoff ), mean_1, col = 'darkgrey', lty = 3, lwd = 3)
  p2 = ggplot(shapiro_data[shapiro_data$phi_factors == 'low',], aes(x=x, y=z))+
    geom_point(col = colours[2])+ xlab(bquote(gamma))+theme(legend.position="none")+theme_jose+
    ggtitle(bquote(.(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5))))+
    ylim(shapiro_min,shapiro_max)+geom_line(aes( x, y, linetype = cutoff ), mean_2, col ='darkgrey', lty = 3, lwd = 3)
  p3 = ggplot(shapiro_data[shapiro_data$phi_factors == 'high',], aes(x=x, y=z))+theme_jose+
    geom_point(col = colours[3])+ xlab(bquote(gamma))+theme(legend.position="none")+
    ggtitle(bquote(.(round(phi_boundaries[2],5))~'<'~phi))+
    ylim(shapiro_min,shapiro_max)+geom_line(aes( x, y, linetype = cutoff ), mean_2, col = 'darkgrey', lty = 3, lwd = 3)
  multiplot(p1, p2, p3, cols=3)
  
}


if(gamma_as_factor){
  shapiro_min = min(shapiro_data$z)
  shapiro_max = max(shapiro_data$z)
  colours = gg_color_hue(3)
  p1 = ggplot(shapiro_data[shapiro_data$gamma_factors == 'low',], aes(x=y, y=z))+theme_jose+
    geom_point(col = colours[1])+geom_smooth(method = lm,se=FALSE,col ='darkgrey', lty = 2)+ xlab(bquote(phi))+
    ylab('-Shapiro-Wilk Statistic')+ggtitle(bquote(gamma~'<'~.(round(gamma_boundaries[1],5))))+
    ylim(shapiro_min,shapiro_max)
  p2 = ggplot(shapiro_data[shapiro_data$gamma_factors == 'medium',], aes(x=y, y=z))+theme_jose+
    geom_point(col = colours[2])+geom_smooth(method = lm,se=FALSE,col ='darkgrey', lty = 2)+ xlab(bquote(phi))+
    ggtitle(bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))))+
    ylim(shapiro_min,shapiro_max)
  p3 = ggplot(shapiro_data[shapiro_data$gamma_factors == 'high',], aes(x=y, y=z))+theme_jose+
    geom_point(col = colours[3])+geom_smooth(method = lm,se=FALSE,col ='darkgrey', lty = 2)+ xlab(bquote(phi))+
    ggtitle(bquote(.(round(gamma_boundaries[2],5))~'<'~gamma))+
    ylim(shapiro_min,shapiro_max)
  
  multiplot(p1, p2, p3, cols=3)
  
  
}

if(compare_means){
  
  phi_zeroes =  shapiro_data[shapiro_data$y == 0,]
  phi_not_zeroes = shapiro_data[shapiro_data$y != 0,]
  
  plot(phi_zeroes$x,phi_zeroes$z, pch = '.')
  points(phi_not_zeroes$x,phi_not_zeroes$z, pch = '.', col = 'red')
  abline(h = mean(phi_not_zeroes$z), col = 'red', lty = 2)
  abline(h = mean(phi_zeroes$z), lty = 2)
  
  phi_small = shapiro_data[phi_boundaries[1] < shapiro_data$y &
                             shapiro_data$y <= phi_boundaries[2],]
  
  plot(phi_zeroes$x,phi_zeroes$z, pch = '.')
  points(phi_small$x,phi_small$z, pch = '.', col = 'red')
  abline(h = mean(phi_small$z), col = 'red', lty = 2)
  abline(h = mean(phi_zeroes$z), lty = 2)
  

  
}

# KL divergence estimates ------------


## Create KL_dataset
full_KL = fread('/Users/Billy/PycharmProjects/GALR/data2/KL_divergence.csv',header = T)

reload_KL = T
if(reload_KL){
  
  real_dist = rnorm(10000 , 6,1)
  
  kl_to_real = function(vec){
    vec_1 = as.numeric(vec)
    return(KL.divergence(real_dist,vec_1,k=5)[5])
  }
  
  KL_vec = pbapply(old_data[,3:ncol(old_data)],MARGIN = 1,kl_to_real)
  
  KL_dataframe = data.frame(old_data[,1],old_data[,3],KL_vec )
  
  colnames(KL_dataframe) = c('x','y','z')
  
  write.csv(x = KL_dataframe, '/Users/Billy/PycharmProjects/GALR/data2/KL_divergence.csv',row.names = F)
}



accuracy_required = 0.5
KL_dataframe = full_KL[6-accuracy_required<=rmeans & rmeans <=6+accuracy_required,]


accuracy_required = 0.2
KL_dataframe = KL_dataframe[(1-accuracy_required<=sds & sds <=1+accuracy_required),]



# plot best histograms (KL) ----------


if(plot_best_histogram_KL){ 
  bin_number = 50
  
  collected_data[,3:ncol(collected_data)] = collected_data[,3:ncol(collected_data)] + 6
  
  # mean_close
  
  entries_per_group = 1
  
  
  panel_1 = collected_data[collected_data$Gamma<= gamma_boundaries[1] & 
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_2 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_3 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_4 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_5 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_6 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_7 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  panel_8 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  panel_9 = collected_data[gamma_boundaries[2] < collected_data$Gamma &  
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  
  
  # take the best shapiros
  KL_dataframe_1 = KL_dataframe[collected_data$Gamma<= gamma_boundaries[1] & 
                             collected_data$Phi == phi_boundaries[1],]
  KL_dataframe_2 = KL_dataframe[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             collected_data$Phi == phi_boundaries[1],]
  KL_dataframe_3 = KL_dataframe[gamma_boundaries[2] < collected_data$Gamma & 
                             collected_data$Phi == phi_boundaries[1],]
  KL_dataframe_4 = KL_dataframe[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],]
  KL_dataframe_5 = KL_dataframe[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],]
  KL_dataframe_6 = KL_dataframe[gamma_boundaries[2] < collected_data$Gamma & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],]
  KL_dataframe_7 = KL_dataframe[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[2]< collected_data$Phi,]
  KL_dataframe_8 = KL_dataframe[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[2]< collected_data$Phi,]
  KL_dataframe_9 = KL_dataframe[gamma_boundaries[2] < collected_data$Gamma &  
                             phi_boundaries[2]< collected_data$Phi,]
  
  
  
  panel_1 = data.frame(x = melt(panel_1[order(as.numeric(KL_dataframe_1$z), decreasing = F)[1:entries_per_group],])[,2])
  panel_2 = data.frame(x = melt(panel_2[order(as.numeric(KL_dataframe_2$z), decreasing = F)[1:entries_per_group],])[,2])
  panel_3 = data.frame(x = melt(panel_3[order(as.numeric(KL_dataframe_3$z), decreasing = F)[1:entries_per_group],])[,2])
  panel_4 = data.frame(x = melt(panel_4[order(as.numeric(KL_dataframe_4$z), decreasing = F)[1:entries_per_group],])[,2])
  panel_5 = data.frame(x = melt(panel_5[order(as.numeric(KL_dataframe_5$z), decreasing = F)[1:entries_per_group],])[,2])
  panel_6 = data.frame(x = melt(panel_6[order(as.numeric(KL_dataframe_6$z), decreasing = F)[1:entries_per_group],])[,2])
  panel_7 = data.frame(x = melt(panel_7[order(as.numeric(KL_dataframe_7$z), decreasing = F)[1:entries_per_group],])[,2])
  panel_8 = data.frame(x = melt(panel_8[order(as.numeric(KL_dataframe_8$z), decreasing = F)[1:entries_per_group],])[,2])
  panel_9 = data.frame(x = melt(panel_9[order(as.numeric(KL_dataframe_9$z), decreasing = F)[1:entries_per_group],])[,2])
  
  
  # adjust so the scales are the same
  
  pull_max_density =function(panel){
    min_break = round_any(min(panel$x), diff(range(panel$x))/bin_number, floor)
    max_break = round_any(max(panel$x), diff(range(panel$x))/bin_number, ceiling)
    breaks = seq(min_break, max_break, diff(range(panel$x/bin_number)))
    histo = hist(panel$x, breaks=breaks, plot=F)
    return(max(histo$density))
  }
  
  max_density = max(c(pull_max_density(panel_1),pull_max_density(panel_2),pull_max_density(panel_3),
                      pull_max_density(panel_4),pull_max_density(panel_5),pull_max_density(panel_6),
                      pull_max_density(panel_7),pull_max_density(panel_8),pull_max_density(panel_9)))
  
  
  
  # chart titles
  
  title1 = bquote(gamma~ '<' ~.(round(gamma_boundaries[1],5))~', '~phi~'='~.(round(phi_boundaries[1],5)))
  
  title2 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~', '
                  ~phi~'='~.(round(phi_boundaries[1],5)))
  
  title3 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~phi~'='~
                    .(round(phi_boundaries[1],5)))
  
  title4 = bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '~.(round(phi_boundaries[1],5))~'<'
                  ~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title5 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~', '~
                    .(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title6 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~
                    .(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title7 = bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  title8 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~
                    ', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  title9 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~
                    ', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  # ggplot 
  x_limits = c(2,10)
  
  p1 = ggplot(data.frame(x=panel_1), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ xlab('')+ ylab('')+
    labs(title=title1)+theme_jose_qq+ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p2 = ggplot(data.frame(x=panel_2), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ xlab('')+ ylab('')+
    labs(title=title2)+theme_jose_qq+ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p3 = ggplot(data.frame(x=panel_3), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title3)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p4 = ggplot(data.frame(x=panel_4), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title4)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p5 = ggplot(data.frame(x=panel_5), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title5)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p6 = ggplot(data.frame(x=panel_6), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title6)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p7 = ggplot(data.frame(x=panel_7), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title7)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p8 = ggplot(data.frame(x=panel_8), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title8)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  p9 = ggplot(data.frame(x=panel_9), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                  col = cols[1])+ xlab('')+ ylab('')+labs(title=title9)+theme_jose_qq+
    ylim(0,max_density)+xlim(x_limits[1],x_limits[2])
  
  
  
  jpeg(paste('/Users/Billy/Documents/Uni/cam/GAN/essay tex/',paste(sub_folder,'best_histograms_KL.jpeg',sep = ''),sep = ''), units="in", width=16, height=12, res=300)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  collected_data[,3:ncol(collected_data)] = collected_data[,3:ncol(collected_data)] - 6

  
}


# QQ KL ------

if(plot_best_QQ_KL){ 
  bin_number = 50

  
  # mean_close
  
  entries_per_group = 1
  
  
  panel_1 = collected_data[collected_data$Gamma<= gamma_boundaries[1] & 
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_2 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_3 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  panel_4 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_5 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_6 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                             phi_boundaries[1] < collected_data$Phi &
                             collected_data$Phi <= phi_boundaries[2],3:ncol(collected_data)]
  
  panel_7 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  panel_8 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                             collected_data$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  panel_9 = collected_data[gamma_boundaries[2] < collected_data$Gamma &  
                             phi_boundaries[2]< collected_data$Phi,3:ncol(collected_data)]
  
  
  
  # take the best shapiros
  KL_dataframe_1 = KL_dataframe[collected_data$Gamma<= gamma_boundaries[1] & 
                                  collected_data$Phi == phi_boundaries[1],]
  KL_dataframe_2 = KL_dataframe[gamma_boundaries[1]< collected_data$Gamma &  
                                  collected_data$Gamma <= gamma_boundaries[2] &
                                  collected_data$Phi == phi_boundaries[1],]
  KL_dataframe_3 = KL_dataframe[gamma_boundaries[2] < collected_data$Gamma & 
                                  collected_data$Phi == phi_boundaries[1],]
  KL_dataframe_4 = KL_dataframe[collected_data$Gamma <= gamma_boundaries[1] & 
                                  phi_boundaries[1] < collected_data$Phi &
                                  collected_data$Phi <= phi_boundaries[2],]
  KL_dataframe_5 = KL_dataframe[gamma_boundaries[1]< collected_data$Gamma &  
                                  collected_data$Gamma <= gamma_boundaries[2] &
                                  phi_boundaries[1] < collected_data$Phi &
                                  collected_data$Phi <= phi_boundaries[2],]
  KL_dataframe_6 = KL_dataframe[gamma_boundaries[2] < collected_data$Gamma & 
                                  phi_boundaries[1] < collected_data$Phi &
                                  collected_data$Phi <= phi_boundaries[2],]
  KL_dataframe_7 = KL_dataframe[collected_data$Gamma <= gamma_boundaries[1] & 
                                  phi_boundaries[2]< collected_data$Phi,]
  KL_dataframe_8 = KL_dataframe[gamma_boundaries[1]< collected_data$Gamma &  
                                  collected_data$Gamma <= gamma_boundaries[2] &
                                  phi_boundaries[2]< collected_data$Phi,]
  KL_dataframe_9 = KL_dataframe[gamma_boundaries[2] < collected_data$Gamma &  
                                  phi_boundaries[2]< collected_data$Phi,]
  
  
  panel_1 = panel_1[order(as.numeric(KL_dataframe_1$z), decreasing = F)[1:entries_per_group],]
  panel_2 = panel_2[order(as.numeric(KL_dataframe_2$z), decreasing = F)[1:entries_per_group],]
  panel_3 = panel_3[order(as.numeric(KL_dataframe_3$z), decreasing = F)[1:entries_per_group],]
  panel_4 = panel_4[order(as.numeric(KL_dataframe_4$z), decreasing = F)[1:entries_per_group],]
  panel_5 = panel_5[order(as.numeric(KL_dataframe_5$z), decreasing = F)[1:entries_per_group],]
  panel_6 = panel_6[order(as.numeric(KL_dataframe_6$z), decreasing = F)[1:entries_per_group],]
  panel_7 = panel_7[order(as.numeric(KL_dataframe_7$z), decreasing = F)[1:entries_per_group],]
  panel_8 = panel_8[order(as.numeric(KL_dataframe_8$z), decreasing = F)[1:entries_per_group],]
  panel_9 = panel_9[order(as.numeric(KL_dataframe_9$z), decreasing = F)[1:entries_per_group],]

  
  
  # chart titles
  
  title1 = bquote(gamma~ '<' ~.(round(gamma_boundaries[1],5))~', '~phi~'='~.(round(phi_boundaries[1],5)))
  
  title2 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~', '
                  ~phi~'='~.(round(phi_boundaries[1],5)))
  
  title3 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~phi~'='~
                    .(round(phi_boundaries[1],5)))
  
  title4 = bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '~.(round(phi_boundaries[1],5))~'<'
                  ~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title5 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~', '~
                    .(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title6 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~
                    .(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5)))
  
  title7 = bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  title8 = bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))~
                    ', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  title9 = bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~
                    ', '~.(round(phi_boundaries[2],5))~'<'~phi)
  
  # ggplot 
  
  p1 = qqplot_by_row2(panel_3,title3,xlab = '',ylab = '')
  p2 = qqplot_by_row2(panel_2,title2,xlab = '',ylab = '')
  p3 = qqplot_by_row2(panel_1,title1,xlab = '',ylab = '')
  p4 = qqplot_by_row2(panel_6,title6,xlab = '',ylab = '')
  p5 = qqplot_by_row2(panel_5,title5,xlab = '',ylab = '')
  p6 = qqplot_by_row2(panel_4,title4,xlab = '',ylab = '')
  p7 = qqplot_by_row2(panel_9,title9,xlab = '',ylab = '')
  p8 = qqplot_by_row2(panel_8,title8,xlab = '',ylab = '')
  p9 = qqplot_by_row2(panel_7,title7,xlab = '',ylab = '')
  jpeg(paste('/Users/Billy/Documents/Uni/cam/GAN/essay tex/',paste(sub_folder,'QQ_KL.jpeg',sep = ''),sep = '')
       , units="in", width=16, height=12, res=300)
  multiplot_QQ(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  
}




# LOESS KL plot ------------
KL_dataframe = full_KL

plot(KL_dataframe$x, KL_dataframe$y)

if(loess_plot_KL){
  
  par(mfrow = c(1,1))
  
  size_of_grid = 80
  
  fit_loess = loess(z~x*y,data =KL_dataframe, span = 0.15)
  
  g_p_grid = expand.grid(list(x = seq(min(KL_dataframe$x), max(KL_dataframe$x), length.out = size_of_grid), 
                              y = seq(min(KL_dataframe$y),  max(KL_dataframe$y), length.out = size_of_grid)))
  predicted_shapiro = predict(fit_loess, newdata = g_p_grid)
  
  new_data = melt(predicted_shapiro)
  new_data$x = sapply(new_data$x,function(x) as.numeric(gsub("x=", "", x)))
  new_data$y = sapply(new_data$x,function(x) as.numeric(gsub("y=", "", x)))
  trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
  p1 =wireframe(value ~ x * y, data = new_data, xlab = expression(gamma), par.settings = theme.novpadding,
                ylab = expression(phi), zlab = list('Estimated KL Divergence',rot = 90), col = 'black',
                shade = F,pretty=T)
  print(p1)
  jpeg(paste('/Users/Billy/Documents/Uni/cam/GAN/essay tex/',paste(sub_folder,'loess_KL.jpeg',sep = ''),sep = ''), units="in", width=7, height=7, res=300)
  trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
  print(p1)
  dev.off()
}  



# Factor plots KL ################################################## 

KL_dataframe$gamma_factors = cut(KL_dataframe$x, breaks = c(-Inf, gamma_boundaries, Inf), 
                                 labels = c('low','medium', 'high'), right = TRUE)


KL_dataframe$phi_factors = cut(KL_dataframe$y, breaks = c(-Inf, phi_boundaries, Inf), 
                               labels = c('zero','low', 'high'), right = TRUE)

if(phi_as_factor_KL){
  KL_min = min(KL_dataframe$z)
  KL_max = max(KL_dataframe$z)
  colours = gg_color_hue(3)
  mean_1 = data.frame( x = c(-Inf, Inf), y = mean(KL_dataframe$z[KL_dataframe$phi_factors == 'zero']), 
                       cutoff = factor(mean(KL_dataframe$z[KL_dataframe$phi_factors == 'zero'])))
  mean_2 = data.frame( x = c(-Inf, Inf), y = mean(KL_dataframe$z[KL_dataframe$phi_factors == 'low']), 
                       cutoff = factor(mean(KL_dataframe$z[KL_dataframe$phi_factors == 'low'])))
  mean_3 = data.frame( x = c(-Inf, Inf), y = mean(KL_dataframe$z[KL_dataframe$phi_factors == 'high']), 
                       cutoff = factor(mean(KL_dataframe$z[KL_dataframe$phi_factors == 'high'])))
  
  p1 = ggplot(KL_dataframe[KL_dataframe$phi_factors == 'zero',], aes(x=x, y=z))+
    geom_point(col = colours[1])+ xlab(bquote(gamma))+ theme(legend.position="none")+
    ylab('-Shapiro-Wilk Statistic')+ggtitle(bquote(phi~'='~'0'))+theme_jose+
    ylim(KL_min,KL_max)+geom_line(aes( x, y, linetype = cutoff ), mean_1, col = 'darkgrey', lty = 3, lwd = 3)
  p2 = ggplot(KL_dataframe[KL_dataframe$phi_factors == 'low',], aes(x=x, y=z))+
    geom_point(col = colours[2])+ xlab(bquote(gamma))+theme(legend.position="none")+theme_jose+
    ggtitle(bquote(.(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5))))+
    ylim(KL_min,KL_max)+geom_line(aes( x, y, linetype = cutoff ), mean_2, col ='darkgrey', lty = 3, lwd = 3)
  p3 = ggplot(KL_dataframe[KL_dataframe$phi_factors == 'high',], aes(x=x, y=z))+theme_jose+
    geom_point(col = colours[3])+ xlab(bquote(gamma))+theme(legend.position="none")+
    ggtitle(bquote(.(round(phi_boundaries[2],5))~'<'~phi))+
    ylim(KL_min,KL_max)+geom_line(aes( x, y, linetype = cutoff ), mean_2, col = 'darkgrey', lty = 3, lwd = 3)
  multiplot(p1, p2, p3, cols=3)
  
}


