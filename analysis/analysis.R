##################################################
##################################################
#
#   
#
##################################################
##################################################
rm(list = ls(all=T))
library(reshape2)
library(plyr)
library(ggplot2)
library(scatterplot3d)
library(plotly)
library(lattice)
source('/Users/Billy/PycharmProjects/GALR/mulitplot.R')

############## WHICH PLOTS #######################
##################################################
phi_against_gamma = TRUE
plot_histograms = FALSE
interactive_plot = F
scatter_3D = FALSE
loess_plot = T
phi_as_factor = F
gamma_as_factor = F

##################################################
sub_folder = 'gd'

##################################################
################COLOURS###########################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(2)

##################################################
##################################################

data_path = paste('/Users/Billy/PycharmProjects/GALR/data/',sub_folder, sep = '')
setwd(data_path)

files = list.files(pattern = '\\.csv')
tables = lapply(files, read.csv, header = FALSE)
collected_data =do.call(rbind , tables)
collected_data = na.omit(collected_data)
colnames(collected_data) = c('Gamma','Phi',3:ncol(collected_data))

gamma_boundaries= c(min(collected_data$Gamma)+
                      (max(collected_data$Gamma)-min(collected_data$Gamma))/3 ,
                    min(collected_data$Gamma)+
                      2*(max(collected_data$Gamma)-min(collected_data$Gamma))/3)

phi_boundaries = c(min(collected_data$Phi),
                   min(collected_data$Phi)+
                     (max(collected_data$Phi)-min(collected_data$Phi))/2)

if(phi_against_gamma){
  cols = gg_color_hue(2)
  ggplot(collected_data,aes(x = Gamma, y = Phi))+geom_point(col=cols[1])+xlab(bquote(gamma))+ylab(bquote(phi))
}

nrow(collected_data)
mean(collected_data$Phi == min(collected_data$Phi)) # proportion phi zeroes have we collected


# create 9 paneled histogram picture

if(plot_histograms){ 
      
      entries_per_group = 10
      
      panel_1 = collected_data[collected_data$Gamma<= gamma_boundaries[1] & 
                                 collected_data$Phi == phi_boundaries[1],3:1002]
      
      panel_2 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                                 collected_data$Gamma <= gamma_boundaries[2] &
                                 collected_data$Phi == phi_boundaries[1],3:1002]
      
      panel_3 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                                 collected_data$Phi == phi_boundaries[1],3:1002]
      
      panel_4 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                                  phi_boundaries[1] < collected_data$Phi &
                                 collected_data$Phi <= phi_boundaries[2],3:1002]
      
      panel_5 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                                 collected_data$Gamma <= gamma_boundaries[2] &
                                 phi_boundaries[1] < collected_data$Phi &
                                 collected_data$Phi <= phi_boundaries[2],3:1002]
      
      panel_6 = collected_data[gamma_boundaries[2] < collected_data$Gamma & 
                                 phi_boundaries[1] < collected_data$Phi &
                                 collected_data$Phi <= phi_boundaries[2],3:1002]
      
      panel_7 = collected_data[collected_data$Gamma <= gamma_boundaries[1] & 
                                  phi_boundaries[2]< collected_data$Phi,3:1002]
      
      panel_8 = collected_data[gamma_boundaries[1]< collected_data$Gamma &  
                                 collected_data$Gamma <= gamma_boundaries[2] &
                                 phi_boundaries[2]< collected_data$Phi,3:1002]
      
      panel_9 = collected_data[gamma_boundaries[2] < collected_data$Gamma &  
                                 phi_boundaries[2]< collected_data$Phi,3:1002]
      
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
        min_break = round_any(min(panel$x), diff(range(panel$x))/30, floor)
        max_break = round_any(max(panel$x), diff(range(panel$x))/30, ceiling)
        breaks = seq(min_break, max_break, diff(range(panel$x/30)))
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
      
      p1 = ggplot(data.frame(x=panel_1), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ xlab('')+ ylab('')+
          labs(title=title1)+ theme(plot.title = element_text(size=8))+ylim(0,max_density)+xlim(4,8)
      p2 = ggplot(data.frame(x=panel_2), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ xlab('')+ ylab('')+
        labs(title=title2)+ theme(plot.title = element_text(size=8))+ylim(0,max_density)+xlim(4,8)
      p3 = ggplot(data.frame(x=panel_3), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title3)+ theme(plot.title = element_text(size=8))+
        ylim(0,max_density)+xlim(4,8)
      p4 = ggplot(data.frame(x=panel_4), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title4)+ theme(plot.title = element_text(size=8))+
        ylim(0,max_density)+xlim(4,8)
      p5 = ggplot(data.frame(x=panel_5), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title5)+ theme(plot.title = element_text(size=8))+
        ylim(0,max_density)+xlim(4,8)
      p6 = ggplot(data.frame(x=panel_6), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title6)+ theme(plot.title = element_text(size=8))+
        ylim(0,max_density)+xlim(4,8)
      p7 = ggplot(data.frame(x=panel_7), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title7)+ theme(plot.title = element_text(size=8))+
        ylim(0,max_density)+xlim(4,8)
      p8 = ggplot(data.frame(x=panel_8), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title8)+ theme(plot.title = element_text(size=8))+
        ylim(0,max_density)+xlim(4,8)
      p9 = ggplot(data.frame(x=panel_9), aes(x)) + geom_histogram(fill = cols[2],aes(y = ..density..))+
        stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, 
                      col = cols[1])+ xlab('')+ ylab('')+labs(title=title9)+ theme(plot.title = element_text(size=8))+
        ylim(0,max_density)+xlim(4,8)
      
      multiplot(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
      print(paste('Sample size:',sample_size))
}

# plot shapiro surface

normal_data = collected_data[,3:ncol(collected_data)]
normal_data = normal_data - 6 # de-mean


pull_shapiro=function(row){
  return(as.numeric(shapiro.test(as.numeric(normal_data[row,]))$statistic))
}

pull_shapiro2=function(row){
  return(as.numeric(shapiro.test(as.numeric(normal_data[row,]))$p.value))
}


row_var = function(row){
  return(var(as.numeric(normal_data[row,])))
}

non_collapsed_indices = ! sapply(1:nrow(normal_data),row_var) ==0

phi_gamma_data = collected_data[non_collapsed_indices, 1:2] # remove complete mode collapse
normal_data = normal_data[non_collapsed_indices, ] # remove complete mode collapse


shapiro_stats = 1-sapply(1:nrow(normal_data),pull_shapiro)

shapiro_data = data.frame(x = phi_gamma_data$Gamma, y = phi_gamma_data$Phi,z = shapiro_stats)

if(loess_plot){
  
    par(mfrow = c(1,1))
    for(theta_1 in c(45,135,225,315)){
      
        size_of_grid = 80
        
        fit_loess = loess(z~x*y,data =shapiro_data, span = 0.1)
        
        g_p_grid = expand.grid(list(x = seq(min(shapiro_data$x), max(shapiro_data$x), length.out = size_of_grid), 
                                         y = seq(min(shapiro_data$y),  max(shapiro_data$y), length.out = size_of_grid)))
        predicted_shapiro = predict(fit_loess, newdata = g_p_grid)
        
        persp(seq(min(shapiro_data$x), max(shapiro_data$x), length.out = size_of_grid),
              y = seq(min(shapiro_data$y),  max(shapiro_data$y), length.out = size_of_grid),
              predicted_shapiro, theta = theta_1,phi = 20, # theta = -45,phi = 20,
              xlab = "Gamma", ylab = "Phi",zlab = 'Shapiro-Wilk Statistic', main = "")
    }
}  

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
    ylab(expression(frac(1,'Shapiro-Wilk Statistic')))+ggtitle(bquote(phi~'='~'0'))+
    ylim(shapiro_min,shapiro_max)+geom_line(aes( x, y, linetype = cutoff ), mean_1, col = cols[1], lty = 2, lwd = 3)
  p2 = ggplot(shapiro_data[shapiro_data$phi_factors == 'low',], aes(x=x, y=z))+
    geom_point(col = colours[2])+ xlab(bquote(gamma))+theme(legend.position="none")+
    ggtitle(bquote(.(round(phi_boundaries[1],5))~'<'~phi~'<'~.(round(phi_boundaries[2],5))))+
    ylim(shapiro_min,shapiro_max)+geom_line(aes( x, y, linetype = cutoff ), mean_2, col = cols[2], lty = 2, lwd = 3)
  p3 = ggplot(shapiro_data[shapiro_data$phi_factors == 'high',], aes(x=x, y=z))+
    geom_point(col = colours[3])+ xlab(bquote(gamma))+theme(legend.position="none")+
    ggtitle(bquote(.(round(phi_boundaries[2],5))~'<'~phi))+
    ylim(shapiro_min,shapiro_max)+geom_line(aes( x, y, linetype = cutoff ), mean_2, col = cols[3], lty = 2, lwd = 3)
  multiplot(p1, p2, p3, cols=3)
  
}


if(gamma_as_factor){
  shapiro_min = min(shapiro_data$z)
  shapiro_max = max(shapiro_data$z)
  colours = gg_color_hue(3)
  p1 = ggplot(shapiro_data[shapiro_data$gamma_factors == 'low',], aes(x=y, y=z))+
    geom_point(col = colours[1])+geom_smooth(method = lm,se=FALSE,col =cols[1])+ xlab(bquote(phi))+
    ylab(expression(frac(1,'Shapiro-Wilk Statistic')))+ggtitle(bquote(gamma~'<'~.(round(gamma_boundaries[1],5))))+
    ylim(shapiro_min,shapiro_max)
  p2 = ggplot(shapiro_data[shapiro_data$gamma_factors == 'medium',], aes(x=y, y=z))+
    geom_point(col = colours[2])+geom_smooth(method = lm,se=FALSE,col =cols[2])+ xlab(bquote(phi))+
    ggtitle(bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'~.(round(gamma_boundaries[2],5))))+
    ylim(shapiro_min,shapiro_max)
  p3 = ggplot(shapiro_data[shapiro_data$gamma_factors == 'high',], aes(x=y, y=z))+
    geom_point(col = colours[3])+geom_smooth(method = lm,se=FALSE,col =cols[2])+ xlab(bquote(phi))+
    ggtitle(bquote(.(round(gamma_boundaries[2],5))~'<'~gamma))+
    ylim(shapiro_min,shapiro_max)
  
  multiplot(p1, p2, p3, cols=3)
  
  
}



