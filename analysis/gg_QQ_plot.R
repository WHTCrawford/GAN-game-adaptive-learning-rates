
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


qqplot_by_row <- function(data_frame,title = '', title_font_size = 8,xlab ='Theoretical Quantiles'
                          ,ylab = 'Actual Quantiles'){ # data frame has rows, we went to look at the qqplot for each row
  
  cols = gg_color_hue(nrow(data_frame))
  slope_and_int = data.frame(matrix(nrow = nrow(data_frame),ncol = 2))
  qq = data.frame(name = NA, resids = NA)
  for(i in 1:nrow(data_frame)){ # i = 1
    vec = as.numeric(data_frame[i,!is.na(data_frame[i])])
    y = quantile(vec, c(0.25, 0.75))
    x = qnorm(c(0.25, 0.75))
    slope = diff(y)/diff(x)
    int = y[1L] - slope * x[1L]
    slope_and_int[i,1:2] = c(slope,int)
    new_dataframe = data.frame(name = replicate(length(vec),paste('row.',i,sep='')),resids = vec)
    qq = rbind(qq,new_dataframe)
  }
  qq = na.omit(qq)
  
  require(ggplot2)
  ggplot(qq) +
    stat_qq(aes(sample = resids, color = factor(name)))+
    geom_abline(slope = slope_and_int[,1], intercept = slope_and_int[,2], col = cols)+
    theme(legend.position="none")+labs(title=title)+
    theme(plot.title = element_text(size=title_font_size))+xlab(xlab)+ylab(ylab)
}


qqplot_by_row2 <- function(data_frame,title = '', title_font_size = 8,xlab ='Theoretical Quantiles'
                          ,ylab = 'Actual Quantiles'){ # data frame has rows, we went to look at the qqplot for each row
  
  cols = gg_color_hue(nrow(data_frame))
  slope_and_int = data.frame(matrix(nrow = nrow(data_frame),ncol = 2))
  qq = data.frame(name = NA, resids = NA)
  for(i in 1:nrow(data_frame)){ # i = 1
    vec = as.numeric(data_frame[i,!is.na(data_frame[i])])
    y = quantile(vec, c(0.25, 0.75))
    x = qnorm(c(0.25, 0.75))
    slope = diff(y)/diff(x)
    int = y[1L] - slope * x[1L]
    slope_and_int[i,1:2] = c(slope,int)
    new_dataframe = data.frame(name = replicate(length(vec),paste('row.',i,sep='')),resids = vec)
    qq = rbind(qq,new_dataframe)
  }
  qq = na.omit(qq)
  
  require(ggplot2)
  ggplot(qq) +
    stat_qq(aes(sample = resids, color = factor(name)))+
    geom_abline(slope = 1, intercept = 0, col = 'black', lty = 2)+
    theme(legend.position="none")+labs(title=title)+
    theme(plot.title = element_text(size=title_font_size))+xlab(xlab)+ylab(ylab)+
    ylim(-3,3)+
    xlim(-3,3)
}

# testing
testing = FALSE
if(testing){

  
  sub_folder = 'momentum0.6'
  data_path = paste('/Users/Billy/PycharmProjects/GALR/data2/',sub_folder, sep = '')
  setwd(data_path)
  
  collected_data = read.csv('output.csv')
  collected_data = na.omit(collected_data)
  colnames(collected_data) = c('Gamma','Phi',3:ncol(collected_data))
  
  
  gamma_boundaries= c(min(collected_data$Gamma)+
                        (max(collected_data$Gamma)-min(collected_data$Gamma))/3 ,
                      min(collected_data$Gamma)+
                        2*(max(collected_data$Gamma)-min(collected_data$Gamma))/3)
  
  phi_boundaries = c(min(collected_data$Phi),
                     min(collected_data$Phi)+
                       (max(collected_data$Phi)-min(collected_data$Phi))/2)
  
  
  collected_data[,3:ncol(collected_data)] = collected_data[,3:ncol(collected_data)] - 6 # de-mean
  
  
  panel_1 = collected_data[collected_data$Gamma<= gamma_boundaries[1] & 
                             collected_data$Phi == phi_boundaries[1],3:ncol(collected_data)]
  
  sample_size = 2
  
  panel_1 = panel_1[sample(1:nrow(panel_1),size = sample_size,replace = F),]
  
  title1 = bquote(gamma~ '<' ~.(round(gamma_boundaries[1],5))~', '~phi~'='~.(round(phi_boundaries[1],5)))
  
  data_frame = panel_1
  cols = gg_color_hue(nrow(data_frame))
  slope_and_int = data.frame(matrix(nrow = nrow(data_frame),ncol = 2))
  qq = data.frame(name = NA, resids = NA)
  for(i in 1:nrow(data_frame)){ # i = 1
    vec = as.numeric(data_frame[i,!is.na(data_frame[i])])
    y = quantile(vec, c(0.25, 0.75))
    x = qnorm(c(0.25, 0.75))
    slope = diff(y)/diff(x)
    int = y[1L] - slope * x[1L]
    slope_and_int[i,1:2] = c(slope,int)
    new_dataframe = data.frame(name = replicate(length(vec),paste('row.',i,sep='')),resids = vec)
    qq = rbind(qq,new_dataframe)
  }
  qq = na.omit(qq)
  
  title = ''
  title_font_size = 8
  xlab ='Theoretical Quantiles'
  ylab = 'Actual Quantiles'
  
  
  require(ggplot2)
  ggplot(qq) +
    stat_qq(aes(sample = resids, color = factor(name)))+
    geom_abline(slope = slope_and_int[,1], intercept = slope_and_int[,2], col = cols)+
    theme(legend.position="none")+labs(title=title)+
    theme(plot.title = element_text(size=title_font_size))+xlab(xlab)+ylab(ylab)
  
  
  qqplot_by_row(panel_1)
  qqplot_by_row2(panel_1)
}