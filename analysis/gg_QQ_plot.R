
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
  
  line_colour = gg_color_hue(nrow(data_frame)+1)[nrow(data_frame)+1]
  
  require(ggplot2)
  ggplot(qq) +
    stat_qq(aes(sample = resids, color = factor(name)))+
    geom_abline(slope = 1, intercept = 0, col = line_colour, lty = 2)+
    theme(legend.position="none")+labs(title=title)+
    theme(plot.title = element_text(size=title_font_size))+xlab(xlab)+ylab(ylab)+
    ylim(-3,3)+
    xlim(-3,3)
}
