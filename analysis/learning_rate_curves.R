rm(list = ls(all=T))
library(reshape2)
library(ggplot2)

save_picture_name = function(name){
  dir = paste(c('/Users/Billy/Documents/Uni/cam/GAN/essay tex/plots_pictures/',name,'.jpeg'),
              collapse = '')
  return(dir)
}


export_theme1 = theme(text=element_text(size=40),
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

x_seq = seq(-5,0,length.out = 1000)
n = length(x_seq)

d_point = function(x,phi_g,phi_d,gamma_g,gamma_d){
  psi = log((gamma_d - gamma_g - (2*phi_g) - phi_d)/(-gamma_d+gamma_g-phi_d))/log(16.0)
  learning_rate_d = gamma_d-phi_d*tanh(psi*x)
  return(learning_rate_d)
}


g_point = function(x,phi_g,phi_d,gamma_g,gamma_d){
  psi = log((gamma_d - gamma_g - (2*phi_g) - phi_d)/(-gamma_d+gamma_g-phi_d))/log(16.0)
  learning_rate_g = gamma_g + phi_g*(1 + tanh(psi*x))
  return(learning_rate_g)
}

d_curve = function(phi_g,phi_d,gamma_g,gamma_d){
  return(sapply(x_seq,d_point,phi_g=phi_g,phi_d=phi_d,gamma_g=gamma_g,gamma_d=gamma_d))
}

g_curve = function(phi_g,phi_d,gamma_g,gamma_d){
  return(sapply(x_seq,g_point,phi_g=phi_g,phi_d=phi_d,gamma_g=gamma_g,gamma_d=gamma_d))
}

both_curves = function(phi_g,phi_d,gamma_g,gamma_d){
  d = d_curve(phi_g,phi_d,gamma_g,gamma_d) 
  g = g_curve(phi_g,phi_d,gamma_g,gamma_d)
  df = data.frame(x_seq,d,g)
  return(df)
}

df1 = both_curves(0.2,0.2,1,1)
df2 = both_curves(1,1,1,1)
df3 = both_curves(1,1,0.2,0.2)

all_data = merge(merge(df1,df2,by = 'x_seq'),df3,by = 'x_seq')

legendtitle = bquote(phi~', '~ gamma)
col1 = bquote(phi~'= 0.2, '~ gamma~ '= 1')
col2 = bquote(phi~'= 1, '~ gamma~ '= 1')
col3 = bquote(phi~'= 1, '~ gamma~ '= 0.2')



p1 = ggplot(all_data, aes(x_seq)) + 
  ylim(min(all_data[,-1]),max(all_data[,-1]))+
  geom_line(aes(y = d.y, colour = 'var0')) + 
  geom_line(aes(y = g.y,  colour = "var0"))+
  geom_line(aes(y = d.x,  colour = "var1")) + 
  geom_line(aes(y = g.x,  colour = "var1"))+
  geom_line(aes(y = d,  colour = "var2")) + 
  geom_line(aes(y = g,  colour = "var2"))+
  xlab(expression(hat(V)~'(D,G)'))+
  ylab('Learning Rates')+
  geom_vline(xintercept = -log(4), lty = 2)+
  scale_colour_discrete(name  =legendtitle,
                        breaks=c("var0", "var1",'var2'),
                        labels=c(col1, col2, col3))


jpeg(save_picture_name('learning_rates'), units="in", width=5, height=3, res=300)
print(p1)
dev.off()


# seperate plots 




df1 = both_curves(1,1,1.5,1.5)
df2 = both_curves(1,1,1,1)
df3 = both_curves(1,1,0.5,0.5)

all_data = merge(merge(df1,df2,by = 'x_seq'),df3,by = 'x_seq')

legendtitle = bquote(phi~', '~ gamma)
col1 = bquote(phi~'= 1, '~ gamma~ '= 1.5')
col2 = bquote(phi~'= 1, '~ gamma~ '= 1')
col3 = bquote(phi~'= 1, '~ gamma~ '= 0.5')



p1 = ggplot(all_data, aes(x_seq)) + 
  ylim(min(all_data[,-1]),max(all_data[,-1]))+
  geom_line(aes(y = d.y, colour = 'var0')) + 
  geom_line(aes(y = g.y,  colour = "var0"))+
  geom_line(aes(y = d.x,  colour = "var1")) + 
  geom_line(aes(y = g.x,  colour = "var1"))+
  geom_line(aes(y = d,  colour = "var2")) + 
  geom_line(aes(y = g,  colour = "var2"))+
  xlab(expression(hat(V)~'(D,G)'))+
  ylab('Learning Rates')+
  geom_vline(xintercept = -log(4), lty = 2)+
  scale_colour_discrete(name  =legendtitle,
                        breaks=c("var0", "var1",'var2'),
                        labels=c(col1, col2, col3))


jpeg(save_picture_name('learning_rates_change_gamma'), units="in", width=5, height=3, res=300)
print(p1)
dev.off()



df1 = both_curves(1.5,1.5,1,1)
df2 = both_curves(1,1,1,1)
df3 = both_curves(0.5,0.5,1,1)

all_data = merge(merge(df1,df2,by = 'x_seq'),df3,by = 'x_seq')

legendtitle = bquote(phi~', '~ gamma)
col1 = bquote(phi~'= 1.5, '~ gamma~ '= 1')
col2 = bquote(phi~'= 1, '~ gamma~ '= 1')
col3 = bquote(phi~'= 0.5, '~ gamma~ '= 1')



p1 = ggplot(all_data, aes(x_seq)) + 
  ylim(min(all_data[,-1]),max(all_data[,-1]))+
  geom_line(aes(y = d.y, colour = 'var0')) + 
  geom_line(aes(y = g.y,  colour = "var0"))+
  geom_line(aes(y = d.x,  colour = "var1")) + 
  geom_line(aes(y = g.x,  colour = "var1"))+
  geom_line(aes(y = d,  colour = "var2")) + 
  geom_line(aes(y = g,  colour = "var2"))+
  xlab(expression(hat(V)~'(D,G)'))+
  ylab('Learning Rates')+
  geom_vline(xintercept = -log(4), lty = 2)+
  scale_colour_discrete(name  =legendtitle,
                        breaks=c("var0", "var1",'var2'),
                        labels=c(col1, col2, col3))


jpeg(save_picture_name('learning_rates_change_phi'), units="in", width=5, height=3, res=300)
print(p1)
dev.off()




