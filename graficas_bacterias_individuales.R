#grafica de experimetnos bacterias individuales con protocolo comsint  
#eje x: horas transcurridas totales 
#eje y: OD600


library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(ggpubr)

#cambiar dependiendo el archivo 
data_1 <- read_ods("C:/Users/natal/Documents/LIIGH/data_test_single_strains (1).ods", sheet = "OD_1")
data_2 <- read_ods("C:/Users/natal/Documents/LIIGH/data_test_bacteria_comsint.ods", sheet = "OD600")

od600_1 <- as.data.frame(data_1)
od600_2 <- as.data.frame(data_2)


i<-0
real_names<-c('Leifsonia sp.','Bacillus sp.','Arthrobacter sp.','Rhodococcus erythropolis','Pseudomonas sp.','Mycobacterium sp. ','Variovorax paradoxus ','Paenibacillus sp.','Agrobacterium sp.','Rhizobium sp.')
bacterias<-c('ST00042','ST00046','ST00060','ST00094','ST00101','ST00109','ST00110','ST00143','ST00154','ST00164')


for (bacteria in bacterias){
  #selecciona las filas con las bcaterias del mismo nombre
  df_bact_1<-od600_1[od600_1$strain==bacteria,]
  df_bact_2<-od600_2[od600_2$strain==bacteria,]
  
  #quita la columna con bact name 
  df_bact_1$strain<-NULL
  df_bact_2$strain<-NULL

  
  #names rows with temp
  rownames(df_bact_1)<-df_bact_1$temp
  rownames(df_bact_2)<-df_bact_2$temp
  
  df_bact_1$temp<-NULL
  df_bact_1$t_1<-NULL
  df_bact_1$t_2<-NULL
  
  df_bact_2$temp<-NULL
  
  
  #trnasposes df
  df_t_1<-as.data.frame(t(df_bact_1))
  df_t_2<-as.data.frame(t(df_bact_2))
  rownames(df_t_1)<-c('t_0','t_1','t_2','t_3')
  rownames(df_t_2)<-c('t_0','t_1.1','t_2.1','t_3.1')
  
  
  df_t_1$timeline<-c(0,24,48,72)
  df_t_2$timeline<-c(0,24,48,72)
  
  
  df_t<-rbind(df_t_1, df_t_2)
  df_t
  
  
  df_melted <- melt(df_t,  id.vars = 'timeline', variable.name = 'temp')
  df_melted$medicion<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
  df_melted
  
  plot_name<-paste('plot_', bacteria)
  i=i+1
  
  #meter a for loop para sacar gráficas individuales 
  temp_plot<-ggplot(df_melted, aes(timeline,value)) +
    geom_point()+
    geom_line(aes(group=medicion, color=temp))+ 
    ggtitle(real_names[i])+
    xlab('Total time (h)') + ylab('OD 600')+
    labs(colour ='Temperature (°C)')+
    theme(plot.title = element_text(hjust = 0.5,face='italic'))+
    ylim(0,0.9)+
    scale_x_continuous(breaks=c(0,24,48,72))
  assign(plot_name, temp_plot)

}



#arrange all plots in one window, with shared legened
plot_1<-ggarrange(
  `plot_ ST00143`,`plot_ ST00046`,`plot_ ST00164`,`plot_ ST00154`, # list of plots
  labels = NULL, # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 2, # number of rows
  ncol=2
)

plot_2<-ggarrange(
  `plot_ ST00110`,`plot_ ST00101`,`plot_ ST00109`,`plot_ ST00094`, # list of plots
  labels = NULL, # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 2, # number of rows
  ncol=2
)

plot_3<-ggarrange(
  `plot_ ST00060`,`plot_ ST00042`, # list of plots
  labels = NULL, # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 1, # number of rows
  ncol=2
)

ggsave(plot_1, file="plot_1.png", width = 14, height = 14, units = "cm")
ggsave(plot_2, file="plot_2.png", width = 14, height = 14, units = "cm")
ggsave(plot_3, file="plot_3.png", width = 14, height = 9, units = "cm")




