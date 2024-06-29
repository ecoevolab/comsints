#grafica de comsint experimentos
#eje x: días (d0,d1,d2) o con timepoints 
#eje y: OD600

library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(ggpubr)

#cambiar dependiendo el archivo 
data_1 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS1.ods", sheet = "OD600_real")
data_2 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS2.ods", sheet = "OD600_real")
data_3 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS3.ods", sheet = "OD600_real")

od600_1 <- as.data.frame(data_1)
od600_2 <- as.data.frame(data_2)
od600_3 <- as.data.frame(data_3)

od600_1<-rbind(od600_1,od600_3)

comsints_1<-c('R1','R2','R3','R4','R5','R6')
for (comsint in comsints_1){

  #selecciona las filas con las bcaterias del mismo nombre
  df_bact_1<-od600_1[od600_1$Community==comsint,]
  
  #quita la columna con bact name y rename colnames 
  df_bact_1$Community<-NULL
  colnames(df_bact_1)<-c('temp','0','24','48','72')
  
  #adds medicion para poder juntar los 28 y los 32 de un solo color
  df_bact_1$medicion<-c(1,2,3,4)
  
  #meltea la data en tabla dependiendo de la medicion y temp 
  melt_data <- melt(df_bact_1, id = c('medicion', 'temp'), variable.name = 'timeline')
  
  #plots
  plot_name<-paste('plot_', comsint)
  melt_data$temp<-as.factor(melt_data$temp)
  
  temp_plot<-ggplot(melt_data, aes(timeline,value)) +
    geom_point()+
    scale_color_manual(values = c( "#3333FF", "#FF6600"))+
    geom_line(
      aes(group=medicion, colour=temp))+ 
    ggtitle(comsint)+
    xlab('Total time (h)') + ylab('OD 600')+
    labs(colour ='Temperature (°C)')+
    theme(plot.title = element_text(hjust = 0.5))+
    ylim(0,1.5)
    
  assign(plot_name, temp_plot)
}


comsints_2<-c('R7','R8','R9','R10','R11','R12')
for (comsint in comsints_2){
  #selecciona las filas con las bcaterias del mismo nombre
  df_bact_2<-od600_2[od600_2$Community==comsint,]
  
  #quita la columna con bact name 
  df_bact_2$Community<-NULL
  colnames(df_bact_2)<-c('temp','0','24','48','72')
  
  df_bact_2$medicion<-c(1,2)
  
  melt_data <- melt(df_bact_2, id = c('medicion', 'temp'), variable.name = 'timeline')
  plot_name<-paste('plot_', comsint)
  melt_data$temp<-as.factor(melt_data$temp)
  
  temp_plot<-ggplot(melt_data, aes(timeline,value)) +
    geom_point()+
    scale_color_manual(values = c( "#3333FF", "#FF6600"))+
    geom_line(
      aes(group=medicion, colour=temp))+ 
    ggtitle(comsint)+
    xlab('Total time (h)') + ylab('OD 600')+
    labs(colour ='Temperature (°C)')+
    theme(plot.title = element_text(hjust = 0.5))+
    ylim(0,1.5)
  
  temp_plot
  assign(plot_name, temp_plot)

}


#arrange all plots in one window, with shared legened
plot_1<-ggarrange(
  `plot_ R1`,`plot_ R2`,`plot_ R3`,`plot_ R4`, `plot_ R5`, `plot_ R6`, # list of plots
  labels = NULL, # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 3, # number of rows
  ncol=2
)


plot_2<-ggarrange(
  `plot_ R7`,`plot_ R8`,`plot_ R9`,`plot_ R10`, `plot_ R11`, `plot_ R12`, # list of plots
  labels = NULL, # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 3, # number of rows
  ncol=2
)


plot_1
plot_2

ggsave(plot_1, file="NS1_plot.png", width = 14, height = 14, units = "cm")
ggsave(plot_2, file="NS2_plot.png", width = 14, height = 14, units = "cm")


