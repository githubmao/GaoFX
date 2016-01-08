# 编辑GaoFX的Shiny app Server

library(shiny)
library(ggplot2)

# 导入用LandXML包解析后的CrossSects数据
CrossSect <- read.table("data/SZhighway_CrossSects.csv", header=T,sep=",",stringsAsFactors =F)

# 导入计算后的评价数据
Speed <- read.table("data/Speed.csv", header=T,sep=",",stringsAsFactors =F)
Speedx <- read.table("data/Speedx.csv", header=T,sep=",",stringsAsFactors =F)
Acc_surge <- read.table("data/Acc_surge.csv", header=T,sep=",",stringsAsFactors =F)
Acc_sway <- read.table("data/Acc_sway.csv", header=T,sep=",",stringsAsFactors =F)
Brake_pedal <- read.table("data/Brake_pedal.csv", header=T,sep=",",stringsAsFactors =F)
BrakeCR <- read.table("data/BrakeCR.csv", header=T,sep=",",stringsAsFactors =F)
Steering <- read.table("data/Steering.csv", header=T,sep=",",stringsAsFactors =F)
SteeringCR <- read.table("data/SteeringCR.csv", header=T,sep=",",stringsAsFactors =F)

# Speed相关图
PlotSpeed85 <- ggplot(data = Speed, aes(x = factor(Position), y = Speed_85))+
                 geom_boxplot(fill = "grey")+labs(x="Position", y="Speed")+
                 geom_hline(yintercept = 1.00, colour = "blue", size = 1.2)                        # Speed_85 图

# Speedx相关图
PlotSpeedx85 <- ggplot(data = Speedx, aes(x = factor(Position), y = Speed_x_85))+
                  geom_boxplot(fill = "grey")+labs(x="Position", y="Speed_x")                      # Speed_x_85 图

# Acc_surge相关图
PlotAccsurgeMax <- ggplot(data = Acc_surge, aes(x = factor(Position), y = Acc_surge_max))+
                     geom_boxplot(fill = "grey")+labs(x="Position", y="Acc_surge")                 # Acc_surge_max 图

# Acc-sway相关图
PlotAccswayMax <- ggplot(data = Acc_sway, aes(x = factor(Position), y = Acc_sway_max))+
                    geom_boxplot(fill = "grey")+labs(x="Position", y="Acc_sway")                   # Acc_sway_max 图

# Brake_pedal相关图
PlotBrakepedalMax <- ggplot(data = Brake_pedal, aes(x = factor(Position), y = Brake_pedal_max))+
                       geom_boxplot(fill = "grey")+labs(x="Position", y="Brake_pedal")             # Brake_pedal_max 图

# BrakeCR相关图
PlotBrakeCRMax <- ggplot(data = BrakeCR, aes(x = factor(Position), y = BrakeCR_max))+
                    geom_boxplot(fill = "grey")+labs(x="Position", y="Brake Change Rate")          # BrakeCR_max 图

# Steering相关图
PlotSteeringMax <- ggplot(data = Steering, aes(x = factor(Position), y = Steering_max))+
                     geom_boxplot(fill = "grey")+labs(x="Position", y="Steering")                  # Steering_max 图

# SteeringCR相关图
PlotSteeringCRMax <- ggplot(data = SteeringCR, aes(x = factor(Position), y = SteeringCR_max))+
                       geom_boxplot(fill = "grey")+labs(x="Position", y="Steering Change Rate")    # SteeringCR_max 图

shinyServer(function(input, output) {
  
  output$IndexPlot <- renderPlot({
    finalPlot <- switch (input$Index,
                         "Speed" = PlotSpeed85,
                         "Acc_surge" = PlotAccsurgeMax,
                         "Speedx" = PlotSpeedx85,
                         "Acc_sway" = PlotAccswayMax,
                         "Brake_pedal" = PlotBrakepedalMax,
                         "BrakeCR" = PlotBrakeCRMax,
                         "Steering" = PlotSteeringMax,
                         "SteeringCR" = PlotSteeringCRMax)
    
    finalPlot
  })
})