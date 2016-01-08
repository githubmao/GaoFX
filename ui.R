# 编辑GaoFX的Shiny app UI

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("高风险道路环境驾驶行为谱"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      
      h4("这是一个高风险道路环境驾驶行为谱的草案。"),
      
      selectInput("HighRiskENV", label = h4("高风险道路环境"),
                  choices = list("隧道入口" = 1,
                                 "隧道出口" = 2,
                                 "互通入口" = 3,
                                 "互通出口" = 4),
                  selected = 1),
      
      radioButtons("Index", label = h4("驾驶行为指标"),
                   choices = list("纵向速度" = "Speed",
                                  "纵向加速度" = "Acc_surge",
                                  "横向速度" = "Speedx",
                                  "横向加速度" = "Acc_sway",
                                  "刹车踏板踩踏深度" = "Brake_pedal",
                                  "刹车踏板踩踏深度变化率" = "BrakeCR",
                                  "方向盘转角" = "Steering",
                                  "方向盘转角变化率" = "SteeringCR"),
                   selected = "Speed")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h3("驾驶行为谱系图"),
      
      plotOutput("IndexPlot")
    )
  )
))
