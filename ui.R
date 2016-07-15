
# “高风险道路道路环境驾驶行为谱系研究”谱系查询工具
#
# 说明：本脚本文件为谱系查询工具的UI面板，含以下功能键

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("高风险道路环境驾驶行为谱系查询工具"),
  
  # 查询工具控制区
  sidebarLayout(
    sidebarPanel(
      
      # 高风险道路环境选取
      selectInput("RiskEnv", label = h3("选择需查询的高风险道路环境"),
                  choices = list("隧道入口" = "TunnelEnt",
                                 "隧道出口" = "TunnelExt",
                                 "八车道互通环境入口匝道" = "8CrossEnt",
                                 "八车道互通环境互通主线" = "8CrossMln",
                                 "八车道互通环境出口匝道" = "8CrossExt",
                                 "四车道互通环境入口匝道" = "4CrossEnt",
                                 "四车道互通环境互通主线" = "4CrossMln",
                                 "四车道互通环境出口匝道" = "4CrossExt"),
                  selected = "TunnelEnt"),
      
      br(),
      
      # 驾驶行为指标选取
      radioButtons("Index", label = h3("选择需查询的驾驶行为指标"),
                   choices = list("纵向最大速度" = "MaxLongitudinalSpeed",
                                  "纵向85分位速度" = "P85LongitudinalSpeed",
                                  "纵向中位速度" = "MedianLongitudinalSpeed",
                                  "纵向平均速度" = "AvgLongitudinalSpeed",
                                  "纵向最小速度" = "MinLongitudinalSpeed",
                                  "纵向最大加速度" = "MaxLongitudinalAcc",
                                  "纵向最小加速度" = "MinLongitudinalAcc",
                                  "纵向最大加速度变化率" = "LongitudinalAccRate",
                                  "加速踏板最大踩踏深度" = "AccPedal",
                                  "加速踏板最大踩踏深度变化率" = "AccPedalRate",
                                  "刹车踏板最大踩踏深度" = "BrakePedal",
                                  "刹车踏板最大踩踏深度变化率" = "BrakePedalRate",
                                  "超速比例" = "SpeedingRate",
                                  "横向最大速度" = "MaxLateralSpeed",
                                  "横向85分位速度" = "P85LateralSpeed",
                                  "横向中位速度" = "MedianLateralSpeed",
                                  "横向平均速度" = "AvgLateralSpeed",
                                  "横向最小速度" = "MinLateralSpeed",
                                  "横向最大加速度" = "MaxLateralAcc",
                                  "横向最小加速度" = "MinLateralAcc",
                                  "横向最大加速度变化率" = "LateralAccRate",
                                  "平均转向值" = "Steering",
                                  "平均转向变化率" = "SteeringRate",
                                  "车道平均距离" = "Laneoffset",
                                  "TLC中位值" = "TLC",
                                  "TLC小于1s的比例" = "TLCRate"),
                   selected = "MaxLongitudinalSpeed"),
      
      br(),
      
      # 特征点位置选择
      sliderInput("DisPosition", label = h3("选择距离特征点位置"),
                  min = -195, 
                  max = 195,
                  value = -195,
                  step = 10),
      
      br(),
      br(),
      
      p("Copyright 2016 RIOH | All Rights Reserved", align = "center")
      
    ),
    
    # 绘图区
    mainPanel(
      
      h3("驾驶行为谱系图"),
      
      plotOutput("IndexPlot"),
      
      br(),
      br(),
      
      h3("查询位置驾驶行为指标值"),
      
      tableOutput("IndexTable")
      
    )
  )
))
