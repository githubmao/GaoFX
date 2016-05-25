
# “高风险道路道路环境驾驶行为谱系研究”谱系查询工具
#
# 说明：本脚本文件为谱系查询工具的Sever

library(shiny)
library(ggplot2)
library(data.table)
library(reshape2)

# 导入数据 ---------------------------------------------------------------------------
get.filename <- list.files(path = "data",
                           pattern = ".csv")  # 获取工作目录.csv数据文件全名
get.dfname <- gsub(".csv", "", get.filename)  # 获取导入的数据框名称

for (i in 1:length(get.filename)) {
  get.tmpdata <- fread(input = paste("data", get.filename[i], sep = "/"))  # 导入数据
  assign(get.dfname[i], get.tmpdata)  # 命名导入的数据框
}


# 相关图 -----------------------------------------------------------------------------

# 隧道入口 ----
# 纵向最大速度
MaxLongitudinalSpeedPlot.TunnelEnt <- 
  ggplot(data = LFMaxLongitudinalSpeed.TunnelEnt,
         aes(x = position.disgap, y = MaxLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向85分位速度
P85LongitudinalSpeedPlot.TunnelEnt <-
  ggplot(data = LFP85LongitudinalSpeed.TunnelEnt,
         aes(x = position.disgap, y = P85LongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向中位速度
MedianLongitudinalSpeedPlot.TunnelEnt <- 
  ggplot(data = LFMedianLongitudinalSpeed.TunnelEnt,
         aes(x = position.disgap, y = MedianLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向平均速度
AvgLongitudinalSpeedPlot.TunnelEnt <- 
  ggplot(data = LFAvgLongitudinalSpeed.TunnelEnt,
         aes(x = position.disgap, y = AvgLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小速度
MinLongitudinalSpeedPlot.TunnelEnt <- 
  ggplot(data = LFMinLongitudinalSpeed.TunnelEnt,
         aes(x = position.disgap, y = MinLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度
MaxLongitudinalAccPlot.TunnelEnt <- 
  ggplot(data = LFMaxLongitudinalAcc.TunnelEnt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小加速度
MinLongitudinalAccPlot.TunnelEnt <- 
  ggplot(data = LFMinLongitudinalAcc.TunnelEnt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度变化率
LongitudinalAccRatePlot.TunnelEnt <- 
  ggplot(data = LFLongitudinalAccRate.TunnelEnt,
         aes(x = position.disgap, y = LongitudinalAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Longitudinal Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度
AccPedalPlot.TunnelEnt <- 
  ggplot(data = LFAccPedal.TunnelEnt,
         aes(x = position.disgap, y = AccPedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度变化率
AccPedalRatePlot.TunnelEnt <- 
  ggplot(data = LFAccPedalRate.TunnelEnt,
         aes(x = position.disgap, y = AccPedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度
BrakePedalPlot.TunnelEnt <- 
  ggplot(data = LFBrakePedal.TunnelEnt,
         aes(x = position.disgap, y = BrakePedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度变化率
BrakePedalRatePlot.TunnelEnt <- 
  ggplot(data = LFBrakePedalRate.TunnelEnt,
         aes(x = position.disgap, y = BrakePedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 超速比例
SpeedingRatePlot.TunnelEnt <- 
  ggplot(data = LFSpeedingRate.TunnelEnt,
         aes(x = position.disgap, y = SpeedingRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Speeding Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大速度
MaxLateralSpeedPlot.TunnelEnt <- 
  ggplot(data = LFMaxLateralSpeed.TunnelEnt,
         aes(x = position.disgap, y = MaxLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向85分位速度
P85LateralSpeedPlot.TunnelEnt <- 
  ggplot(data = LFP85LateralSpeed.TunnelEnt,
         aes(x = position.disgap, y = P85LateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向中位速度
MedianLateralSpeedPlot.TunnelEnt <- 
  ggplot(data = LFMedianLateralSpeed.TunnelEnt,
         aes(x = position.disgap, y = MedianLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向平均速度
AvgLateralSpeedPlot.TunnelEnt <- 
  ggplot(data = LFAvgLateralSpeed.TunnelEnt,
         aes(x = position.disgap, y = AvgLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小速度
MinLateralSpeedPlot.TunnelEnt <- 
  ggplot(data = LFMinLateralSpeed.TunnelEnt,
         aes(x = position.disgap, y = MinLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度
MaxLateralAccPlot.TunnelEnt <- 
  ggplot(data = LFMaxLateralAcc.TunnelEnt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小加速度
MinLateralAccPlot.TunnelEnt <- 
  ggplot(data = LFMinLateralAcc.TunnelEnt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度变化率
LateralAccRatePlot.TunnelEnt <- 
  ggplot(data = LFLateralAccRate.TunnelEnt,
         aes(x = position.disgap, y = LateralAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Lateral Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向值
SteeringPlot.TunnelEnt <- 
  ggplot(data = LFSteering.TunnelEnt,
         aes(x = position.disgap, y = Steering, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向变化率
SteeringRatePlot.TunnelEnt <- 
  ggplot(data = LFSteeringRate.TunnelEnt,
         aes(x = position.disgap, y = SteeringRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 车道平均距离
LaneoffsetPlot.TunnelEnt <- 
  ggplot(data = LFLaneoffset.TunnelEnt,
         aes(x = position.disgap, y = Laneoffset, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Laneoffset, m")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC中位值
TLCPlot.TunnelEnt <- 
  ggplot(data = LFTLC.TunnelEnt,
         aes(x = position.disgap, y = TLC, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC, s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC小于1s的比例
TLCRatePlot.TunnelEnt <- 
  ggplot(data = LFTLCRate.TunnelEnt,
         aes(x = position.disgap, y = TLCRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))


# 隧道出口 ----
# 纵向最大速度
MaxLongitudinalSpeedPlot.TunnelExt <- 
  ggplot(data = LFMaxLongitudinalSpeed.TunnelExt,
         aes(x = position.disgap, y = MaxLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向85分位速度
P85LongitudinalSpeedPlot.TunnelExt <- 
  ggplot(data = LFP85LongitudinalSpeed.TunnelExt,
         aes(x = position.disgap, y = P85LongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向中位速度
MedianLongitudinalSpeedPlot.TunnelExt <- 
  ggplot(data = LFMedianLongitudinalSpeed.TunnelExt,
         aes(x = position.disgap, y = MedianLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向平均速度
AvgLongitudinalSpeedPlot.TunnelExt <- 
  ggplot(data = LFAvgLongitudinalSpeed.TunnelExt,
         aes(x = position.disgap, y = AvgLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小速度
MinLongitudinalSpeedPlot.TunnelExt <- 
  ggplot(data = LFMinLongitudinalSpeed.TunnelExt,
         aes(x = position.disgap, y = MinLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度
MaxLongitudinalAccPlot.TunnelExt <- 
  ggplot(data = LFMaxLongitudinalAcc.TunnelExt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小加速度
MinLongitudinalAccPlot.TunnelExt <- 
  ggplot(data = LFMinLongitudinalAcc.TunnelExt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度变化率
LongitudinalAccRatePlot.TunnelExt <- 
  ggplot(data = LFLongitudinalAccRate.TunnelExt,
         aes(x = position.disgap, y = LongitudinalAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Longitudinal Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度
AccPedalPlot.TunnelExt <- 
  ggplot(data = LFAccPedal.TunnelExt,
         aes(x = position.disgap, y = AccPedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度变化率
AccPedalRatePlot.TunnelExt <- 
  ggplot(data = LFAccPedalRate.TunnelExt,
         aes(x = position.disgap, y = AccPedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度
BrakePedalPlot.TunnelExt <- 
  ggplot(data = LFBrakePedal.TunnelExt,
         aes(x = position.disgap, y = BrakePedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度变化率
BrakePedalRatePlot.TunnelExt <- 
  ggplot(data = LFBrakePedalRate.TunnelExt,
         aes(x = position.disgap, y = BrakePedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 超速比例
SpeedingRatePlot.TunnelExt <- 
  ggplot(data = LFSpeedingRate.TunnelExt,
         aes(x = position.disgap, y = SpeedingRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Speeding Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大速度
MaxLateralSpeedPlot.TunnelExt <- 
  ggplot(data = LFMaxLateralSpeed.TunnelExt,
         aes(x = position.disgap, y = MaxLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向85分位速度
P85LateralSpeedPlot.TunnelExt <- 
  ggplot(data = LFP85LateralSpeed.TunnelExt,
         aes(x = position.disgap, y = P85LateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向中位速度
MedianLateralSpeedPlot.TunnelExt <- 
  ggplot(data = LFMedianLateralSpeed.TunnelExt,
         aes(x = position.disgap, y = MedianLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向平均速度
AvgLateralSpeedPlot.TunnelExt <- 
  ggplot(data = LFAvgLateralSpeed.TunnelExt,
         aes(x = position.disgap, y = AvgLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小速度
MinLateralSpeedPlot.TunnelExt <- 
  ggplot(data = LFMinLateralSpeed.TunnelExt,
         aes(x = position.disgap, y = MinLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度
MaxLateralAccPlot.TunnelExt <- 
  ggplot(data = LFMaxLateralAcc.TunnelExt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小加速度
MinLateralAccPlot.TunnelExt <- 
  ggplot(data = LFMinLateralAcc.TunnelExt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度变化率
LateralAccRatePlot.TunnelExt <- 
  ggplot(data = LFLateralAccRate.TunnelExt,
         aes(x = position.disgap, y = LateralAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Lateral Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向值
SteeringPlot.TunnelExt <- 
  ggplot(data = LFSteering.TunnelExt,
         aes(x = position.disgap, y = Steering, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向变化率
SteeringRatePlot.TunnelExt <- 
  ggplot(data = LFSteeringRate.TunnelExt,
         aes(x = position.disgap, y = SteeringRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 车道平均距离
LaneoffsetPlot.TunnelExt <- 
  ggplot(data = LFLaneoffset.TunnelExt,
         aes(x = position.disgap, y = Laneoffset, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Laneoffset, m")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC中位值
TLCPlot.TunnelExt <- 
  ggplot(data = LFTLC.TunnelExt,
         aes(x = position.disgap, y = TLC, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC, s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC小于1s的比例
TLCRatePlot.TunnelExt <- 
  ggplot(data = LFTLCRate.TunnelExt,
         aes(x = position.disgap, y = TLCRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))


# 八车道互通环境入口匝道 ----
# 纵向最大速度
MaxLongitudinalSpeedPlot.8CrossEnt <- 
  ggplot(data = LFMaxLongitudinalSpeed.8CrossEnt,
         aes(x = position.disgap, y = MaxLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向85分位速度
P85LongitudinalSpeedPlot.8CrossEnt <- 
  ggplot(data = LFP85LongitudinalSpeed.8CrossEnt,
         aes(x = position.disgap, y = P85LongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向中位速度
MedianLongitudinalSpeedPlot.8CrossEnt <- 
  ggplot(data = LFMedianLongitudinalSpeed.8CrossEnt,
         aes(x = position.disgap, y = MedianLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向平均速度
AvgLongitudinalSpeedPlot.8CrossEnt <- 
  ggplot(data = LFAvgLongitudinalSpeed.8CrossEnt,
         aes(x = position.disgap, y = AvgLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小速度
MinLongitudinalSpeedPlot.8CrossEnt <- 
  ggplot(data = LFMinLongitudinalSpeed.8CrossEnt,
         aes(x = position.disgap, y = MinLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度
MaxLongitudinalAccPlot.8CrossEnt <- 
  ggplot(data = LFMaxLongitudinalAcc.8CrossEnt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小加速度
MinLongitudinalAccPlot.8CrossEnt <- 
  ggplot(data = LFMinLongitudinalAcc.8CrossEnt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度变化率
LongitudinalAccRatePlot.8CrossEnt <- 
  ggplot(data = LFLongitudinalAccRate.8CrossEnt,
         aes(x = position.disgap, y = LongitudinalAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Longitudinal Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度
AccPedalPlot.8CrossEnt <- 
  ggplot(data = LFAccPedal.8CrossEnt,
         aes(x = position.disgap, y = AccPedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度变化率
AccPedalRatePlot.8CrossEnt <- 
  ggplot(data = LFAccPedalRate.8CrossEnt,
         aes(x = position.disgap, y = AccPedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度
BrakePedalPlot.8CrossEnt <- 
  ggplot(data = LFBrakePedal.8CrossEnt,
         aes(x = position.disgap, y = BrakePedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度变化率
BrakePedalRatePlot.8CrossEnt <- 
  ggplot(data = LFBrakePedalRate.8CrossEnt,
         aes(x = position.disgap, y = BrakePedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 超速比例
SpeedingRatePlot.8CrossEnt <- 
  ggplot(data = LFSpeedingRate.8CrossEnt,
         aes(x = position.disgap, y = SpeedingRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Speeding Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大速度
MaxLateralSpeedPlot.8CrossEnt <- 
  ggplot(data = LFMaxLateralSpeed.8CrossEnt,
         aes(x = position.disgap, y = MaxLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向85分位速度
P85LateralSpeedPlot.8CrossEnt <- 
  ggplot(data = LFP85LateralSpeed.8CrossEnt,
         aes(x = position.disgap, y = P85LateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向中位速度
MedianLateralSpeedPlot.8CrossEnt <- 
  ggplot(data = LFMedianLateralSpeed.8CrossEnt,
         aes(x = position.disgap, y = MedianLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向平均速度
AvgLateralSpeedPlot.8CrossEnt <- 
  ggplot(data = LFAvgLateralSpeed.8CrossEnt,
         aes(x = position.disgap, y = AvgLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小速度
MinLateralSpeedPlot.8CrossEnt <- 
  ggplot(data = LFMinLateralSpeed.8CrossEnt,
         aes(x = position.disgap, y = MinLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度
MaxLateralAccPlot.8CrossEnt <- 
  ggplot(data = LFMaxLateralAcc.8CrossEnt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小加速度
MinLateralAccPlot.8CrossEnt <- 
  ggplot(data = LFMinLateralAcc.8CrossEnt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度变化率
LateralAccRatePlot.8CrossEnt <- 
  ggplot(data = LFLateralAccRate.8CrossEnt,
         aes(x = position.disgap, y = LateralAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Lateral Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向值
SteeringPlot.8CrossEnt <- 
  ggplot(data = LFSteering.8CrossEnt,
         aes(x = position.disgap, y = Steering, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向变化率
SteeringRatePlot.8CrossEnt <- 
  ggplot(data = LFSteeringRate.8CrossEnt,
         aes(x = position.disgap, y = SteeringRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 车道平均距离
LaneoffsetPlot.8CrossEnt <- 
  ggplot(data = LFLaneoffset.8CrossEnt,
         aes(x = position.disgap, y = Laneoffset, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Laneoffset, m")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC中位值
TLCPlot.8CrossEnt <- 
  ggplot(data = LFTLC.8CrossEnt,
         aes(x = position.disgap, y = TLC, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC, s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC小于1s的比例
TLCRatePlot.8CrossEnt <- 
  ggplot(data = LFTLCRate.8CrossEnt,
         aes(x = position.disgap, y = TLCRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))


# 八车道互通环境互通主线 ----
# 纵向最大速度
MaxLongitudinalSpeedPlot.8CrossMln <- 
  ggplot(data = LFMaxLongitudinalSpeed.8CrossMln,
         aes(x = position.disgap, y = MaxLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向85分位速度
P85LongitudinalSpeedPlot.8CrossMln <- 
  ggplot(data = LFP85LongitudinalSpeed.8CrossMln,
         aes(x = position.disgap, y = P85LongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向中位速度
MedianLongitudinalSpeedPlot.8CrossMln <- 
  ggplot(data = LFMedianLongitudinalSpeed.8CrossMln,
         aes(x = position.disgap, y = MedianLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向平均速度
AvgLongitudinalSpeedPlot.8CrossMln <- 
  ggplot(data = LFAvgLongitudinalSpeed.8CrossMln,
         aes(x = position.disgap, y = AvgLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小速度
MinLongitudinalSpeedPlot.8CrossMln <- 
  ggplot(data = LFMinLongitudinalSpeed.8CrossMln,
         aes(x = position.disgap, y = MinLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度
MaxLongitudinalAccPlot.8CrossMln <- 
  ggplot(data = LFMaxLongitudinalAcc.8CrossMln,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小加速度
MinLongitudinalAccPlot.8CrossMln <- 
  ggplot(data = LFMinLongitudinalAcc.8CrossMln,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度变化率
LongitudinalAccRatePlot.8CrossMln <- 
  ggplot(data = LFLongitudinalAccRate.8CrossMln,
         aes(x = position.disgap, y = LongitudinalAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Longitudinal Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度
AccPedalPlot.8CrossMln <- 
  ggplot(data = LFAccPedal.8CrossMln,
         aes(x = position.disgap, y = AccPedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度变化率
AccPedalRatePlot.8CrossMln <- 
  ggplot(data = LFAccPedalRate.8CrossMln,
         aes(x = position.disgap, y = AccPedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度
BrakePedalPlot.8CrossMln <- 
  ggplot(data = LFBrakePedal.8CrossMln,
         aes(x = position.disgap, y = BrakePedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度变化率
BrakePedalRatePlot.8CrossMln <- 
  ggplot(data = LFBrakePedalRate.8CrossMln,
         aes(x = position.disgap, y = BrakePedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 超速比例
SpeedingRatePlot.8CrossMln <- 
  ggplot(data = LFSpeedingRate.8CrossMln,
         aes(x = position.disgap, y = SpeedingRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Speeding Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大速度
MaxLateralSpeedPlot.8CrossMln <- 
  ggplot(data = LFMaxLateralSpeed.8CrossMln,
         aes(x = position.disgap, y = MaxLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向85分位速度
P85LateralSpeedPlot.8CrossMln <- 
  ggplot(data = LFP85LateralSpeed.8CrossMln,
         aes(x = position.disgap, y = P85LateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向中位速度
MedianLateralSpeedPlot.8CrossMln <- 
  ggplot(data = LFMedianLateralSpeed.8CrossMln,
         aes(x = position.disgap, y = MedianLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向平均速度
AvgLateralSpeedPlot.8CrossMln <- 
  ggplot(data = LFAvgLateralSpeed.8CrossMln,
         aes(x = position.disgap, y = AvgLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小速度
MinLateralSpeedPlot.8CrossMln <- 
  ggplot(data = LFMinLateralSpeed.8CrossMln,
         aes(x = position.disgap, y = MinLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度
MaxLateralAccPlot.8CrossMln <- 
  ggplot(data = LFMaxLateralAcc.8CrossMln,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小加速度
MinLateralAccPlot.8CrossMln <- 
  ggplot(data = LFMinLateralAcc.8CrossMln,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度变化率
LateralAccRatePlot.8CrossMln <- 
  ggplot(data = LFLateralAccRate.8CrossMln,
         aes(x = position.disgap, y = LateralAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Lateral Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向值
SteeringPlot.8CrossMln <- 
  ggplot(data = LFSteering.8CrossMln,
         aes(x = position.disgap, y = Steering, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向变化率
SteeringRatePlot.8CrossMln <- 
  ggplot(data = LFSteeringRate.8CrossMln,
         aes(x = position.disgap, y = SteeringRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 车道平均距离
LaneoffsetPlot.8CrossMln <- 
  ggplot(data = LFLaneoffset.8CrossMln,
         aes(x = position.disgap, y = Laneoffset, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Laneoffset, m")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC中位值
TLCPlot.8CrossMln <- 
  ggplot(data = LFTLC.8CrossMln,
         aes(x = position.disgap, y = TLC, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC, s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC小于1s的比例
TLCRatePlot.8CrossMln <- 
  ggplot(data = LFTLCRate.8CrossMln,
         aes(x = position.disgap, y = TLCRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))


# 八车道互通环境出口匝道 ----
# 纵向最大速度
MaxLongitudinalSpeedPlot.8CrossExt <- 
  ggplot(data = LFMaxLongitudinalSpeed.8CrossExt,
         aes(x = position.disgap, y = MaxLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向85分位速度
P85LongitudinalSpeedPlot.8CrossExt <- 
  ggplot(data = LFP85LongitudinalSpeed.8CrossExt,
         aes(x = position.disgap, y = P85LongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向中位速度
MedianLongitudinalSpeedPlot.8CrossExt <- 
  ggplot(data = LFMedianLongitudinalSpeed.8CrossExt,
         aes(x = position.disgap, y = MedianLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向平均速度
AvgLongitudinalSpeedPlot.8CrossExt <- 
  ggplot(data = LFAvgLongitudinalSpeed.8CrossExt,
         aes(x = position.disgap, y = AvgLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小速度
MinLongitudinalSpeedPlot.8CrossExt <- 
  ggplot(data = LFMinLongitudinalSpeed.8CrossExt,
         aes(x = position.disgap, y = MinLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度
MaxLongitudinalAccPlot.8CrossExt <- 
  ggplot(data = LFMaxLongitudinalAcc.8CrossExt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小加速度
MinLongitudinalAccPlot.8CrossExt <- 
  ggplot(data = LFMinLongitudinalAcc.8CrossExt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度变化率
LongitudinalAccRatePlot.8CrossExt <- 
  ggplot(data = LFLongitudinalAccRate.8CrossExt,
         aes(x = position.disgap, y = LongitudinalAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Longitudinal Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度
AccPedalPlot.8CrossExt <- 
  ggplot(data = LFAccPedal.8CrossExt,
         aes(x = position.disgap, y = AccPedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度变化率
AccPedalRatePlot.8CrossExt <- 
  ggplot(data = LFAccPedalRate.8CrossExt,
         aes(x = position.disgap, y = AccPedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度
BrakePedalPlot.8CrossExt <- 
  ggplot(data = LFBrakePedal.8CrossExt,
         aes(x = position.disgap, y = BrakePedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度变化率
BrakePedalRatePlot.8CrossExt <- 
  ggplot(data = LFBrakePedalRate.8CrossExt,
         aes(x = position.disgap, y = BrakePedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 超速比例
SpeedingRatePlot.8CrossExt <- 
  ggplot(data = LFSpeedingRate.8CrossExt,
         aes(x = position.disgap, y = SpeedingRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Speeding Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大速度
MaxLateralSpeedPlot.8CrossExt <- 
  ggplot(data = LFMaxLateralSpeed.8CrossExt,
         aes(x = position.disgap, y = MaxLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向85分位速度
P85LateralSpeedPlot.8CrossExt <- 
  ggplot(data = LFP85LateralSpeed.8CrossExt,
         aes(x = position.disgap, y = P85LateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向中位速度
MedianLateralSpeedPlot.8CrossExt <- 
  ggplot(data = LFMedianLateralSpeed.8CrossExt,
         aes(x = position.disgap, y = MedianLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向平均速度
AvgLateralSpeedPlot.8CrossExt <- 
  ggplot(data = LFAvgLateralSpeed.8CrossExt,
         aes(x = position.disgap, y = AvgLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小速度
MinLateralSpeedPlot.8CrossExt <- 
  ggplot(data = LFMinLateralSpeed.8CrossExt,
         aes(x = position.disgap, y = MinLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度
MaxLateralAccPlot.8CrossExt <- 
  ggplot(data = LFMaxLateralAcc.8CrossExt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小加速度
MinLateralAccPlot.8CrossExt <- 
  ggplot(data = LFMinLateralAcc.8CrossExt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度变化率
LateralAccRatePlot.8CrossExt <- 
  ggplot(data = LFLateralAccRate.8CrossExt,
         aes(x = position.disgap, y = LateralAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Lateral Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向值
SteeringPlot.8CrossExt <- 
  ggplot(data = LFSteering.8CrossExt,
         aes(x = position.disgap, y = Steering, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向变化率
SteeringRatePlot.8CrossExt <- 
  ggplot(data = LFSteeringRate.8CrossExt,
         aes(x = position.disgap, y = SteeringRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 车道平均距离
LaneoffsetPlot.8CrossExt <- 
  ggplot(data = LFLaneoffset.8CrossExt,
         aes(x = position.disgap, y = Laneoffset, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Laneoffset, m")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC中位值
TLCPlot.8CrossExt <- 
  ggplot(data = LFTLC.8CrossExt,
         aes(x = position.disgap, y = TLC, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC, s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC小于1s的比例
TLCRatePlot.8CrossExt <- 
  ggplot(data = LFTLCRate.8CrossExt,
         aes(x = position.disgap, y = TLCRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))


# 四车道互通环境入口匝道 ----
# 纵向最大速度
MaxLongitudinalSpeedPlot.4CrossEnt <- 
  ggplot(data = LFMaxLongitudinalSpeed.4CrossEnt,
         aes(x = position.disgap, y = MaxLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向85分位速度
P85LongitudinalSpeedPlot.4CrossEnt <- 
  ggplot(data = LFP85LongitudinalSpeed.4CrossEnt,
         aes(x = position.disgap, y = P85LongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向中位速度
MedianLongitudinalSpeedPlot.4CrossEnt <- 
  ggplot(data = LFMedianLongitudinalSpeed.4CrossEnt,
         aes(x = position.disgap, y = MedianLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向平均速度
AvgLongitudinalSpeedPlot.4CrossEnt <- 
  ggplot(data = LFAvgLongitudinalSpeed.4CrossEnt,
         aes(x = position.disgap, y = AvgLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小速度
MinLongitudinalSpeedPlot.4CrossEnt <- 
  ggplot(data = LFMinLongitudinalSpeed.4CrossEnt,
         aes(x = position.disgap, y = MinLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度
MaxLongitudinalAccPlot.4CrossEnt <- 
  ggplot(data = LFMaxLongitudinalAcc.4CrossEnt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小加速度
MinLongitudinalAccPlot.4CrossEnt <- 
  ggplot(data = LFMinLongitudinalAcc.4CrossEnt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度变化率
LongitudinalAccRatePlot.4CrossEnt <- 
  ggplot(data = LFLongitudinalAccRate.4CrossEnt,
         aes(x = position.disgap, y = LongitudinalAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Longitudinal Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度
AccPedalPlot.4CrossEnt <- 
  ggplot(data = LFAccPedal.4CrossEnt,
         aes(x = position.disgap, y = AccPedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度变化率
AccPedalRatePlot.4CrossEnt <- 
  ggplot(data = LFAccPedalRate.4CrossEnt,
         aes(x = position.disgap, y = AccPedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度
BrakePedalPlot.4CrossEnt <- 
  ggplot(data = LFBrakePedal.4CrossEnt,
         aes(x = position.disgap, y = BrakePedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度变化率
BrakePedalRatePlot.4CrossEnt <- 
  ggplot(data = LFBrakePedalRate.4CrossEnt,
         aes(x = position.disgap, y = BrakePedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 超速比例
SpeedingRatePlot.4CrossEnt <- 
  ggplot(data = LFSpeedingRate.4CrossEnt,
         aes(x = position.disgap, y = SpeedingRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Speeding Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大速度
MaxLateralSpeedPlot.4CrossEnt <- 
  ggplot(data = LFMaxLateralSpeed.4CrossEnt,
         aes(x = position.disgap, y = MaxLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向85分位速度
P85LateralSpeedPlot.4CrossEnt <- 
  ggplot(data = LFP85LateralSpeed.4CrossEnt,
         aes(x = position.disgap, y = P85LateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向中位速度
MedianLateralSpeedPlot.4CrossEnt <- 
  ggplot(data = LFMedianLateralSpeed.4CrossEnt,
         aes(x = position.disgap, y = MedianLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向平均速度
AvgLateralSpeedPlot.4CrossEnt <- 
  ggplot(data = LFAvgLateralSpeed.4CrossEnt,
         aes(x = position.disgap, y = AvgLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小速度
MinLateralSpeedPlot.4CrossEnt <- 
  ggplot(data = LFMinLateralSpeed.4CrossEnt,
         aes(x = position.disgap, y = MinLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度
MaxLateralAccPlot.4CrossEnt <- 
  ggplot(data = LFMaxLateralAcc.4CrossEnt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小加速度
MinLateralAccPlot.4CrossEnt <- 
  ggplot(data = LFMinLateralAcc.4CrossEnt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度变化率
LateralAccRatePlot.4CrossEnt <- 
  ggplot(data = LFLateralAccRate.4CrossEnt,
         aes(x = position.disgap, y = LateralAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Lateral Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向值
SteeringPlot.4CrossEnt <- 
  ggplot(data = LFSteering.4CrossEnt,
         aes(x = position.disgap, y = Steering, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向变化率
SteeringRatePlot.4CrossEnt <- 
  ggplot(data = LFSteeringRate.4CrossEnt,
         aes(x = position.disgap, y = SteeringRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 车道平均距离
LaneoffsetPlot.4CrossEnt <- 
  ggplot(data = LFLaneoffset.4CrossEnt,
         aes(x = position.disgap, y = Laneoffset, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Laneoffset, m")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC中位值
TLCPlot.4CrossEnt <- 
  ggplot(data = LFTLC.4CrossEnt,
         aes(x = position.disgap, y = TLC, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC, s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC小于1s的比例
TLCRatePlot.4CrossEnt <- 
  ggplot(data = LFTLCRate.4CrossEnt,
         aes(x = position.disgap, y = TLCRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))


# 四车道互通环境互通主线 ----
# 纵向最大速度
MaxLongitudinalSpeedPlot.4CrossMln <- 
  ggplot(data = LFMaxLongitudinalSpeed.4CrossMln,
         aes(x = position.disgap, y = MaxLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向85分位速度
P85LongitudinalSpeedPlot.4CrossMln <- 
  ggplot(data = LFP85LongitudinalSpeed.4CrossMln,
         aes(x = position.disgap, y = P85LongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向中位速度
MedianLongitudinalSpeedPlot.4CrossMln <- 
  ggplot(data = LFMedianLongitudinalSpeed.4CrossMln,
         aes(x = position.disgap, y = MedianLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向平均速度
AvgLongitudinalSpeedPlot.4CrossMln <- 
  ggplot(data = LFAvgLongitudinalSpeed.4CrossMln,
         aes(x = position.disgap, y = AvgLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小速度
MinLongitudinalSpeedPlot.4CrossMln <- 
  ggplot(data = LFMinLongitudinalSpeed.4CrossMln,
         aes(x = position.disgap, y = MinLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度
MaxLongitudinalAccPlot.4CrossMln <- 
  ggplot(data = LFMaxLongitudinalAcc.4CrossMln,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小加速度
MinLongitudinalAccPlot.4CrossMln <- 
  ggplot(data = LFMinLongitudinalAcc.4CrossMln,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度变化率
LongitudinalAccRatePlot.4CrossMln <- 
  ggplot(data = LFLongitudinalAccRate.4CrossMln,
         aes(x = position.disgap, y = LongitudinalAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Longitudinal Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度
AccPedalPlot.4CrossMln <- 
  ggplot(data = LFAccPedal.4CrossMln,
         aes(x = position.disgap, y = AccPedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度变化率
AccPedalRatePlot.4CrossMln <- 
  ggplot(data = LFAccPedalRate.4CrossMln,
         aes(x = position.disgap, y = AccPedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度
BrakePedalPlot.4CrossMln <- 
  ggplot(data = LFBrakePedal.4CrossMln,
         aes(x = position.disgap, y = BrakePedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度变化率
BrakePedalRatePlot.4CrossMln <- 
  ggplot(data = LFBrakePedalRate.4CrossMln,
         aes(x = position.disgap, y = BrakePedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 超速比例
SpeedingRatePlot.4CrossMln <- 
  ggplot(data = LFSpeedingRate.4CrossMln,
         aes(x = position.disgap, y = SpeedingRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Speeding Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大速度
MaxLateralSpeedPlot.4CrossMln <- 
  ggplot(data = LFMaxLateralSpeed.4CrossMln,
         aes(x = position.disgap, y = MaxLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向85分位速度
P85LateralSpeedPlot.4CrossMln <- 
  ggplot(data = LFP85LateralSpeed.4CrossMln,
         aes(x = position.disgap, y = P85LateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向中位速度
MedianLateralSpeedPlot.4CrossMln <- 
  ggplot(data = LFMedianLateralSpeed.4CrossMln,
         aes(x = position.disgap, y = MedianLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向平均速度
AvgLateralSpeedPlot.4CrossMln <- 
  ggplot(data = LFAvgLateralSpeed.4CrossMln,
         aes(x = position.disgap, y = AvgLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小速度
MinLateralSpeedPlot.4CrossMln <- 
  ggplot(data = LFMinLateralSpeed.4CrossMln,
         aes(x = position.disgap, y = MinLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度
MaxLateralAccPlot.4CrossMln <- 
  ggplot(data = LFMaxLateralAcc.4CrossMln,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小加速度
MinLateralAccPlot.4CrossMln <- 
  ggplot(data = LFMinLateralAcc.4CrossMln,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度变化率
LateralAccRatePlot.4CrossMln <- 
  ggplot(data = LFLateralAccRate.4CrossMln,
         aes(x = position.disgap, y = LateralAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Lateral Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向值
SteeringPlot.4CrossMln <- 
  ggplot(data = LFSteering.4CrossMln,
         aes(x = position.disgap, y = Steering, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向变化率
SteeringRatePlot.4CrossMln <- 
  ggplot(data = LFSteeringRate.4CrossMln,
         aes(x = position.disgap, y = SteeringRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 车道平均距离
LaneoffsetPlot.4CrossMln <- 
  ggplot(data = LFLaneoffset.4CrossMln,
         aes(x = position.disgap, y = Laneoffset, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Laneoffset, m")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC中位值
TLCPlot.4CrossMln <- 
  ggplot(data = LFTLC.4CrossMln,
         aes(x = position.disgap, y = TLC, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC, s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC小于1s的比例
TLCRatePlot.4CrossMln <- 
  ggplot(data = LFTLCRate.4CrossMln,
         aes(x = position.disgap, y = TLCRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))


# 四车道互通环境出口匝道 ----
# 纵向最大速度
MaxLongitudinalSpeedPlot.4CrossExt <- 
  ggplot(data = LFMaxLongitudinalSpeed.4CrossExt,
         aes(x = position.disgap, y = MaxLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向85分位速度
P85LongitudinalSpeedPlot.4CrossExt <- 
  ggplot(data = LFP85LongitudinalSpeed.4CrossExt,
         aes(x = position.disgap, y = P85LongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向中位速度
MedianLongitudinalSpeedPlot.4CrossExt <- 
  ggplot(data = LFMedianLongitudinalSpeed.4CrossExt,
         aes(x = position.disgap, y = MedianLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向平均速度
AvgLongitudinalSpeedPlot.4CrossExt <- 
  ggplot(data = LFAvgLongitudinalSpeed.4CrossExt,
         aes(x = position.disgap, y = AvgLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小速度
MinLongitudinalSpeedPlot.4CrossExt <- 
  ggplot(data = LFMinLongitudinalSpeed.4CrossExt,
         aes(x = position.disgap, y = MinLongitudinalSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Speed, km/h")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度
MaxLongitudinalAccPlot.4CrossExt <- 
  ggplot(data = LFMaxLongitudinalAcc.4CrossExt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最小加速度
MinLongitudinalAccPlot.4CrossExt <- 
  ggplot(data = LFMinLongitudinalAcc.4CrossExt,
         aes(x = position.disgap, y = LongitudinalAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Longitudinal Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向最大加速度变化率
LongitudinalAccRatePlot.4CrossExt <- 
  ggplot(data = LFLongitudinalAccRate.4CrossExt,
         aes(x = position.disgap, y = LongitudinalAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Longitudinal Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度
AccPedalPlot.4CrossExt <- 
  ggplot(data = LFAccPedal.4CrossExt,
         aes(x = position.disgap, y = AccPedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 纵向加速踏板最大踩踏深度变化率
AccPedalRatePlot.4CrossExt <- 
  ggplot(data = LFAccPedalRate.4CrossExt,
         aes(x = position.disgap, y = AccPedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Acceleration Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度
BrakePedalPlot.4CrossExt <- 
  ggplot(data = LFBrakePedal.4CrossExt,
         aes(x = position.disgap, y = BrakePedal, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 刹车踏板最大踩踏深度变化率
BrakePedalRatePlot.4CrossExt <- 
  ggplot(data = LFBrakePedalRate.4CrossExt,
         aes(x = position.disgap, y = BrakePedalRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Brake Pedal Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 超速比例
SpeedingRatePlot.4CrossExt <- 
  ggplot(data = LFSpeedingRate.4CrossExt,
         aes(x = position.disgap, y = SpeedingRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Speeding Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大速度
MaxLateralSpeedPlot.4CrossExt <- 
  ggplot(data = LFMaxLateralSpeed.4CrossExt,
         aes(x = position.disgap, y = MaxLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向85分位速度
P85LateralSpeedPlot.4CrossExt <- 
  ggplot(data = LFP85LateralSpeed.4CrossExt,
         aes(x = position.disgap, y = P85LateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "P85 Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向中位速度
MedianLateralSpeedPlot.4CrossExt <- 
  ggplot(data = LFMedianLateralSpeed.4CrossExt,
         aes(x = position.disgap, y = MedianLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Median Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向平均速度
AvgLateralSpeedPlot.4CrossExt <- 
  ggplot(data = LFAvgLateralSpeed.4CrossExt,
         aes(x = position.disgap, y = AvgLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Avg Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小速度
MinLateralSpeedPlot.4CrossExt <- 
  ggplot(data = LFMinLateralSpeed.4CrossExt,
         aes(x = position.disgap, y = MinLateralSpeed, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Speed, m/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度
MaxLateralAccPlot.4CrossExt <- 
  ggplot(data = LFMaxLateralAcc.4CrossExt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Max Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最小加速度
MinLateralAccPlot.4CrossExt <- 
  ggplot(data = LFMinLateralAcc.4CrossExt,
         aes(x = position.disgap, y = LateralAcc, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Min Lateral Acceleration, m^2/s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 横向最大加速度变化率
LateralAccRatePlot.4CrossExt <- 
  ggplot(data = LFLateralAccRate.4CrossExt,
         aes(x = position.disgap, y = LateralAccRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Lateral Acceleration Rate, m^2/s^2")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向值
SteeringPlot.4CrossExt <- 
  ggplot(data = LFSteering.4CrossExt,
         aes(x = position.disgap, y = Steering, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 平均转向变化率
SteeringRatePlot.4CrossExt <- 
  ggplot(data = LFSteeringRate.4CrossExt,
         aes(x = position.disgap, y = SteeringRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Steering Rate")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# 车道平均距离
LaneoffsetPlot.4CrossExt <- 
  ggplot(data = LFLaneoffset.4CrossExt,
         aes(x = position.disgap, y = Laneoffset, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  labs(x = "Position, m", y = "Laneoffset, m")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC中位值
TLCPlot.4CrossExt <- 
  ggplot(data = LFTLC.4CrossExt,
         aes(x = position.disgap, y = TLC, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC, s")+
  scale_x_continuous(breaks=seq(-200, 200, 50))

# TLC小于1s的比例
TLCRatePlot.4CrossExt <- 
  ggplot(data = LFTLCRate.4CrossExt,
         aes(x = position.disgap, y = TLCRate, group = Type))+
  geom_line(aes(color = Type), size = 1.2)+
  # geom_hline(yintersept = 1.5, colour = "red", size = 1.2)+
  labs(x = "Position, m", y = "TLC Rate, %")+
  ylim(0, 1)+
  scale_x_continuous(breaks=seq(-200, 200, 50))


# sever主程序 ------------------------------------------------------------------------
shinyServer(function(input, output) {
  
  output$IndexPlot <- renderPlot({
    
    if (input$RiskEnv == "TunnelEnt") {
      
      FinalPlot <- switch (input$Index,
                           "MaxLongitudinalSpeed" = MaxLongitudinalSpeedPlot.TunnelEnt,
                           "P85LongitudinalSpeed" = P85LongitudinalSpeedPlot.TunnelEnt,
                           "MedianLongitudinalSpeed" = MedianLongitudinalSpeedPlot.TunnelEnt,
                           "AvgLongitudinalSpeed" = AvgLongitudinalSpeedPlot.TunnelEnt,
                           "MinLongitudinalSpeed" = MinLongitudinalSpeedPlot.TunnelEnt,
                           "MaxLongitudinalAcc" = MaxLongitudinalAccPlot.TunnelEnt,
                           "MinLongitudinalAcc" = MinLongitudinalAccPlot.TunnelEnt,
                           "LongitudinalAccRate" = LongitudinalAccRatePlot.TunnelEnt,
                           "AccPedal" = AccPedalPlot.TunnelEnt,
                           "AccPedalRate" = AccPedalRatePlot.TunnelEnt,
                           "BrakePedal" = BrakePedalPlot.TunnelEnt,
                           "BrakePedalRate" = BrakePedalRatePlot.TunnelEnt,
                           "SpeedingRate" = SpeedingRatePlot.TunnelEnt,
                           "MaxLateralSpeed" = MaxLateralSpeedPlot.TunnelEnt,
                           "P85LateralSpeed" = P85LateralSpeedPlot.TunnelEnt,
                           "MedianLateralSpeed" = MedianLateralSpeedPlot.TunnelEnt,
                           "AvgLateralSpeed" = AvgLateralSpeedPlot.TunnelEnt,
                           "MinLateralSpeed" = MinLateralSpeedPlot.TunnelEnt,
                           "MaxLateralAcc" = MaxLateralAccPlot.TunnelEnt,
                           "MinLateralAcc" = MinLateralAccPlot.TunnelEnt,
                           "LateralAccRate" = LateralAccRatePlot.TunnelEnt,
                           "Steering" = SteeringPlot.TunnelEnt,
                           "SteeringRate" = SteeringRatePlot.TunnelEnt,
                           "Laneoffset" = LaneoffsetPlot.TunnelEnt,
                           "TLC" = TLCPlot.TunnelEnt,
                           "TLCRate" = TLCRatePlot.TunnelEnt)
      
    } else if (input$RiskEnv == "TunnelExt") {
      
      FinalPlot <- switch (input$Index,
                           "MaxLongitudinalSpeed" = MaxLongitudinalSpeedPlot.TunnelExt,
                           "P85LongitudinalSpeed" = P85LongitudinalSpeedPlot.TunnelExt,
                           "MedianLongitudinalSpeed" = MedianLongitudinalSpeedPlot.TunnelExt,
                           "AvgLongitudinalSpeed" = AvgLongitudinalSpeedPlot.TunnelExt,
                           "MinLongitudinalSpeed" = MinLongitudinalSpeedPlot.TunnelExt,
                           "MaxLongitudinalAcc" = MaxLongitudinalAccPlot.TunnelExt,
                           "MinLongitudinalAcc" = MinLongitudinalAccPlot.TunnelExt,
                           "LongitudinalAccRate" = LongitudinalAccRatePlot.TunnelExt,
                           "AccPedal" = AccPedalPlot.TunnelExt,
                           "AccPedalRate" = AccPedalRatePlot.TunnelExt,
                           "BrakePedal" = BrakePedalPlot.TunnelExt,
                           "BrakePedalRate" = BrakePedalRatePlot.TunnelExt,
                           "SpeedingRate" = SpeedingRatePlot.TunnelExt,
                           "MaxLateralSpeed" = MaxLateralSpeedPlot.TunnelExt,
                           "P85LateralSpeed" = P85LateralSpeedPlot.TunnelExt,
                           "MedianLateralSpeed" = MedianLateralSpeedPlot.TunnelExt,
                           "AvgLateralSpeed" = AvgLateralSpeedPlot.TunnelExt,
                           "MinLateralSpeed" = MinLateralSpeedPlot.TunnelExt,
                           "MaxLateralAcc" = MaxLateralAccPlot.TunnelExt,
                           "MinLateralAcc" = MinLateralAccPlot.TunnelExt,
                           "LateralAccRate" = LateralAccRatePlot.TunnelExt,
                           "Steering" = SteeringPlot.TunnelExt,
                           "SteeringRate" = SteeringRatePlot.TunnelExt,
                           "Laneoffset" = LaneoffsetPlot.TunnelExt,
                           "TLC" = TLCPlot.TunnelExt,
                           "TLCRate" = TLCRatePlot.TunnelExt)
      
    } else if (input$RiskEnv == "8CrossEnt") {
      
      FinalPlot <- switch (input$Index,
                           "MaxLongitudinalSpeed" = MaxLongitudinalSpeedPlot.8CrossEnt,
                           "P85LongitudinalSpeed" = P85LongitudinalSpeedPlot.8CrossEnt,
                           "MedianLongitudinalSpeed" = MedianLongitudinalSpeedPlot.8CrossEnt,
                           "AvgLongitudinalSpeed" = AvgLongitudinalSpeedPlot.8CrossEnt,
                           "MinLongitudinalSpeed" = MinLongitudinalSpeedPlot.8CrossEnt,
                           "MaxLongitudinalAcc" = MaxLongitudinalAccPlot.8CrossEnt,
                           "MinLongitudinalAcc" = MinLongitudinalAccPlot.8CrossEnt,
                           "LongitudinalAccRate" = LongitudinalAccRatePlot.8CrossEnt,
                           "AccPedal" = AccPedalPlot.8CrossEnt,
                           "AccPedalRate" = AccPedalRatePlot.8CrossEnt,
                           "BrakePedal" = BrakePedalPlot.8CrossEnt,
                           "BrakePedalRate" = BrakePedalRatePlot.8CrossEnt,
                           "SpeedingRate" = SpeedingRatePlot.8CrossEnt,
                           "MaxLateralSpeed" = MaxLateralSpeedPlot.8CrossEnt,
                           "P85LateralSpeed" = P85LateralSpeedPlot.8CrossEnt,
                           "MedianLateralSpeed" = MedianLateralSpeedPlot.8CrossEnt,
                           "AvgLateralSpeed" = AvgLateralSpeedPlot.8CrossEnt,
                           "MinLateralSpeed" = MinLateralSpeedPlot.8CrossEnt,
                           "MaxLateralAcc" = MaxLateralAccPlot.8CrossEnt,
                           "MinLateralAcc" = MinLateralAccPlot.8CrossEnt,
                           "LateralAccRate" = LateralAccRatePlot.8CrossEnt,
                           "Steering" = SteeringPlot.8CrossEnt,
                           "SteeringRate" = SteeringRatePlot.8CrossEnt,
                           "Laneoffset" = LaneoffsetPlot.8CrossEnt,
                           "TLC" = TLCPlot.8CrossEnt,
                           "TLCRate" = TLCRatePlot.8CrossEnt)
      
    } else if (input$RiskEnv == "8CrossMln") {
      
      FinalPlot <- switch (input$Index,
                           "MaxLongitudinalSpeed" = MaxLongitudinalSpeedPlot.8CrossMln,
                           "P85LongitudinalSpeed" = P85LongitudinalSpeedPlot.8CrossMln,
                           "MedianLongitudinalSpeed" = MedianLongitudinalSpeedPlot.8CrossMln,
                           "AvgLongitudinalSpeed" = AvgLongitudinalSpeedPlot.8CrossMln,
                           "MinLongitudinalSpeed" = MinLongitudinalSpeedPlot.8CrossMln,
                           "MaxLongitudinalAcc" = MaxLongitudinalAccPlot.8CrossMln,
                           "MinLongitudinalAcc" = MinLongitudinalAccPlot.8CrossMln,
                           "LongitudinalAccRate" = LongitudinalAccRatePlot.8CrossMln,
                           "AccPedal" = AccPedalPlot.8CrossMln,
                           "AccPedalRate" = AccPedalRatePlot.8CrossMln,
                           "BrakePedal" = BrakePedalPlot.8CrossMln,
                           "BrakePedalRate" = BrakePedalRatePlot.8CrossMln,
                           "SpeedingRate" = SpeedingRatePlot.8CrossMln,
                           "MaxLateralSpeed" = MaxLateralSpeedPlot.8CrossMln,
                           "P85LateralSpeed" = P85LateralSpeedPlot.8CrossMln,
                           "MedianLateralSpeed" = MedianLateralSpeedPlot.8CrossMln,
                           "AvgLateralSpeed" = AvgLateralSpeedPlot.8CrossMln,
                           "MinLateralSpeed" = MinLateralSpeedPlot.8CrossMln,
                           "MaxLateralAcc" = MaxLateralAccPlot.8CrossMln,
                           "MinLateralAcc" = MinLateralAccPlot.8CrossMln,
                           "LateralAccRate" = LateralAccRatePlot.8CrossMln,
                           "Steering" = SteeringPlot.8CrossMln,
                           "SteeringRate" = SteeringRatePlot.8CrossMln,
                           "Laneoffset" = LaneoffsetPlot.8CrossMln,
                           "TLC" = TLCPlot.8CrossMln,
                           "TLCRate" = TLCRatePlot.8CrossMln)
      
    } else if (input$RiskEnv == "8CrossExt") {
      
      FinalPlot <- switch (input$Index,
                           "MaxLongitudinalSpeed" = MaxLongitudinalSpeedPlot.8CrossExt,
                           "P85LongitudinalSpeed" = P85LongitudinalSpeedPlot.8CrossExt,
                           "MedianLongitudinalSpeed" = MedianLongitudinalSpeedPlot.8CrossExt,
                           "AvgLongitudinalSpeed" = AvgLongitudinalSpeedPlot.8CrossExt,
                           "MinLongitudinalSpeed" = MinLongitudinalSpeedPlot.8CrossExt,
                           "MaxLongitudinalAcc" = MaxLongitudinalAccPlot.8CrossExt,
                           "MinLongitudinalAcc" = MinLongitudinalAccPlot.8CrossExt,
                           "LongitudinalAccRate" = LongitudinalAccRatePlot.8CrossExt,
                           "AccPedal" = AccPedalPlot.8CrossExt,
                           "AccPedalRate" = AccPedalRatePlot.8CrossExt,
                           "BrakePedal" = BrakePedalPlot.8CrossExt,
                           "BrakePedalRate" = BrakePedalRatePlot.8CrossExt,
                           "SpeedingRate" = SpeedingRatePlot.8CrossExt,
                           "MaxLateralSpeed" = MaxLateralSpeedPlot.8CrossExt,
                           "P85LateralSpeed" = P85LateralSpeedPlot.8CrossExt,
                           "MedianLateralSpeed" = MedianLateralSpeedPlot.8CrossExt,
                           "AvgLateralSpeed" = AvgLateralSpeedPlot.8CrossExt,
                           "MinLateralSpeed" = MinLateralSpeedPlot.8CrossExt,
                           "MaxLateralAcc" = MaxLateralAccPlot.8CrossExt,
                           "MinLateralAcc" = MinLateralAccPlot.8CrossExt,
                           "LateralAccRate" = LateralAccRatePlot.8CrossExt,
                           "Steering" = SteeringPlot.8CrossExt,
                           "SteeringRate" = SteeringRatePlot.8CrossExt,
                           "Laneoffset" = LaneoffsetPlot.8CrossExt,
                           "TLC" = TLCPlot.8CrossExt,
                           "TLCRate" = TLCRatePlot.8CrossExt)
      
    } else if (input$RiskEnv == "4CrossEnt") {
      
      FinalPlot <- switch (input$Index,
                           "MaxLongitudinalSpeed" = MaxLongitudinalSpeedPlot.4CrossEnt,
                           "P85LongitudinalSpeed" = P85LongitudinalSpeedPlot.4CrossEnt,
                           "MedianLongitudinalSpeed" = MedianLongitudinalSpeedPlot.4CrossEnt,
                           "AvgLongitudinalSpeed" = AvgLongitudinalSpeedPlot.4CrossEnt,
                           "MinLongitudinalSpeed" = MinLongitudinalSpeedPlot.4CrossEnt,
                           "MaxLongitudinalAcc" = MaxLongitudinalAccPlot.4CrossEnt,
                           "MinLongitudinalAcc" = MinLongitudinalAccPlot.4CrossEnt,
                           "LongitudinalAccRate" = LongitudinalAccRatePlot.4CrossEnt,
                           "AccPedal" = AccPedalPlot.4CrossEnt,
                           "AccPedalRate" = AccPedalRatePlot.4CrossEnt,
                           "BrakePedal" = BrakePedalPlot.4CrossEnt,
                           "BrakePedalRate" = BrakePedalRatePlot.4CrossEnt,
                           "SpeedingRate" = SpeedingRatePlot.4CrossEnt,
                           "MaxLateralSpeed" = MaxLateralSpeedPlot.4CrossEnt,
                           "P85LateralSpeed" = P85LateralSpeedPlot.4CrossEnt,
                           "MedianLateralSpeed" = MedianLateralSpeedPlot.4CrossEnt,
                           "AvgLateralSpeed" = AvgLateralSpeedPlot.4CrossEnt,
                           "MinLateralSpeed" = MinLateralSpeedPlot.4CrossEnt,
                           "MaxLateralAcc" = MaxLateralAccPlot.4CrossEnt,
                           "MinLateralAcc" = MinLateralAccPlot.4CrossEnt,
                           "LateralAccRate" = LateralAccRatePlot.4CrossEnt,
                           "Steering" = SteeringPlot.4CrossEnt,
                           "SteeringRate" = SteeringRatePlot.4CrossEnt,
                           "Laneoffset" = LaneoffsetPlot.4CrossEnt,
                           "TLC" = TLCPlot.4CrossEnt,
                           "TLCRate" = TLCRatePlot.4CrossEnt)
      
    } else if (input$RiskEnv == "4CrossMln") {
      
      FinalPlot <- switch (input$Index,
                           "MaxLongitudinalSpeed" = MaxLongitudinalSpeedPlot.4CrossMln,
                           "P85LongitudinalSpeed" = P85LongitudinalSpeedPlot.4CrossMln,
                           "MedianLongitudinalSpeed" = MedianLongitudinalSpeedPlot.4CrossMln,
                           "AvgLongitudinalSpeed" = AvgLongitudinalSpeedPlot.4CrossMln,
                           "MinLongitudinalSpeed" = MinLongitudinalSpeedPlot.4CrossMln,
                           "MaxLongitudinalAcc" = MaxLongitudinalAccPlot.4CrossMln,
                           "MinLongitudinalAcc" = MinLongitudinalAccPlot.4CrossMln,
                           "LongitudinalAccRate" = LongitudinalAccRatePlot.4CrossMln,
                           "AccPedal" = AccPedalPlot.4CrossMln,
                           "AccPedalRate" = AccPedalRatePlot.4CrossMln,
                           "BrakePedal" = BrakePedalPlot.4CrossMln,
                           "BrakePedalRate" = BrakePedalRatePlot.4CrossMln,
                           "SpeedingRate" = SpeedingRatePlot.4CrossMln,
                           "MaxLateralSpeed" = MaxLateralSpeedPlot.4CrossMln,
                           "P85LateralSpeed" = P85LateralSpeedPlot.4CrossMln,
                           "MedianLateralSpeed" = MedianLateralSpeedPlot.4CrossMln,
                           "AvgLateralSpeed" = AvgLateralSpeedPlot.4CrossMln,
                           "MinLateralSpeed" = MinLateralSpeedPlot.4CrossMln,
                           "MaxLateralAcc" = MaxLateralAccPlot.4CrossMln,
                           "MinLateralAcc" = MinLateralAccPlot.4CrossMln,
                           "LateralAccRate" = LateralAccRatePlot.4CrossMln,
                           "Steering" = SteeringPlot.4CrossMln,
                           "SteeringRate" = SteeringRatePlot.4CrossMln,
                           "Laneoffset" = LaneoffsetPlot.4CrossMln,
                           "TLC" = TLCPlot.4CrossMln,
                           "TLCRate" = TLCRatePlot.4CrossMln)
      
    } else if (input$RiskEnv == "4CrossExt") {
      
      FinalPlot <- switch (input$Index,
                           "MaxLongitudinalSpeed" = MaxLongitudinalSpeedPlot.4CrossExt,
                           "P85LongitudinalSpeed" = P85LongitudinalSpeedPlot.4CrossExt,
                           "MedianLongitudinalSpeed" = MedianLongitudinalSpeedPlot.4CrossExt,
                           "AvgLongitudinalSpeed" = AvgLongitudinalSpeedPlot.4CrossExt,
                           "MinLongitudinalSpeed" = MinLongitudinalSpeedPlot.4CrossExt,
                           "MaxLongitudinalAcc" = MaxLongitudinalAccPlot.4CrossExt,
                           "MinLongitudinalAcc" = MinLongitudinalAccPlot.4CrossExt,
                           "LongitudinalAccRate" = LongitudinalAccRatePlot.4CrossExt,
                           "AccPedal" = AccPedalPlot.4CrossExt,
                           "AccPedalRate" = AccPedalRatePlot.4CrossExt,
                           "BrakePedal" = BrakePedalPlot.4CrossExt,
                           "BrakePedalRate" = BrakePedalRatePlot.4CrossExt,
                           "SpeedingRate" = SpeedingRatePlot.4CrossExt,
                           "MaxLateralSpeed" = MaxLateralSpeedPlot.4CrossExt,
                           "P85LateralSpeed" = P85LateralSpeedPlot.4CrossExt,
                           "MedianLateralSpeed" = MedianLateralSpeedPlot.4CrossExt,
                           "AvgLateralSpeed" = AvgLateralSpeedPlot.4CrossExt,
                           "MinLateralSpeed" = MinLateralSpeedPlot.4CrossExt,
                           "MaxLateralAcc" = MaxLateralAccPlot.4CrossExt,
                           "MinLateralAcc" = MinLateralAccPlot.4CrossExt,
                           "LateralAccRate" = LateralAccRatePlot.4CrossExt,
                           "Steering" = SteeringPlot.4CrossExt,
                           "SteeringRate" = SteeringRatePlot.4CrossExt,
                           "Laneoffset" = LaneoffsetPlot.4CrossExt,
                           "TLC" = TLCPlot.4CrossExt,
                           "TLCRate" = TLCRatePlot.4CrossExt)
      
    }
    
    FinalPlot
  })
  
  
  FinalTable <- reactive({
    
    if (input$RiskEnv == "TunnelEnt") {
      
      RiskDF <- switch (input$Index,
                        "MaxLongitudinalSpeed" = LFMaxLongitudinalSpeed.TunnelEnt,
                        "P85LongitudinalSpeed" = LFP85LongitudinalSpeed.TunnelEnt,
                        "MedianLongitudinalSpeed" = LFMedianLongitudinalSpeed.TunnelEnt,
                        "AvgLongitudinalSpeed" = LFAvgLongitudinalSpeed.TunnelEnt,
                        "MinLongitudinalSpeed" = LFMinLongitudinalSpeed.TunnelEnt,
                        "MaxLongitudinalAcc" = LFMaxLongitudinalAcc.TunnelEnt,
                        "MinLongitudinalAcc" = LFMinLongitudinalAcc.TunnelEnt,
                        "LongitudinalAccRate" = LFLongitudinalAccRate.TunnelEnt,
                        "AccPedal" = LFAccPedal.TunnelEnt,
                        "AccPedalRate" = LFAccPedalRate.TunnelEnt,
                        "BrakePedal" = LFBrakePedal.TunnelEnt,
                        "BrakePedalRate" = LFBrakePedalRate.TunnelEnt,
                        "SpeedingRate" = LFSpeedingRate.TunnelEnt,
                        "MaxLateralSpeed" = LFMaxLateralSpeed.TunnelEnt,
                        "P85LateralSpeed" = LFP85LateralSpeed.TunnelEnt,
                        "MedianLateralSpeed" = LFMedianLateralSpeed.TunnelEnt,
                        "AvgLateralSpeed" = LFAvgLateralSpeed.TunnelEnt,
                        "MinLateralSpeed" = LFMinLateralSpeed.TunnelEnt,
                        "MaxLateralAcc" = LFMaxLateralAcc.TunnelEnt,
                        "MinLateralAcc" = LFMinLateralAcc.TunnelEnt,
                        "LateralAccRate" = LFLateralAccRate.TunnelEnt,
                        "Steering" = LFSteering.TunnelEnt,
                        "SteeringRate" = LFSteeringRate.TunnelEnt,
                        "Laneoffset" = LFLaneoffset.TunnelEnt,
                        "TLC" = LFTLC.TunnelEnt,
                        "TLCRate" = LFTLCRate.TunnelEnt)
      
    } else if (input$RiskEnv == "TunnelExt") {
      
      RiskDF <- switch (input$Index,
                        "MaxLongitudinalSpeed" = LFMaxLongitudinalSpeed.TunnelExt,
                        "P85LongitudinalSpeed" = LFP85LongitudinalSpeed.TunnelExt,
                        "MedianLongitudinalSpeed" = LFMedianLongitudinalSpeed.TunnelExt,
                        "AvgLongitudinalSpeed" = LFAvgLongitudinalSpeed.TunnelExt,
                        "MinLongitudinalSpeed" = LFMinLongitudinalSpeed.TunnelExt,
                        "MaxLongitudinalAcc" = LFMaxLongitudinalAcc.TunnelExt,
                        "MinLongitudinalAcc" = LFMinLongitudinalAcc.TunnelExt,
                        "LongitudinalAccRate" = LFLongitudinalAccRate.TunnelExt,
                        "AccPedal" = LFAccPedal.TunnelExt,
                        "AccPedalRate" = LFAccPedalRate.TunnelExt,
                        "BrakePedal" = LFBrakePedal.TunnelExt,
                        "BrakePedalRate" = LFBrakePedalRate.TunnelExt,
                        "SpeedingRate" = LFSpeedingRate.TunnelExt,
                        "MaxLateralSpeed" = LFMaxLateralSpeed.TunnelExt,
                        "P85LateralSpeed" = LFP85LateralSpeed.TunnelExt,
                        "MedianLateralSpeed" = LFMedianLateralSpeed.TunnelExt,
                        "AvgLateralSpeed" = LFAvgLateralSpeed.TunnelExt,
                        "MinLateralSpeed" = LFMinLateralSpeed.TunnelExt,
                        "MaxLateralAcc" = LFMaxLateralAcc.TunnelExt,
                        "MinLateralAcc" = LFMinLateralAcc.TunnelExt,
                        "LateralAccRate" = LFLateralAccRate.TunnelExt,
                        "Steering" = LFSteering.TunnelExt,
                        "SteeringRate" = LFSteeringRate.TunnelExt,
                        "Laneoffset" = LFLaneoffset.TunnelExt,
                        "TLC" = LFTLC.TunnelExt,
                        "TLCRate" = LFTLCRate.TunnelExt)
      
    } else if (input$RiskEnv == "8CrossEnt") {
      
      RiskDF <- switch (input$Index,
                        "MaxLongitudinalSpeed" = LFMaxLongitudinalSpeed.8CrossEnt,
                        "P85LongitudinalSpeed" = LFP85LongitudinalSpeed.8CrossEnt,
                        "MedianLongitudinalSpeed" = LFMedianLongitudinalSpeed.8CrossEnt,
                        "AvgLongitudinalSpeed" = LFAvgLongitudinalSpeed.8CrossEnt,
                        "MinLongitudinalSpeed" = LFMinLongitudinalSpeed.8CrossEnt,
                        "MaxLongitudinalAcc" = LFMaxLongitudinalAcc.8CrossEnt,
                        "MinLongitudinalAcc" = LFMinLongitudinalAcc.8CrossEnt,
                        "LongitudinalAccRate" = LFLongitudinalAccRate.8CrossEnt,
                        "AccPedal" = LFAccPedal.8CrossEnt,
                        "AccPedalRate" = LFAccPedalRate.8CrossEnt,
                        "BrakePedal" = LFBrakePedal.8CrossEnt,
                        "BrakePedalRate" = LFBrakePedalRate.8CrossEnt,
                        "SpeedingRate" = LFSpeedingRate.8CrossEnt,
                        "MaxLateralSpeed" = LFMaxLateralSpeed.8CrossEnt,
                        "P85LateralSpeed" = LFP85LateralSpeed.8CrossEnt,
                        "MedianLateralSpeed" = LFMedianLateralSpeed.8CrossEnt,
                        "AvgLateralSpeed" = LFAvgLateralSpeed.8CrossEnt,
                        "MinLateralSpeed" = LFMinLateralSpeed.8CrossEnt,
                        "MaxLateralAcc" = LFMaxLateralAcc.8CrossEnt,
                        "MinLateralAcc" = LFMinLateralAcc.8CrossEnt,
                        "LateralAccRate" = LFLateralAccRate.8CrossEnt,
                        "Steering" = LFSteering.8CrossEnt,
                        "SteeringRate" = LFSteeringRate.8CrossEnt,
                        "Laneoffset" = LFLaneoffset.8CrossEnt,
                        "TLC" = LFTLC.8CrossEnt,
                        "TLCRate" = LFTLCRate.8CrossEnt)
      
    } else if (input$RiskEnv == "8CrossMln") {
      
      RiskDF <- switch (input$Index,
                        "MaxLongitudinalSpeed" = LFMaxLongitudinalSpeed.8CrossMln,
                        "P85LongitudinalSpeed" = LFP85LongitudinalSpeed.8CrossMln,
                        "MedianLongitudinalSpeed" = LFMedianLongitudinalSpeed.8CrossMln,
                        "AvgLongitudinalSpeed" = LFAvgLongitudinalSpeed.8CrossMln,
                        "MinLongitudinalSpeed" = LFMinLongitudinalSpeed.8CrossMln,
                        "MaxLongitudinalAcc" = LFMaxLongitudinalAcc.8CrossMln,
                        "MinLongitudinalAcc" = LFMinLongitudinalAcc.8CrossMln,
                        "LongitudinalAccRate" = LFLongitudinalAccRate.8CrossMln,
                        "AccPedal" = LFAccPedal.8CrossMln,
                        "AccPedalRate" = LFAccPedalRate.8CrossMln,
                        "BrakePedal" = LFBrakePedal.8CrossMln,
                        "BrakePedalRate" = LFBrakePedalRate.8CrossMln,
                        "SpeedingRate" = LFSpeedingRate.8CrossMln,
                        "MaxLateralSpeed" = LFMaxLateralSpeed.8CrossMln,
                        "P85LateralSpeed" = LFP85LateralSpeed.8CrossMln,
                        "MedianLateralSpeed" = LFMedianLateralSpeed.8CrossMln,
                        "AvgLateralSpeed" = LFAvgLateralSpeed.8CrossMln,
                        "MinLateralSpeed" = LFMinLateralSpeed.8CrossMln,
                        "MaxLateralAcc" = LFMaxLateralAcc.8CrossMln,
                        "MinLateralAcc" = LFMinLateralAcc.8CrossMln,
                        "LateralAccRate" = LFLateralAccRate.8CrossMln,
                        "Steering" = LFSteering.8CrossMln,
                        "SteeringRate" = LFSteeringRate.8CrossMln,
                        "Laneoffset" = LFLaneoffset.8CrossMln,
                        "TLC" = LFTLC.8CrossMln,
                        "TLCRate" = LFTLCRate.8CrossMln)
      
    } else if (input$RiskEnv == "8CrossExt") {
      
      RiskDF <- switch (input$Index,
                        "MaxLongitudinalSpeed" = LFMaxLongitudinalSpeed.8CrossExt,
                        "P85LongitudinalSpeed" = LFP85LongitudinalSpeed.8CrossExt,
                        "MedianLongitudinalSpeed" = LFMedianLongitudinalSpeed.8CrossExt,
                        "AvgLongitudinalSpeed" = LFAvgLongitudinalSpeed.8CrossExt,
                        "MinLongitudinalSpeed" = LFMinLongitudinalSpeed.8CrossExt,
                        "MaxLongitudinalAcc" = LFMaxLongitudinalAcc.8CrossExt,
                        "MinLongitudinalAcc" = LFMinLongitudinalAcc.8CrossExt,
                        "LongitudinalAccRate" = LFLongitudinalAccRate.8CrossExt,
                        "AccPedal" = LFAccPedal.8CrossExt,
                        "AccPedalRate" = LFAccPedalRate.8CrossExt,
                        "BrakePedal" = LFBrakePedal.8CrossExt,
                        "BrakePedalRate" = LFBrakePedalRate.8CrossExt,
                        "SpeedingRate" = LFSpeedingRate.8CrossExt,
                        "MaxLateralSpeed" = LFMaxLateralSpeed.8CrossExt,
                        "P85LateralSpeed" = LFP85LateralSpeed.8CrossExt,
                        "MedianLateralSpeed" = LFMedianLateralSpeed.8CrossExt,
                        "AvgLateralSpeed" = LFAvgLateralSpeed.8CrossExt,
                        "MinLateralSpeed" = LFMinLateralSpeed.8CrossExt,
                        "MaxLateralAcc" = LFMaxLateralAcc.8CrossExt,
                        "MinLateralAcc" = LFMinLateralAcc.8CrossExt,
                        "LateralAccRate" = LFLateralAccRate.8CrossExt,
                        "Steering" = LFSteering.8CrossExt,
                        "SteeringRate" = LFSteeringRate.8CrossExt,
                        "Laneoffset" = LFLaneoffset.8CrossExt,
                        "TLC" = LFTLC.8CrossExt,
                        "TLCRate" = LFTLCRate.8CrossExt)
      
    } else if (input$RiskEnv == "4CrossEnt") {
      
      RiskDF <- switch (input$Index,
                        "MaxLongitudinalSpeed" = LFMaxLongitudinalSpeed.4CrossEnt,
                        "P85LongitudinalSpeed" = LFP85LongitudinalSpeed.4CrossEnt,
                        "MedianLongitudinalSpeed" = LFMedianLongitudinalSpeed.4CrossEnt,
                        "AvgLongitudinalSpeed" = LFAvgLongitudinalSpeed.4CrossEnt,
                        "MinLongitudinalSpeed" = LFMinLongitudinalSpeed.4CrossEnt,
                        "MaxLongitudinalAcc" = LFMaxLongitudinalAcc.4CrossEnt,
                        "MinLongitudinalAcc" = LFMinLongitudinalAcc.4CrossEnt,
                        "LongitudinalAccRate" = LFLongitudinalAccRate.4CrossEnt,
                        "AccPedal" = LFAccPedal.4CrossEnt,
                        "AccPedalRate" = LFAccPedalRate.4CrossEnt,
                        "BrakePedal" = LFBrakePedal.4CrossEnt,
                        "BrakePedalRate" = LFBrakePedalRate.4CrossEnt,
                        "SpeedingRate" = LFSpeedingRate.4CrossEnt,
                        "MaxLateralSpeed" = LFMaxLateralSpeed.4CrossEnt,
                        "P85LateralSpeed" = LFP85LateralSpeed.4CrossEnt,
                        "MedianLateralSpeed" = LFMedianLateralSpeed.4CrossEnt,
                        "AvgLateralSpeed" = LFAvgLateralSpeed.4CrossEnt,
                        "MinLateralSpeed" = LFMinLateralSpeed.4CrossEnt,
                        "MaxLateralAcc" = LFMaxLateralAcc.4CrossEnt,
                        "MinLateralAcc" = LFMinLateralAcc.4CrossEnt,
                        "LateralAccRate" = LFLateralAccRate.4CrossEnt,
                        "Steering" = LFSteering.4CrossEnt,
                        "SteeringRate" = LFSteeringRate.4CrossEnt,
                        "Laneoffset" = LFLaneoffset.4CrossEnt,
                        "TLC" = LFTLC.4CrossEnt,
                        "TLCRate" = LFTLCRate.4CrossEnt)
      
    } else if (input$RiskEnv == "4CrossMln") {
      
      RiskDF <- switch (input$Index,
                        "MaxLongitudinalSpeed" = LFMaxLongitudinalSpeed.4CrossMln,
                        "P85LongitudinalSpeed" = LFP85LongitudinalSpeed.4CrossMln,
                        "MedianLongitudinalSpeed" = LFMedianLongitudinalSpeed.4CrossMln,
                        "AvgLongitudinalSpeed" = LFAvgLongitudinalSpeed.4CrossMln,
                        "MinLongitudinalSpeed" = LFMinLongitudinalSpeed.4CrossMln,
                        "MaxLongitudinalAcc" = LFMaxLongitudinalAcc.4CrossMln,
                        "MinLongitudinalAcc" = LFMinLongitudinalAcc.4CrossMln,
                        "LongitudinalAccRate" = LFLongitudinalAccRate.4CrossMln,
                        "AccPedal" = LFAccPedal.4CrossMln,
                        "AccPedalRate" = LFAccPedalRate.4CrossMln,
                        "BrakePedal" = LFBrakePedal.4CrossMln,
                        "BrakePedalRate" = LFBrakePedalRate.4CrossMln,
                        "SpeedingRate" = LFSpeedingRate.4CrossMln,
                        "MaxLateralSpeed" = LFMaxLateralSpeed.4CrossMln,
                        "P85LateralSpeed" = LFP85LateralSpeed.4CrossMln,
                        "MedianLateralSpeed" = LFMedianLateralSpeed.4CrossMln,
                        "AvgLateralSpeed" = LFAvgLateralSpeed.4CrossMln,
                        "MinLateralSpeed" = LFMinLateralSpeed.4CrossMln,
                        "MaxLateralAcc" = LFMaxLateralAcc.4CrossMln,
                        "MinLateralAcc" = LFMinLateralAcc.4CrossMln,
                        "LateralAccRate" = LFLateralAccRate.4CrossMln,
                        "Steering" = LFSteering.4CrossMln,
                        "SteeringRate" = LFSteeringRate.4CrossMln,
                        "Laneoffset" = LFLaneoffset.4CrossMln,
                        "TLC" = LFTLC.4CrossMln,
                        "TLCRate" = LFTLCRate.4CrossMln)
      
    } else if (input$RiskEnv == "4CrossExt") {
      
      RiskDF <- switch (input$Index,
                        "MaxLongitudinalSpeed" = LFMaxLongitudinalSpeed.4CrossExt,
                        "P85LongitudinalSpeed" = LFP85LongitudinalSpeed.4CrossExt,
                        "MedianLongitudinalSpeed" = LFMedianLongitudinalSpeed.4CrossExt,
                        "AvgLongitudinalSpeed" = LFAvgLongitudinalSpeed.4CrossExt,
                        "MinLongitudinalSpeed" = LFMinLongitudinalSpeed.4CrossExt,
                        "MaxLongitudinalAcc" = LFMaxLongitudinalAcc.4CrossExt,
                        "MinLongitudinalAcc" = LFMinLongitudinalAcc.4CrossExt,
                        "LongitudinalAccRate" = LFLongitudinalAccRate.4CrossExt,
                        "AccPedal" = LFAccPedal.4CrossExt,
                        "AccPedalRate" = LFAccPedalRate.4CrossExt,
                        "BrakePedal" = LFBrakePedal.4CrossExt,
                        "BrakePedalRate" = LFBrakePedalRate.4CrossExt,
                        "SpeedingRate" = LFSpeedingRate.4CrossExt,
                        "MaxLateralSpeed" = LFMaxLateralSpeed.4CrossExt,
                        "P85LateralSpeed" = LFP85LateralSpeed.4CrossExt,
                        "MedianLateralSpeed" = LFMedianLateralSpeed.4CrossExt,
                        "AvgLateralSpeed" = LFAvgLateralSpeed.4CrossExt,
                        "MinLateralSpeed" = LFMinLateralSpeed.4CrossExt,
                        "MaxLateralAcc" = LFMaxLateralAcc.4CrossExt,
                        "MinLateralAcc" = LFMinLateralAcc.4CrossExt,
                        "LateralAccRate" = LFLateralAccRate.4CrossExt,
                        "Steering" = LFSteering.4CrossExt,
                        "SteeringRate" = LFSteeringRate.4CrossExt,
                        "Laneoffset" = LFLaneoffset.4CrossExt,
                        "TLC" = LFTLC.4CrossExt,
                        "TLCRate" = LFTLCRate.4CrossExt)
      
    }
    
    dcast(RiskDF[RiskDF$position.disgap == input$DisPosition,],
          position.disgap~Type)
    
  }) 
  
  output$IndexTable <- renderTable({
    
    FinalTable()
    
  })
  
})
