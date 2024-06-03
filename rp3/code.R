library(ggplot2)
library(plyr)
library(car)
library(agricolae)
library(pak)

df <- read.csv("data.csv")#读数据文件
groups <- unique(df$group)
salinity <- df$seawater - df$freshwater
df$salinity <- salinity
split_df <- split(df, df$group)

#######绘图数据处理
# 正态性检验
## qq图 点沿着线分布 则满足正态分布
qqPlot(lm(salinity ~ area, data = split_df[["shrimp"]]), 
       simulate=TRUE, main="Q-Q Plot", 
       lables=FALSE)

# 方差齐性检验
kruskal.test(area ~ factor(salinity), data = split_df[["shrimp"]])
#一般情况下，如果 p 值小于设定的显著性水平（通常为 0.05），则表示不同组别的方差不具有统计显著性差异，即可以接受方差齐性假设；
#反之，如果 p 值大于显著性水平，则表示不同组别的方差存在统计显著性差异，即拒绝了方差齐性假设。

# 方差分析
oneway<-aov(area ~ factor(salinity), data = split_df[["shrimp"]])
summary(oneway)
# 进行多重比较
out <- LSD.test(oneway,"factor(salinity)",p.adj ="none")#或者等于更加严格的p.adj = “bonferroni”；
print(out$groups)
# 标记字母法数据准备
mark <- data.frame(out$groups)
mark$group1 = rownames(mark)
head(mark)
str(mark)

#画图
ggplot(split_df[["shrimp"]], aes(x = salinity , y = area , fill = factor(salinity))) +
  geom_boxplot(width = 0.25, position = position_dodge(0.9)) +
  geom_point(alpha = 0.6, shape = 16, aes(color = factor(salinity)), position = position_jitter(width = 0.05)) +
  scale_fill_manual(values = c("#3951a2", "#72aacf","#cae8f2","#fefbba","#fdb96b","#ec5d3b","#a80326")) +
  scale_color_manual(values = c("#173398", "#3491cd","#77cfec","#ddd404","#fb9825","#c02300","#790010")) +
  xlab("Salinity") + ylab("Area") +
  scale_x_continuous(breaks = seq(-3, 3, by = 1)) +  # 设置 x 轴刻度
  theme_bw() + 
  theme(legend.position='none') +
  annotate("text", x = as.numeric(mark$group1), y = mark$area + 50000, label = mark$groups, color = "black", size = 5)#标记字母

  