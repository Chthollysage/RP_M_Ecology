library(data.table)
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(plyr)
library(car)
library(agricolae)
library(pak)

df <- read_excel("r.xlsx")#读数据文件
df_long <- melt(df, id.vars = c("N","L"), value.name = "c")

#删掉含有空的行
df_long <- na.omit(df_long)

# 移除 variable 列中的多余字符，只保留第一个数字
df_long$day <- as.integer(gsub("^\\D*(\\d+).*", "\\1", df_long$variable))

# 移除原来的 variable 列
df_long <- df_long[, -which(names(df_long) == "variable")]

# 修改列名为 "day"
names(df_long)[names(df_long) == "day"] <- "day"

#光照单因素
df_irritante <- df_long[df_long$N %in% c(4), ]
  
#筛选出4-6次的数据
df_grow <- df_irritante[df_irritante$day %in% c(4,5,6,7), ]


# 根据变量进行分组，计算每组的均值
df_cleaned <- df_grow %>%
  group_by(L) %>%
  filter(c > quantile(c, 0.25) & c < quantile(c, 0.75))


#######绘图数据处理
# 正态性检验
## qq图 点沿着线分布 则满足正态分布
qqPlot(lm(c ~ L, data = df_cleaned), 
       simulate=TRUE, main="Q-Q Plot", 
       lables=FALSE)

# 方差齐性检验
kruskal.test(c ~ factor(L), data = df_cleaned)
#一般情况下，如果 p 值小于设定的显著性水平（通常为 0.05），则表示不同组别的方差不具有统计显著性差异，即可以接受方差齐性假设；
#反之，如果 p 值大于显著性水平，则表示不同组别的方差存在统计显著性差异，即拒绝了方差齐性假设。

# 方差分析
oneway<-aov(c ~ factor(L), data = df_cleaned)
summary(oneway)
# 进行多重比较
out <- LSD.test(oneway,"factor(L)",p.adj ="none")#或者等于更加严格的p.adj = “bonferroni”；
print(out$groups)
# 标记字母法数据准备
mark <- data.frame(out$groups)
mark$group1 = rownames(mark)
head(mark)
str(mark)

#画图
ggplot(df_cleaned, aes(x = factor(L) , y = c , fill = factor(L))) +
  geom_bar(alpha = 0.7, stat = "summary", fun = "mean", position = "dodge",width = 0.3) +
  geom_point(size = 2, shape = 16, aes(color = factor(L)), position = position_jitter(width = 0.1)) +
  geom_errorbar(stat = "summary", fun.data = "mean_cl_normal", position = position_dodge(width = 0.4), width = 0.2) +
  scale_fill_manual(values = c("#9180AC", "#D9BDD8","#8AB1D2","#F99391","#9DD0C7")) +
  scale_color_manual(values = c("#907bb2", "#bb89ba","#5890be","#e35a58","#73b1a6")) +
  xlab("irritante") + ylab("cell concentration") +
  theme_bw() + 
  annotate("text", x = mark$group1, y = mark$c + 53, label = mark$groups, color = "black", size = 5) + #标记字母
  theme(legend.position = "none")


