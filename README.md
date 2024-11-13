###Stacked bar plot###
#Reference:
#Muñoz, K.A., Ulrich, R.J., Vasan, A.K. et al. A Gram-negative-selective antibiotic that spares the gut microbiome. Nature 630, 429–436 (2024).
#https://doi.org/10.1038/s41586-024-07502-0
#Original figure：https://www.nature.com/articles/s41586-024-07502-0/figures/5

#install.packages("pacman") #如果没有pacman，请先安装，pacman是一个管理R包的工具
library(pacman)
p_load(reshape2)
getwd()
#由于作者没有提供原始数据，这里生成10行，24列的随机数据用于画图set.seed(123) 
#设置随机种子，方便复现

d1 <- matrix(runif(6 * 24, min = 0, max = 0.05), nrow = 6, ncol = 24) #生成均匀分布的随机数
d2 <- matrix(runif(4 * 24, min = 0.3, max = 0.6), nrow = 4, ncol = 24)#生成均匀分布的随机数
df <- rbind(d1,d2)#按行合并两个数据框                                                
df <- apply(df, 2, function(x) x / sum(x)) #按列归一化
df <- as.data.frame(df)
microbes <- paste0("Microbe_", 1:10)
df$microbes <- microbes
df <- melt(df,id.vars = "microbes") #宽数据转为长数据
df$sample<- rep(1:24, each = 10)
df$day <- c(rep("Day 0",60),rep("Day 7",60), rep("Day 10",60), rep("Day 31",60))
df$day <- factor(df$day,levels = c("Day 0","Day 7","Day 10","Day 31"))
df$microbes <- factor(df$microbes,levels = c("Microbe_7","Microbe_4","Microbe_2","Microbe_9","Microbe_5",
                                            "Microbe_8","Microbe_1","Microbe_3","Microbe_6","Microbe_10"))

p <- ggplot(data = df,aes(x = sample,y = value, fill = microbes)) +
 geom_bar(stat = "identity",position = "stack") +        #绘制堆积柱状图，使用原始数值，并进行堆积
 labs(title = "",subtitle = "Vehicle") +                 #设置标题和副标题
 labs(x = "", y = "Relative abundance",fill = "Family") +#设置坐标轴标签和图例标题
 #手动设置配色方案
 scale_fill_manual(values = c( "Microbe_1"="#D3D1D2", "Microbe_2"="#CC79A8", "Microbe_3"="#7AC0EA", "Microbe_4"="#0EA079","Microbe_5"="#68C49D",
                               "Microbe_6"="#EACEDF", "Microbe_7"="#5AB4E5","Microbe_8"="#D99BBB","Microbe_9"="#98D1F0", "Microbe_10"="#7F7F7F")) +
scale_y_continuous(limits = c(0, 1), expand = c(0.02, 0))+ #limits设置刻度范围；expand = c(lower, upper)：用于设置坐标轴的扩展范围。
theme_classic()+
theme(
plot.subtitle = element_text(color = "black", size = 14),
axis.text = element_text(color = "black", size = 14),
axis.title =  element_text(color = "black", size = 14),
axis.text.x = element_blank(), #隐藏X轴文本
axis.ticks.x = element_blank(),#隐藏X轴刻度
strip.text.x = element_text(size = 14),
strip.background = element_blank(),
strip.placement = "outside",   #facet标签将显示在绘图区域外部
legend.position = "right",
legend.title = element_text(size = 12),
legend.text = element_text(size = 11),
legend.key.size = unit(0.4, "cm"),
legend.key.spacing.y = unit(0.2, "cm"),
panel.spacing = unit(0.05, "lines"),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
panel.background = element_blank())+
facet_grid(~ day,scales = "free", switch="both") 
#分面标签通常展示在plot的上面或右面，switch参数设置为x，上面的标签会放到下面；switch参数设置为y，右面的标签会放到左边，设置为both，相当于x+y
#https://stackoverflow.com/questions/3261597/change-the-position-of-the-strip-label-in-ggplot-from-the-top-to-the-bottom
p
ggsave(filename = "Stacked bar plot.pdf", plot =p, height = 4, width = 6 )
