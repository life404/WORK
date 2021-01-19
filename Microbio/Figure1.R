library(ggplot2)

setwd(getwd())

data_right_bar  <-  read.table("右条形图.csv", sep = ",", header = T)
data_right_bar$bar_value = as.numeric(data_right_bar$bar_value)

p1 = ggplot(data_right_bar, aes(x = factor(data_right_bar$order, 
									  levels = rev(data_right_bar$order)),
						   y = bar_value*100)) + 
	geom_bar(stat = "identity", width = .6, fill = "#2EC4B6") + 
	theme(panel.background = element_blank(),
		  axis.title = element_blank(),
		  axis.text = element_text(color = "black", size = rel(1.3))) +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50)) + 
	coord_flip() + 
	theme(axis.ticks.x.bottom = element_line(color = "red", size = 1),
		  axis.line.x.bottom = element_line(color = "black", size = 1))

pdf("右条形图.pdf", height = 10, width = 10)
p1
dev.off()

data_right_bub = read.table("右气泡图.csv", sep = ",", header = T)

pest  <- unique(data_right_bub$order)
p2  <- ggplot(data_right_bub, aes(
	x  <-  rep(factor(pest, levels = rev(pest)), time = 4),
	y  <- location, size = value
)) +
geom_point(color = "#2EC4B6") + 
theme(panel.background = element_blank(),
	  axis.title = element_blank(),
	  axis.text = element_text(color = "black", size = rel(1.3)),
	  axis.line = element_blank()) +
scale_y_discrete(position = "right") + 
coord_flip()

pdf("右气泡图.pdf", height = 10, width = 10)
p2
dev.off()

data_left_bar = read.table("左条形图.csv", sep = ",", header = T)
data_left_bar$bar_value = as.numeric(data_left_bar$bar_value)
p3 = ggplot(data_left_bar, aes(
	x = factor(data_left_bar$order, levels = rev(data_left_bar$order)),
	y = bar_value*100
)) + 
geom_bar(stat = "identity", width = .6, fill = "#2EC4B6") + 
theme(panel.background = element_blank(), 
	  axis.title = element_blank(),
	  axis.text = element_text(color = "black", size = rel(1.3))) + 
scale_y_continuous(breaks = seq(0, 100, 10)) + 
coord_flip() + 
theme(axis.ticks.x.bottom = element_line(color = "red", size = 1),
	  axis.line.x.bottom = element_line(color = "black", size = 1))

pdf("左条形图.pdf", height = 10, width = 10)
p3
dev.off()

data_left_bub = read.table("左气泡图.csv", sep = ",", header = T)
pest = unique(data_left_bub$order)
p4 = ggplot(data_left_bub, aes(
	x = rep(factor(pest, levels = rev(pest)), time = 4),
	y = location, size = value 
)) + geom_point(color = "#2EC4B6") + 
theme(panel.background = element_blank(),
	  axis.title = element_blank(),
	  axis.text = element_text(color = "black", size = rel(1.3)),
	  axis.line = element_blank()) + 
scale_y_discrete(position = "right") + 
coord_flip()

pdf("左气泡图.pdf", height = 10, width = 10)
p4
dev.off()


