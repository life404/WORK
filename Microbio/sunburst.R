#! /usr/bin/env R

library(ggplot2)

setwd(getwd())

data <- read.table("旭日图数据.csv", sep = ",", header = T, as.is = T, stringsAsFactors = F)

data <- unique(data[, c("class", "order", "family")])
Ymin <- function(x){

	cross = table(x)
	cross = cross[unique(x)]
	value = 0
	ymin = c(value)

	for(i in 1:(length(cross)-1)){
		value = value + cross[i]
		ymin = c(ymin, value)
	}

	names(ymin) = names(cross)
	ymin
}

Ymax <- function(x){

	cross = table(x)
	cross = cross[unique(x)]
	value = cross[1]
	ymax = c(value)


	for(i in 2:length(cross)){
		value = value + cross[i]
		ymax = c(ymax, value)
	}

	names(ymax) = names(cross)
	ymax
}


Class = data.frame(ymin = Ymin(data$class), ymax = Ymax(data$class), xmin = 0, xmax = 4, label = names(Ymax(data$class)))
Order = data.frame(ymin = Ymin(data$order), ymax = Ymax(data$order), xmin = 4, xmax = 8, label = names(Ymax(data$order)))
Family = data.frame(ymin = Ymin(data$family), ymax = Ymax(data$family), xmin = 8, xmax = 12, label = names(Ymax(data$family)))
#Genus = data.frame(ymin = Ymin(data$genus), ymax = Ymax(data$genus), xmin = 12, xmax = 16, label = names(Ymax(data$genus)))
#Species = data.frame(ymin = Ymin(data$species), ymax = Ymax(data$species), xmin = 16, xmax = 20, label = names(Ymax(data$species)))

sun = rbind(Class, Order, Family)
sun$label = as.character(sun$label)
sun$group = unlist(lapply(sun$label, function(x) if(x %in% data$family){data[which(data$family == x), "order"]} else x))
sun$group = as.character(sun$group)
sun$alpha = lapply(sun$label, function(x) if(x %in% data$family){"alpha"} else{"pure"})
sun$alpha = as.character(sun$alpha)

library(ggplot2)

ggplot(sun) + geom_rect(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = factor(group, levels = unique(group)), alpha = factor(alpha, levels = c("pure", "alpha"))), color = "black") + coord_polar(theta = "y") + theme(panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +  scale_fill_manual(values = c("#277DA1","#F94144",  "#3098C5", "#F3722C", "#F8961E", "#F9844A", "#F9C74F", "#F69D3E", "#DC9B45", "#90BE6D", "#43AA8B", "#C5C35E", "#4D908E", "#247294", "#577590", "#45468B")) 
