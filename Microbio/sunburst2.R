#! /usr/bin/env R

library(ggplot2)

setwd(getwd())

data <- read.table("旭日图数据.csv", sep = ",", header = T)

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
Genus = data.frame(ymin = Ymin(data$genus), ymax = Ymax(data$genus), xmin = 12, xmax = 16, label = names(Ymax(data$genus)))
Species = data.frame(ymin = Ymin(data$species), ymax = Ymax(data$species), xmin = 16, xmax = 20, label = names(Ymax(data$species)))

sun = rbind(Class, Order, Family)

library(ggplot2)

ggplot(sun) + geom_rect(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), color = "black") + coord_polar(theta = "y")
