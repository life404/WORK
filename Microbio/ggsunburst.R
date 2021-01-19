#! /usr/bin/env R

library(ggsunburst)

setwd(getwd())
sb <- sunburst_data('./browsers.csv', type = 'node_parent', sep = ",", 
					node_attributes = c("browser", "size"))
