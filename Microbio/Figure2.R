browser  <- structure(list(
	browser  <- structure(
		c(3L, 3L, 3L, 3L, 2L, 2L, 2L, 1L, 5L, 5L, 4L), 
		.Label = c('Chrome', 'Firefox', 'MSIE', 'Opera', 'Safari')
	),
	version  <- structure(
		c(5L, 6L, 7L, 8L, 2L, 3L, 4L, 1L, 10L, 11L, 9L),
		.Label = c('Chrome 10.0', 'Firefox 3.5', 'Firefox 3.6', 'Firefox 4.0', 'MSIE 6.0', 'MSIE 7.0', 'MSIE 8.0', 'MSIE 9.0', 'Opera 11.x', 'Safari 4.0', 'Safari 5.0')
	),
	share  <- c(10.85, 7.35, 33.06, 2.81, 1.58, 13.12, 5.43, 9.91, 1.42, 4.55, 1.65),
	ymax  <- c(10.85, 18.2, 51.26, 54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 90.08, 91.73), 
	ymin = c(0, 10.85, 18.2, 51.26, 54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 90.08)),
	.Names = c('browser', 'version', 'share', 'ymax', 'ymin'), 
	row.names = c(NA, -11L), 
	class = 'data.frame'
)

tbl <- table(browser$browser)[order(unique(browser$browser))]
# table函数获取数据的列联表，其中第一行表示因子，第二行表示对应因此出现的频次数

col.main <- Map(rep, seq_along(tbl), tbl)
# seq_along 创建一个从1开始，长度等于输入值的序列

col.sub <- lapply(col.main, 
				  function(x) 
					  Vectorize(adjustcolor)(x, 
											 alpha.f = seq_along(x) / length(x)))

pie(browser$share, border = NA, 
	radius = 1, col = unlist(col.sub),
	labels = browser$version
)

par(new = TRUE)
pie(browser$share, border = NA, radius = 0.8, col = unlist(col.main),
	labels = NA)

## 使用ggplot2绘制
library("ggplot2")

p <- ggplot(browser) + 
	geom_rect(aes(fill = version,
				  ymax = ymax, ymin = ymin,
				  xmax = 4, xmin = 3)) +
	geom_rect(aes(fill = browser, 
				  ymax = ymax, ymin = ymin,
				  xmax = 3, xmin = 0)) + 
	xlim(c(0, 4)) + 
	theme(aspect.ratio = 1)
p + coord_polar(theta = "y")

browser$browser = browser$parent
write.table(browser, file = "browser.csv", row.names = F, sep = ",")
