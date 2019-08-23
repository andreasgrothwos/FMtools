library(tidyverse)



housing <- txhousing %>% group_by(year, city)  %>% 
		   summarise(total = sum(volume, na.rm = T)) %>% filter(city %in% c("El Paso","Dallas", "Houston"))

dat <- housing
yvar  <- dat$total
xvar <- dat$year
gruppe <- dat$city

xakse.navn <- "x label"
yakse.navn <- "y label"
enhed <- 1e6

farve1 <- rgb(3, 29, 92, maxColorValue = 255)
farve2 <- rgb(116, 201, 230, maxColorValue = 255)
farve3 <- rgb(148, 0, 39, maxColorValue = 255)
farver <- c(farve1, farve2, farve3)

linesize <- .25

# NEXT STEP (21/08/2019): FIX MARGINER PÅ TEKST SÅ DET PASSER. SE THEME NEDENFOR

ggplot(data = dat, aes(x = xvar, y = yvar/enhed, colour = gruppe)) + geom_line(size = linesize) + scale_color_manual(values = farver) + scale_y_continuous(sec.axis = sec_axis(~.*1, name = yakse.navn)) + 
											 labs(y = yakse.navn, x = "") + theme_classic() + 
											 theme(text = element_text(size = 6, family = "sans"),
											 	   panel.background = element_rect(fill = "transparent",colour = NA),
        										   plot.background = element_rect(fill = "transparent",colour = NA),
        										   plot.margin = margin(.5,0,0,0, unit = "cm"),
        										   legend.background = element_rect(fill = "transparent",colour = NA),
											 	   line = element_line(size = linesize),
											 	   legend.position = "bottom",
											 	   legend.title = element_blank(),
											 	   axis.title.y.left = element_text(margin(t = 0, unit = "cm"), angle = 0, hjust = -2),
											 	   axis.title.y.right = element_text(margin(t = 0, unit = "cm"), angle = 0,))

ggsave("testin.png", width = 5.95, height = 5.07, units = "cm", bg = "transparent")
