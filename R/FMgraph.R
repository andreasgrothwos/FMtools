library(tidyverse)



housing <- txhousing %>% group_by(year, city)  %>%
		   summarise(total = sum(volume, na.rm = T)) %>% filter(city %in% c("El Paso","Dallas", "Houston"))


dat <- housing
yvar  <- dat$total
xvar <- dat$year
gruppe <- dat$city

sd(dat$total)
require(grid)
p<-ggplot(data = dat, aes(x = xvar, y = yvar/1e6, colour = gruppe)) +
  geom_line() + theme_classic() +
  theme(plot.margin = margin(30,0,0,0))+
  annotation_custom(
    grob = textGrob(label = "y-label", hjust = 0, vjust=-0.9,gp = gpar(cex = 1.0)),
    ymin = max(yvar/1e6)+max(yvar/1e6)*.025,
    xmin = min(xvar)-(0.009*min(xvar)))+
	annotation_custom(
    grob = textGrob(label = "y-label2", hjust = 1, vjust=-0.9,gp = gpar(cex = 1.0)),
    ymin = max(yvar/1e6)+max(yvar/1e6)*.025,
    xmin = max(xvar))+
  labs(y = NULL)
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off" #this lets you put stuff outside the margins
grid.draw(gt)


xakse.navn <- "x label"
yakse.navn <- "y label"
enhed <- 1e6

farve1 <- rgb(3, 29, 92, maxColorValue = 255)
farve2 <- rgb(116, 201, 230, maxColorValue = 255)
farve3 <- rgb(148, 0, 39, maxColorValue = 255)
farver <- c(farve1, farve2, farve3)

linesize <- .25

# NEXT STEP (21/08/2019): FIX MARGINER PÅ TEKST SÅ DET PASSER. SE THEME NEDENFOR
# NEXT STEP (1/09/2019): FIX, så mest muligt håndteres i theme_fm funktionen.



y.oevre <- 25*1e3
y.nedre <- 0
y.breaks <- 2.5*1e3

theme_fm <- function (base_size = 11, base_family = "") {
    theme_classic() +
    theme(
			text = element_text(size = 6, family = "sans"), # tekst på legend - ikke hele lortet
					panel.background = element_rect(fill = "transparent",colour = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					plot.margin = margin(15,0,0,0),
					plot.title = element_blank(),
					legend.background = element_rect(fill = "transparent",colour = NA),
					line = element_line(size = linesize),
					legend.position = "bottom",
					legend.margin=margin(t = -15, b = 0),
					legend.title = element_blank(),
					axis.title.x = element_blank(),
					axis.title.y = element_blank(),
					# axis.title.y.left = element_text(angle = 0, margin = margin(r = -25, l = 10)),
				  # axis.title.y.right = element_text(angle = 0, vjust = 1, margin = margin(l = -25, r = 10)),
					axis.text = element_text(colour = "black"),
					axis.ticks = element_line(colour = "black")
      )
}

require(grid)

d <- last_plot()


fm_save <- function (plot = last_plot(), label = "akse-titel", navn = "", ...){
	plot2 <- plot +
 annotation_custom(
	 grob = textGrob(label = label, hjust = 0, vjust=-0.9,gp = gpar(cex = 1.0, fontsize = 6)),
	 ymin = max(yvar/1e6)+max(yvar/1e6)*.15,
	 xmin = min(xvar)-15.5) +
 annotation_custom(
	 grob = textGrob(label = label, hjust = 1, vjust=-0.9,gp = gpar(cex = 1.0, fontsize = 6)),
	 ymin = max(yvar/1e6)+max(yvar/1e6)*.15,
	 xmin = max(xvar)+.5)

gt <- ggplot_gtable(ggplot_build(plot2))
gt$layout$clip[gt$layout$name == "panel"] <- "off" #for at skrive udenfor margin

png(paste0(deparse(substitute(plot)),".png"),width = 5.95, height = 5.07, units = "cm", res = 300, bg = "transparent")
grid.draw(gt)
dev.off()
grid.draw(gt)
# ggsave("testin2.png", width = 5.95, height = 5.07, units = "cm", bg = "transparent")
}



fig2 <- ggplot(data = dat, aes(x = xvar, y = yvar/enhed, colour = gruppe)) + geom_line(size = linesize) + scale_color_manual(values = farver) + scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), expand = c(0,0), limits = c(y.nedre, y.oevre), breaks = seq(y.nedre, y.oevre,y.breaks), sec.axis = dup_axis()) +
scale_x_continuous(expand = c(0,0)) + theme_fm()

fm_save(label = "Pct.", plot = fig2) #+ grid.text("Second Title")



ggsave("testin.png", width = 5.95, height = 5.07, units = "cm", bg = "transparent")

#  LAVER FARVE-funktion

fm_colors <- c(
  `fm1`        = rgb(3, 29, 92, maxColorValue = 255),
  `fm2`      = rgb(116, 201, 230, maxColorValue = 255),
  `fm3`       = rgb(148, 0, 39, maxColorValue = 255))


#' Function to extract drsimonj colors as hex codes
#'
#' @param ... Character names of drsimonj_colors
#'

fm_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (fm_colors)

  fm_colors[cols]
}

# Laver en liste
fm_palettes <- list(
  `main`  = fm_cols("fm1", "fm2", "fm3"),

  `cool`  = fm_cols("blue", "green")
)


# TIL AT LAVE NUANCER

#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'

fm_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- fm_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}




#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'

scale_color_fm <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fm_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("fm_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'

scale_fill_fm <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fm_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("fm_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



ggplot(data = dat, aes(x = xvar, y = yvar/enhed, colour = gruppe)) + geom_line(size = linesize) + scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), expand = c(0,0), limits = c(y.nedre, y.oevre), breaks = seq(y.nedre, y.oevre,y.breaks), sec.axis = dup_axis()) +
scale_x_continuous(expand = c(0,0)) + labs(y = "test") + theme_fm() + scale_color_fm()
