library(tidyverse)
library(grid)
library(gridtext)
library(gridExtra)
library(gtable)
library(patchwork)


##################################
#
# Here you will learn some basics
# on combining plots 
#
##################################

data_wide <- read_csv("data/saarland.csv")

right_plot <- 
  ggplot(data=data_wide) + 
  geom_col( aes(x=age, y=w2022/1000), width=1, fill="#9dceff") + 
  coord_flip() + 
  scale_x_continuous(expand=c(0,0), breaks=seq(0,100,10)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,10,2), limits = c(0,10)) +
  labs(x=NULL, y=NULL) +
  theme_minimal(base_family = "Verdana", base_size = 12) + 
  theme(
    axis.text.y = element_text(hjust=.5, size=24),
    plot.background = element_rect(
      colour = "red", fill = NA, linewidth = 2, linetype = "dashed"
    )
  )

right_plot


left_plot <- 
  ggplot(data=data_wide) + 
  geom_col( aes(x=age, y=m2022/-1000), width=1, fill="#9dceff") + 
  coord_flip() + 
  scale_x_continuous(expand=c(0,0), breaks=seq(0,100,10), position = "top") +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,-10,-2), limits = c(0,-10),
                     labels = \(x) -x) +
  labs(x=NULL, y=NULL) +
  theme_minimal(base_family = "Verdana", base_size = 12) + 
  theme(
    axis.text.y = element_text(hjust=.5, size=24),
    plot.background = element_rect(
      colour = "blue", fill = NA, linewidth = 2
    )
  )

left_plot1 <- left_plot +
                theme(axis.text.y.right = element_text(color="green"))

left_plot2 <- left_plot +
                theme(axis.text.y.right = element_blank())

left_plot3 <- left_plot +
  scale_y_continuous(expand=c(0,0), limits = c(0,-14),
                     breaks=seq(0,-14,-2), labels = \(x) -x) +
  theme(axis.text.y.right = element_blank())

# border colors for better differentiation
left_plot1
# omit y-axis on left plot
left_plot2
# special case asymmetrical pyramid, e.g. UAE
left_plot3


##################################
#
#  COMBINE THE TWO PLOTS
# 
##################################


# grid: watch the scale units
grid.arrange(grobs=list(left_plot1, right_plot), ncol=2)
grid.arrange(grobs=list(left_plot2, right_plot), ncol=2)

# patchwork: nice syntax, perfect für symmetrical pyramids 
left_plot1 | right_plot
left_plot2 | right_plot

(left_plot2 | right_plot) +
  plot_annotation(title = "My Title or more and Year or Country") &
  theme(plot.title = element_text(hjust = 0.5, size=24, face="bold",
                                  margin = unit(c(0,0,6,0), "pt")))

# special case asymmetrical pyramid fails
left_plot3 | right_plot



# gtable: 
# syntax difficult to remember
# grobs are basically all the plots' parameters as dataframe
# allows most detailed control of all
#
g0 <- ggplotGrob(left_plot3)
g1 <- ggplotGrob(left_plot2)
g2 <- ggplotGrob(right_plot)

grid.newpage()
grid.draw(cbind(g1, g2))

# adding overall title is cumbersome

title <- richtext_grob("My Title or more and Year or Country",
                       hjust=0.5,margin = unit(c(0,0,6,0), "pt"),
                       gp = gpar(fontface = "bold", fontsize = 24))

grid.newpage()
combined <- cbind(g1, g2)
combined <- gtable_add_rows(combined, heights = grobHeight(title) + unit(2, "mm"), pos = 0)
combined <- gtable_add_grob(combined, title, t = 1, l = 1, r = ncol(combined))
grid.draw(combined)


# special case asymmetrical pyramid: solved
panel_0 <- g0$layout$l[g0$layout$name == "panel"]
panel_2 <- g2$layout$l[g2$layout$name == "panel"]

g0$widths[panel_0] <- unit(14, "null")   # left plot  0-14
g2$widths[panel_2] <- unit(10, "null")   # right plot 0-10

grid.newpage()
grid.draw(cbind(g0, g2))



