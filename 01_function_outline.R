library(tidyverse)
library(ggtext)
library(patchwork)

data_wide <- read_csv("data/saarland.csv")

##############################
#
# one function 
# for both pyramid-halves
#
##############################

pyramid_half <- function(data, leftRight, age, bar, step, scale, max) {
  
  direction  <- ifelse(leftRight=="left", -1, 1)
  pyr_margin <- if(leftRight=="left") {c(0,1,0,1)} else {c(0,1,0,0)}
  pyr_fill  <- ifelse(leftRight=="left", "blue", "red")
  pyr_label <- ifelse(leftRight=="left", "Men", "Women")
  axis_pos  <- ifelse(leftRight=="left", "top", "bottom")
  age  <- ensym(age)
  bar  <- ensym(bar)
  step <- ensym(step)
  
  ggplot(data=data) + 
    
    geom_col( aes(x = !!age, y = direction*(!!bar)), 
              width = 1, fill = pyr_fill) + 
    geom_step(aes(x = !!age, y = direction*(!!step)),
              direction="mid", color="grey", linewidth=.7) +
    coord_flip() +
    
    scale_x_continuous(expand=c(0,0), breaks=seq(0,100,10), position = axis_pos) +
    scale_y_continuous(expand=c(0,0), labels=function(x){round(x/(direction*scale),0)}, 
                       breaks=seq(0,direction*max,by=direction*max/4), limits=c(0,direction*max)) +
    
    annotate("text", y=direction*max/1.6, x=95, label= pyr_label,
             hjust=0.5, size.unit = "pt", size=8) + 
    labs(y="in thousand", x=NULL) +
    
    theme_minimal() + 
    theme(
      plot.margin=unit(pyr_margin, "mm"),
      axis.title.x = element_text(hjust=.5),
      axis.text.y = element_text(hjust=.5),
      axis.text.y.right = element_blank(),
      axis.ticks.x = element_line(color = "#dedede", linewidth = 0.5),
    )
}

left_plot  <- 
  pyramid_half(data_wide, "left",  age, m2022, m2011, 1000, 10000)

right_plot <- 
  pyramid_half(data_wide, "right", age, w2022, w2011, 1000, 10000)

# patchwork
(left_plot | right_plot) +
  plot_annotation(title = "Age structure Saarland 2022 
                  vs <span style='color: #777'>2011</span>") &
  theme(plot.title = element_markdown(hjust = 0.5, size=12, face="bold",
                                      margin = unit(c(0,0,9,0), "pt")))

