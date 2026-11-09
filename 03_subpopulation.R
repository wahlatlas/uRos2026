library(tidyverse)
library(ggtext)
library(janitor)
library(patchwork)

# Data from the German Census Databas, Download Format "flatfile-csv"
# https://ergebnisse.zensus2022.de/datenbank/online/table/1000A-3034/

df <- read_csv2("data/1000A-3034_de_flat.csv",
                na = c("", ".", "-", "–", "/", "x","X")
          ) |> 
  clean_names(case="none")


df_wide <- df |> 
  filter(X1_variable_attribute_label=="Hamburg") |> 
  pivot_wider(
    id_cols = c(X2_variable_attribute_code, X2_variable_attribute_label),
    names_from = c(X3_variable_attribute_label, X4_variable_attribute_label),
    values_from = value
  ) |> 
  mutate(X2_variable_attribute_code = recode(X2_variable_attribute_code,
                                             "ALTERU01" = "ALTER000")) |> 
  arrange(X2_variable_attribute_code) |> 
  mutate(alter=seq(0,100))



pyramid_half <- function(data, leftRight, age, bar_total, bar_subpop, scale, max) {
  
  direction <- ifelse(leftRight=="left", -1, 1)
  pyrmargin <- if(leftRight=="left") {c(0,1,0,1)} else {c(0,1,0,0)}
  age_sym <- ensym(age)
  bar1 <- ensym(bar_total)
  bar2 <- ensym(bar_subpop)
  
  ggplot(data=data) + 
    geom_col( aes(x=!!age_sym, y=direction*!!bar1), width=1, 
              fill=ifelse(leftRight=="left", "blue", "red")) + 
    geom_col( aes(x=!!age_sym, y=direction*!!bar2), width=1, 
              fill=ifelse(leftRight=="left", "lightblue", "pink")) + 
    coord_flip() + 
    scale_x_continuous(expand=c(0,0), breaks=seq(0,100,10), position = ifelse(leftRight=="left", "top", "bottom")) +
    scale_y_continuous(expand=c(0,0), labels=function(x){round(x/(direction*scale),0)}, 
                       breaks=seq(0,direction*max,by=direction*max/4), limits=c(0,direction*max)) +
    
    annotate("text", y=direction*max/1.6, x=95, label= ifelse(leftRight=="right", "Women", "Men"), 
             hjust=0.5, size.unit = "pt", size=8) + 
    labs(y="Persons in thousand", x=NULL) +
    
    theme_minimal() +
    theme(
      plot.margin=unit(pyrmargin, "mm"),
      axis.title.x = element_text(hjust=.5),
      axis.text.y = element_text(hjust=.5),
      axis.text.y.right = element_blank(),
      axis.ticks.x = element_line(color = "#dedede", linewidth = 0.5),
    )
}

left_plot  <- 
  pyramid_half(df_wide, "left",  alter, Männlich_Insgesamt, Männlich_Asien, 1000, 20000)

right_plot <- 
  pyramid_half(df_wide, "right", alter, Weiblich_Insgesamt, Weiblich_Asien, 1000, 20000)

# patchwork
(left_plot | right_plot) +
  plot_annotation(title = "Age structure Hamburg 2022",
                  subtitle = "Total and citizens of Asia",
                  caption = "Census database table 1000A-3034") &
  theme(plot.title = element_markdown(hjust = 0, size=9, color = "#484848",
                                      face = "bold"), 
       plot.subtitle = element_markdown(hjust = 0, size=8, color = "#484848",
                                      margin = unit(c(0,0,16,0), "pt")),
        plot.caption = element_markdown(size = 7, lineheight = 1.4,
                                      color = "#484848", hjust = 0,
                                      margin = margin(9,0,0,0)))
