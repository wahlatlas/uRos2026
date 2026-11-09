library(tidyverse)
library(ggtext)
library(patchwork)

# this is how you would read the data form the german census databas
# https://ergebnisse.zensus2022.de/datenbank/online/table/1000A-2027/
# flatfile-csv means long-format data
#
# df <- read_csv2("data/1000A-2027_de_flat.csv",
#                 na = c("", ".", "-", "–", "/", "x","X")
#           ) |> 
#   clean_names(case="none")
# 
# saveRDS(df, file="data/1000A-2027_de_flat_GEOBL1.rds")

df <- readRDS("data/1000A-2027_de_flat_GEOBL1.rds") |> 
        rename(Name= X1_variable_attribute_label,
               AGS = X1_variable_attribute_code) |> 
        filter(Name != "Deutschland")

# percentage retirement age
seniorenAlterCode <- paste0("ALTER0", 65:99)
seniorenAlterCode <- c(seniorenAlterCode, "ALTER100UM")

df <- df |> 
  group_by(AGS) |> 
    mutate(
      senioren = sum(value[X3_variable_attribute_code %in% seniorenAlterCode], na.rm = TRUE),
      insgesamt = sum(value[X3_variable_attribute_label == "Insgesamt"]),
      seniorenAnteil = senioren/insgesamt*100
      ) |> 
  ungroup()

# ranking pct retirement age
ags_senioren_rank <- df |>  
  distinct(AGS, seniorenAnteil) |> 
  arrange(desc(seniorenAnteil)) |> 
  select(AGS)

pyramid_half <- function(data, leftRight, age, bar1, total) {
  
  direction <- ifelse(leftRight=="left", -1, 1)
  pyrmargin <- if(leftRight=="left") {c(0,.5,0,0)} else {c(0,0,0,0)}
  age_sym <- ensym(age)
  bar_sym1 <- ensym(bar1)
  myName  <- sub(",.*", "", data$Name[1])
  mySeniorenAnteil = round(data$seniorenAnteil[1], 1)
  myTitle <- ifelse(leftRight=="left", paste0(myName," ",mySeniorenAnteil,"%"), "")
  myLandInt <- as.integer(substr(data$AGS[1], 1, 2))

  ggplot(data=data) + 
    
    geom_col( aes(x=!!age_sym, y=direction*(!!bar_sym1)/total*100), width=1, 
              fill=ifelse(myLandInt<11, "blue", "red")) + 
    coord_flip() + 
    
    scale_x_continuous(expand=c(0,0), position = ifelse(leftRight=="left", "top", "bottom"),
                       breaks = seq(0,100,10)) +
    
    scale_y_continuous(expand=c(0,0), labels=function(x){round(x/(direction),1)}, 
                       breaks=seq(0.5*direction,direction,by=direction*.5), limits=c(0,direction*2)) +
    
    annotate("label", y=direction, x=95, label= ifelse(leftRight=="right", "Women", "Men"), 
             hjust=0.5, size.unit = "pt", size=5,
             fill = "white", label.padding = unit(2, "mm"), border.colour = NA ) + 
    labs(y="", x=NULL, title = myTitle) + # "Personen in Prozent"
    
    theme_minimal() + 
    theme(
      plot.title = element_markdown(size=7),
      axis.text = element_text(size=5),
      plot.margin = unit(pyrmargin, "mm"),
      axis.title.x = element_text(hjust=.5),
      axis.text.y = element_text(hjust=.5),
      axis.text.y.right = element_blank(),
      axis.text.y.left = element_text(margin = margin(r=1)),
      axis.text.x.bottom = element_text(margin = margin(t=2)),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = "#dedede", linewidth = 0.2),
      axis.line.y = element_line(color = "#000000", linewidth = 0.1),
     )
}


singlePyramid <- function(myAGS) {

  tmp <- df |> 
    filter(AGS==myAGS,
           X3_variable_attribute_label != "Insgesamt") |> 
    select(AGS,Name,insgesamt,seniorenAnteil,X2_variable_attribute_label,X3_variable_attribute_code,X3_variable_attribute_label,value) |> 
    pivot_wider(names_from = c(X2_variable_attribute_label),
                values_from = value) |> 
    mutate(X3_variable_attribute_code = recode(X3_variable_attribute_code,
                                                "ALTERU01" = "ALTER000")) |> 
    arrange(X3_variable_attribute_code)
  
  tmp$Alter <- seq(0,100,1)
  myTotal <- tmp$insgesamt[1]
  
  left_plot  <- 
    pyramid_half(tmp, "left",  Alter, Männlich, myTotal)
  
  right_plot <- 
    pyramid_half(tmp, "right", Alter, Weiblich, myTotal)
  
  (left_plot | right_plot)
}


plots <- map(ags_senioren_rank$AGS, singlePyramid)

combined <- 
  wrap_plots(
  plots,
  nrow = 4,
  ncol = 4,
  byrow = TRUE
) +
  plot_annotation(
    title = "Age structure of German States (May 2022) sorted by retirement age (65+)",
    subtitle = "Persons (in percent) ",
    caption = "Census database table 1000A-2027",
    theme = theme(
      plot.title = element_text(size = 9, face = "bold"),  # overall title
      plot.subtitle = element_text(size = 8, face = "plain"),
      plot.caption = element_text(size = 6, hjust = 0)
  ))
  
combined

