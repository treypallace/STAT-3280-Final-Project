library(tidyverse)
library(patchwork)
library(geojsonio)
library(rnaturalearth)
library(ggrepel)
library(cowplot)
library(magick)
library(ggtext)
library(showtext)
library(sf)
library(ggpointdensity)
library(scales)
library(broom)


############################### Plot 1 ############################### 


pal <- c("#E4572E", "#17BEBB", "#FFC914", "#2E282A", "#76B041") # DNK, FIN, ISL, NOR, SWE

f1 <- "DIN Condensed"
f2 <- "Futura"

theme_set(theme_minimal(base_family = f1))

theme_update(
  legend.position = "none",
  plot.background = element_rect(fill = "#fbf7f5", color = NA),
  strip.text = element_text(size = 12, color = "grey15", hjust = 0, vjust = 0)
)

rent<-read.csv("https://raw.githubusercontent.com/treypallace/STAT-3280-Final-Project/main/medianrent.csv")
rent$date<-lubridate::dmy(rent$date)
rent$date<-format(as.Date(rent$date, format="%d/%m/%Y"),"%Y")

rent <- rent %>% group_by(date,type) %>% 
  summarise(price=mean(price),
            .groups = 'drop') %>%
  as.data.frame()

extremes <- rent %>%                                       # Get min by group
  group_by(type) %>%
  mutate(date=date)%>%
  summarise_at(vars(price),
               list(min = min, max= max))

years<-c(2014,2023,2014,2023,2014,2023,2014,2023,2015,2023)

long<-extremes %>%
  pivot_longer(!type, names_to = "value", values_to = "price")

long<-cbind(long, years)

p1 <- ggplot(rent) +
  geom_textline(aes(x = date, y = price, group = type, label = type, color = type), size = 3, family = f1, linewidth = 0.75) +
  scale_color_manual(values = pal)+
  labs(x = "Year", y= "Median Rent")


p1

myrent<-read.csv("https://raw.githubusercontent.com/treypallace/STAT-3280-Final-Project/main/myrent.csv")
myrent$date<-as.Date(myrent$date, format = "%Y-%m-%d")

p2<-ggplot(myrent)+
  geom_textline(aes(x = date, y = total, label = date), size = 3, family = f1, linewidth = 0.75) +
  scale_color_manual(values = pal) +
  scale_x_date(date_breaks = "months" , date_labels = "%m-%y")+
  labs(x = "Year", y= "Rent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



rent_plot<-p1+
  p2 +
  plot_annotation(
    tag_levels = "A",
    title = "Rising Rent",
    subtitle = "A: Charlottesville Median Rent has been Steadily Increasing \nB: My Rent for One Room in a 4 Bedroom Apartment\n(New Lease Signed August 2022) ",
    caption = "Source: Zumper.com",
    theme = theme(
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(margin = margin(10, 0, 0, 0)),
      plot.margin = margin(20, 20, 10, 20)
    )
  )

rent_plot

######################################################################



############################### Plot 2 ###############################


tuition<-read.csv("https://raw.githubusercontent.com/treypallace/STAT-3280-Final-Project/main/tuition.csv")

theme_update(
  legend.position = "none",
  plot.background = element_rect(fill = "#fbf7f5", color = NA),
  axis.title = element_blank(),
  strip.text = element_text(size = 12, color = "grey15", hjust = 0, vjust = 0)
)

p3base<-ggplot(tuition, aes(x=year, y=total)) +
  geom_bar(aes(fill=factor(state)
               ), 
           position=position_dodge(width=0.8), 
           stat="identity") +
  facet_wrap(~career, scales="free") +
  scale_fill_manual(values=c("#E4572E", "#76B041"),
                    name="State") +
  scale_x_discrete(limits= c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))+
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=2),
    strip.text = element_text(face = "bold", margin = margin(3, 0, 0, 0)),
    plot.background = element_rect(fill = "#fbf7f5", color = NA),
    plot.margin = margin(10, 10, 10, 20),
    legend.position = "bottom", 
    legend.justification = "right",
    legend.direction = "horizontal",
    legend.title=element_blank()
  )


annot <- tribble(
  ~x, ~y, ~label, ~size, ~face, ~color,
  0,  1, "A Decade of ", 10, "bold", "#17BEBB",
  0,  10, "The height of the bars shows the total tuition including required fees.", 4, "plain", "black"
) %>% 
  ggplot() +
  geom_text(aes(x, y, label = str_wrap(label, 50), fontface = face, size = size, color = color), hjust = 1, vjust = 1, family = f2) +
  scale_size_identity() +
  scale_color_identity() +
  scale_x_continuous(limits = c(-1, 0)) +
  scale_y_reverse(limits = c(5, 1)) +
  theme_void()

annot2 <- tribble(
  ~x, ~y, ~label, ~size, ~face, ~color,
  0,  1, "UVA Tuition ", 10, "bold", "#17BEBB",
  0,  10, "The height of the bars shows the total tuition including required fees.", 4, "plain", "black"
) %>% 
  ggplot() +
  geom_text(aes(x, y, label = str_wrap(label, 50), fontface = face, size = size, color = color), hjust = 1, vjust = 1, family = f2) +
  scale_size_identity() +
  scale_color_identity() +
  scale_x_continuous(limits = c(-1, 0)) +
  scale_y_reverse(limits = c(5, 1)) +
  theme_void()


annot3 <- tribble(
  ~x, ~y, ~label, ~size, ~face, ~color,
  0,  1, "There has been a steady ", 5, "bold", "#2E282A"
) %>% 
  ggplot() +
  geom_text(aes(x, y, label = str_wrap(label, 50), fontface = face, size = size, color = color), hjust = 1, vjust = 1, family = f2) +
  scale_size_identity() +
  scale_color_identity() +
  scale_x_continuous(limits = c(-1, 0)) +
  scale_y_reverse(limits = c(5, 1)) +
  theme_void()

annot4 <- tribble(
  ~x, ~y, ~label, ~size, ~face, ~color,
  0,  1, "rise in tuition since 2013.", 5, "bold", "#2E282A"
) %>% 
  ggplot() +
  geom_text(aes(x, y, label = str_wrap(label, 50), fontface = face, size = size, color = color), hjust = 1, vjust = 1, family = f2) +
  scale_size_identity() +
  scale_color_identity() +
  scale_x_continuous(limits = c(-1, 0)) +
  scale_y_reverse(limits = c(5, 1)) +
  theme_void()

annot5 <- tribble(
  ~x, ~y, ~label, ~size, ~face, ~color,
  0,  1, "Source: UVA Instituional Reserach and Analytics", 3, "bold", "#2E282A"
) %>% 
  ggplot() +
  geom_text(aes(x, y, label = str_wrap(label, 50), fontface = face, size = size, color = color), hjust = 1, vjust = 1, family = f2) +
  scale_size_identity() +
  scale_color_identity() +
  scale_x_continuous(limits = c(-1, 0)) +
  scale_y_reverse(limits = c(5, 1)) +
  theme_void()

p3<-p3base + 
  inset_element(annot, 0.4, -5, 1.00 , 0.7)+
  inset_element(annot2, 0.4, -6.5, 0.985 , 0.7)+
  inset_element(annot3, 0.4, -7.8, 1 , 0.7)+
  inset_element(annot4, 0.4, -8.5, 1 , 0.7)+
  inset_element(annot5, -14, -16.5, 1 , 0.7)

p3

######################## Plot 3 ##################################

  
  
hpi_master<-read.csv("https://raw.githubusercontent.com/treypallace/STAT-3280-Final-Project/main/HPI_master.csv")

# clean the data
hpi_va<-hpi_master %>%
  filter(str_detect(place_name, "VA"))

hpi_va_sub <- hpi_va[hpi_va$yr %in% c(2012, 2017, 2022), ]
hpi<-hpi_va_sub[hpi_va_sub$period ==4,]
hpi<-hpi[hpi$hpi_flavor =="all-transactions",]

hpi$place_name <- sapply(strsplit(as.character(hpi$place_name), ","), function(x) x[1])
hpi$place_name <- reorder(hpi$place_name, -hpi$index_nsa)
hpi$place_name <- gsub("Virginia Beach-Norfolk-Newport News", "VA Beach", hpi$place_name)
hpi$place_name <- gsub("Washington-Arlington-Alexandria", "DC", hpi$place_name)
hpi$place_name <- gsub("Blacksburg-Christiansburg", "Blacksburg", hpi$place_name)
hpi$place_name <- gsub("Kingsport-Bristol", "Kingsport", hpi$place_name)

hpi <- hpi %>%
  mutate(cond = case_when(
    place_name =="Charlottesville" ~ '#17BEBB',
    place_name =="DC" ~ '#FFC914',
    TRUE ~ '#E4572E'   #anything that does not meet the criteria above
  ))


my_colors <- c("#FFC914", "#E4572E", "#17BEBB", "#6E5EB5", "#1A4F63")


p4<-ggplot(hpi, aes(x = place_name, y = index_nsa)) +
  geom_bar(stat = "identity", aes(fill = cond)) +
  labs(title = "Housing Price Index in Virginia Cities", subtitle = "Charlottesville has a Comparable Pirce Index to DC", 
       caption = "Source: Federal Housing Finance Agency") +
  facet_wrap(vars(yr), ncol = 1, strip.position = "left")+
  geom_text(aes(y = 0.1, label = index_nsa),vjust = -0.3, size = 6, family = f1, color ="#fbf7f5") + 
  geom_text(aes(label= place_name), stat = 'summary', fun.y = sum, angle = 45, hjust = -.05, vjust =-0.1 ,size = 3.8,
            family = f1, color ="#2E282A") + 
  scale_y_continuous(limits = c(0, 575), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 500), expand = c(0, 0)) + 
  scale_fill_manual(values = my_colors) +
  theme_minimal(base_family = f2, base_size = 12) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fbf7f5", color = NA),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "#E4572E"),
    strip.placement = "outside",
    strip.text = element_text(margin = margin(0, 10, 0, 0), face = "bold", size = 14, color = "#2E282A"),
    panel.spacing = unit(2, "lines"),
    plot.caption = element_text(size = 8, color = "#2E282A")
  )


p4

########################################################################



########################## Plot 4 ######################################

income<-read.csv("https://raw.githubusercontent.com/treypallace/STAT-3280-Final-Project/main/income_distribution.csv")
incomemed<-c("$25k-35k", "$50k-75k")
income_med<-data.frame(matrix(incomemed, nrow = 1, ncol = 2))
colnames(income_med)<-c("median_bracket_2000", "median_bracket_2020")

income$income_bracket<- c("Under $10K" , "$10K-15K", "$15K-25K" , "$25K-35K","$35-50K" ,"$50K-75K" ,"$75K-100K" , "$100K-150K" , "$150K-200k" , "Over $200k" )
income$income_bracket<-factor(income$income_bracket, levels =c("Under $10K" , "$10K-15K", "$15K-25K" , "$25K-35K","$35-50K" ,"$50K-75K" ,"$75K-100K" , "$100K-150K" , "$150K-200k" , "Over $200k" ))
income<-income %>% 
  mutate(change = year_2020-year_2000)


p5<-ggplot(data = income) +
  geom_segment(aes(x = year_2000, y = factor(income_bracket),
                   xend = year_2020, yend = income_bracket,
                   color = if_else(change > 0, "#76B041", "#E4572E")),
               arrow = arrow(length = unit(0.04, "npc")), size = 1) +
  geom_text(aes(x = year_2000, y = income_bracket,
                label = if_else(abs(change) > 4, year_2000, NULL)), nudge_y = -0.275, size = 3, family = f1, color = "grey10") +
  geom_text(aes(x = year_2020, y = income_bracket,
                label = year_2020, color = if_else(change > 0, "#76B041", "#E4572E")),
            nudge_y = -0.275, size = 3, family = f2) +
  scale_color_identity() +
  scale_y_discrete(limits = c("Under $10K" , "$10K-15K", "$15K-25K" , "$25K-35K","$35-50K" ,"$50K-75K" ,"$75K-100K" , "$100K-150K" , "$150K-200k" , "Over $200k" )) +
  labs(
    title = "Changes in Charlottesville's Income Distribution",
    subtitle = str_wrap("The arrows show the change between 1990 and 2020. There has been an decrease in the percentage of households in lower income brackets while there has been an increase in the percentage of households in upper income brackets.", 114),
    caption = "Source: US Census",
    x = "Percentage",
    y = NULL
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fbf7f5", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_line(color = "#b9babd", size = 0.15),
    panel.grid.minor.x = element_line(color = "#b9babd", size = 0.08),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.text.y = element_text(family = f2),
    strip.text = element_text(family = f2, size = 12),
    panel.spacing.x = unit(2, "lines"),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0), family = f2),
    plot.caption = element_text(margin = margin(15, 0, 0, 0), size = 8)
  ) 

p5

## code adapted from https://github.com/gkaramanis/tidytuesday/tree/master/2021/2021-week6
#########################################################################


########################## Plot 5 ######################################
spdf <- geojson_read("https://raw.githubusercontent.com/treypallace/STAT-3280-Final-Project/main/charlottesville-sc_.geojson",  what = "sp")


spdf_fortified <- tidy(spdf)

spdf_fortified$id<-as.numeric(spdf_fortified$id)

cvill_tracts<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
counts<-c(247, 499, 499, 323, 263, 263, 833, 525, 323, 596, 629, 263, 169, 169, 105, 247, 499, 342, 499)
energy<-data.frame(cvill_tracts, counts)

# Make the merge
spdf_fortified <- spdf_fortified %>%
  left_join(. , energy, by=c("id"="cvill_tracts"))

spdf_fortified$counts[ is.na(spdf_fortified$counts)] = 0.001


p6<-ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = counts, x = long, y = lat, group = group)) +
  scale_fill_gradient(low = "#FFC914", high = "#E4572E", name = "Counts") +
  labs(
    title = "Number of Households with High Energy Burden in Charlottesville",
    subtitle = "High counts suggst neighborhoods that are most at risk of bearing unsustainable energy costs",
    caption = "Source: Community Climate Collaborative"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#fbf7f5", color = NA),
    panel.background = element_rect(fill = "#fbf7f5", color = NA),
    legend.background = element_rect(fill = "#fbf7f5", color = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(size= 22, hjust=0.01, color = "#2E282A", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#2E282A", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text(size=12, color = "#2E282A", margin = margin(t = 0.5, r=0.5, unit = "cm"))
  ) +
  coord_map()

p6








