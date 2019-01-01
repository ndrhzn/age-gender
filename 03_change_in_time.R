library(dplyr)
library(ggplot2)
library(ggalt)
library(scales)

### data ----------------------------------------------------------------------------------------

df <- read.csv('population_by_age_sex_year.csv', stringsAsFactors = F)

df <- df %>% 
  group_by(year) %>% 
  summarise(men_sum = sum(men, na.rm = T), 
            women_sum = sum(women, na.rm = T))

annotation <- data.frame(
  x = 2014.5,
  y = 28800000,
  label = 'Починаючи з 2015 року, дані наводяться без врахування тимчасово окупованої території АР Крим та міста Севастополь'
)

legend <- data.frame(
  x = c(1991, 1991),
  y = c(23000000, 28500000),
  label = c('кількість чоловіків', 'кількість жінок')
)



### plot -----------------------------------------------------------------------------------------

png(filename = '03_change_in_time.png', width = 1000, height = 650)

ggplot(df)+
  geom_ribbon(aes(x = year, ymin = men_sum, ymax = women_sum),
              stat = "stepribbon",
              fill = '#d9d9d9', alpha = 0.3)+
  geom_step(aes(x = year, y = women_sum), color = '#de77ae', size = 1)+
  geom_step(aes(x = year, y = men_sum), color = '#80cdc1', size = 1)+
  geom_point(data = df %>% filter(year %in% c(1989, 2018)),
             aes(x = year, y = women_sum), color = '#de77ae', size = 2.5)+
  geom_point(data = df %>% filter(year %in% c(1989, 2018)),
             aes(x = year, y = men_sum), color = '#80cdc1', size = 2.5)+
  geom_text(data = df %>% filter(year == 1989), 
            aes(x = year, y = men_sum, label = format(men_sum/1000000, nsmall = 1, digits = 1)), 
            color = '#80cdc1', family = 'Ubuntu Condensed', nudge_x = -0.6, size = 4.5)+
  geom_text(data = df %>% filter(year %in% c(1993, 2014, 2015)), 
            aes(x = year + 0.5, y = men_sum, label = format(men_sum/1000000, nsmall = 1, digits = 1)), 
            color = '#80cdc1', family = 'Ubuntu Condensed', nudge_y = 250000, size = 4.5)+
  geom_text(data = df %>% filter(year == 2018),
            aes(x = year, y = men_sum, label = format(men_sum/1000000, nsmall = 1, digits = 1)),
            color = '#80cdc1', family = 'Ubuntu Condensed', nudge_x = 0.6, size = 4.5)+
  geom_text(data = df %>% filter(year == 1989), 
            aes(x = year, y = women_sum, label = format(women_sum/1000000, nsmall = 1, digits = 1)), 
            color = '#de77ae', family = 'Ubuntu Condensed', nudge_x = -0.6, size = 4.5)+
  geom_label(data = df %>% filter(year %in% c(1993, 2014, 2015)), 
             aes(x = year + 0.5, y = women_sum, label = format(women_sum/1000000, nsmall = 1, digits = 1)), 
            color = '#de77ae', family = 'Ubuntu Condensed', 
            nudge_y = 250000, size = 4.5, fill = '#F3F7F7', label.size = NA, label.padding = unit(0, 'lines'))+
  geom_text(data = df %>% filter(year == 2018),
            aes(x = year, y = women_sum, label = format(women_sum/1000000, nsmall = 1, digits = 1)),
            color = '#de77ae', family = 'Ubuntu Condensed', nudge_x = 0.6, size = 4.5)+
  geom_label(data = legend, aes(x = x, y = y, label = stringr::str_wrap(label, 10)),
             color = c('#80cdc1', '#de77ae'), fill = '#F3F7F7', label.size = NA,
            family = 'Ubuntu Condensed', size = 4.5, lineheight = 0.9)+
  geom_label(data = annotation,
            aes(x = x, y = y, label = stringr::str_wrap(label, 20)),
            family = 'Ubuntu Condensed', size = 4.5, color = '#5D646F', fill = '#F3F7F7',
            hjust = 0, vjust = 1, label.size = NA)+
  scale_x_continuous(name = NULL, 
                     breaks = c(1989, df[df$men_sum == max(df$men_sum),]$year, 2015, 2018), 
                     minor_breaks = NULL)+
  scale_y_continuous(name = NULL, limits = c(19000000, 29000000),
                     breaks = seq(19000000, 29000000, 1000000),
                     labels = c(19, '', 21, '', 23, '', 25, '', 27, '', '29 млн'))+
  labs(title  = 'Жінок на 3.1 млн більше, ніж чоловіків',
       subtitle = 'Графік демонструє співвідношення кількості жінок та чоловіків у 1989-2018 роках',
       caption = 'Дані: Державна служба статистики України | Візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, color = '#5D646F'),
        axis.text.y = element_text(hjust = 0, vjust = -0.3, margin = margin(r = -40)),
        axis.text.x = element_text(hjust = -0.2, margin = margin(t = -15)),
        panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold', size = 40, margin = margin(b = 10, t = 20)),
        plot.subtitle = element_text(size = 20, face = 'plain', margin = margin(b = 30)),
        plot.caption = element_text(size = 14, margin = margin(t = 20, b = 10), color = '#5D646F'),
        plot.background = element_rect(fill = '#F3F7F7'),
        plot.margin = unit(c(1, 1.5, 1, 1.5), 'cm'))

dev.off()

rm(df, annotation, legend)