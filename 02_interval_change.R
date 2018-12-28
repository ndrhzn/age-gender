library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv('population_by_age_sex_year.csv', stringsAsFactors = F)

# transform data --------------------------------------------------------------------------------------------

df <- df %>% 
  filter(year %in% c(max(year), min(year)),
         !age %in% c('Вік невідомий', '80 і старше')) %>% 
  mutate(age = factor(age, ordered = T, levels = unique(age)),
         population = men + women) %>% 
  select(-men, -women) %>% 
  spread(key = year, value = population) %>% 
  rename(y1989 = `1989`, y2018 = `2018`)


# generate data for the legend -----------------------------------------------------------------------------

legend <- data.frame(
  x = c(72.5, 73.5),
  ymin = c(760000, 850000),
  ymax = c(850000, 760000),
  type = c(TRUE, FALSE)
)

legend_text <- data.frame(
  x = c(72, 74),
  y = c(805000),
  label = c('скорочення населення', 'приріст населення')
)

legend_text_year <- data.frame(
  x = c(72, 72, 74, 74),
  y = c(860000, 750000, 860000, 750000),
  label = c('1989 рік\n', '\n2018 рік', '2018 рік\n', '\n1989 рік')
)

# plot ----------------------------------------------------------------------------------------------------

png(filename = 'interval_change.png', width = 1000, height =650)

ggplot(df)+
  geom_segment(aes(x = as.numeric(age) - 1, y = y1989, xend = as.numeric(age) - 1, yend = y2018, 
                     color = y1989 > y2018), size = 0.6, 
               arrow = arrow(angle = 17.5, length = unit(0.5, 'lines')))+
  geom_rect(aes(xmin = 66.5, xmax = 79.5, ymin = 710000, ymax = 895000), 
            fill = '#F3F7F7', color = '#5D646F', size = 0.15)+
  geom_segment(data = legend, 
                 aes(x = x, y = ymax, xend = x, yend = ymin, color = type), size = 0.6,
               arrow = arrow(angle = 17.5, length = unit(0.5, 'lines')))+
  geom_text(data = legend_text, 
            aes(x = x, y = y, label = stringr::str_wrap(label, 15)), 
            family = 'Ubuntu Condensed', size = 4, color = '#5D646F', 
            hjust = c('right', 'left'), lineheight = 1)+
  geom_text(data = legend_text_year, 
            aes(x = x, y = y, label = stringr::str_wrap(label, 15)), 
            lineheight = 1,
            hjust = c('right', 'right', 'left', 'left'), 
            vjust = c('top', 'bottom', 'top', 'bottom'),
            family = 'Ubuntu Condensed', size = 4, color = '#5D646F')+
  scale_color_manual(values = c('#4575b4', '#d73027'))+
  scale_x_continuous(breaks = c(0, 41, 46, 54, 57, 66, 71, 77, 79), 
                     labels = c('          0 років', 41, 46, 54, 57, 66, 71, 77, 79),
                     expand = c(0.05, 0.02))+
  scale_y_continuous(limits = c(200000, 920000), expand = c(0.01, 0.01),
                     breaks = seq(200000, 900000, 100000),
                     labels = c(seq(200, 800, 100),'900 тис'))+
  labs(title  = 'В більшості вікових груп населення скоротилось',
       subtitle = stringr::str_wrap('Початок кожного сегменту позначає кількість населення у певній віковій групі у 1989 році. Кінець сегменту позначає кількість населення у цій віковій групі у 2018 році. Довжина сегменту пропорційна приросту/скороченню населення у цій віковій групі між 1989 та 2018 роками', 122),
       caption = 'Дані: Державна служба статистики України | Візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, color = '#5D646F'),
        axis.text.y = element_text(hjust = 0, vjust = -0.3, margin = margin(r = -40)),
        legend.position = 'none',
        panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold', size = 40, margin = margin(b = 10, t = 20)),
        plot.subtitle = element_text(size = 18, face = 'plain', margin = margin(b = 10), color = '#5D646F'),
        plot.caption = element_text(size = 14, margin = margin(t = 20, b = 10), color = '#5D646F'),
        plot.background = element_rect(fill = '#F3F7F7'),
        plot.margin = unit(c(1, 1.5, 1, 1.5), 'cm'))

dev.off()
