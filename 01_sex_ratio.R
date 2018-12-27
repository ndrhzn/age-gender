library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv('population_by_age_sex_year.csv', stringsAsFactors = F)

# transform data --------------------------------------------------------------------------------------------

df <- df %>% 
  filter(year == max(year), age != 'Вік невідомий') %>% 
  mutate(age = factor(age, ordered = T, levels = unique(age)),
         ratio = women/men * 100,
         dominant = ifelse(ratio > 100, 'жінки', 'чоловіки'))

# add data for annotations and segments

annotations <- data.frame(
  x = c(69, 59, 31, 5),
  y = c(225, 177, 123, 120),
  label = c('Жінок вдвічі більше,\n ніж чоловіків', 
            'Жінок у півтора раза \nбільше, ніж чоловіків', 
            'Кількість чоловіків \nта жінок майже однакова',
            'Чоловіків трохи \nбільше, ніж жінок')
)

segments <- data.frame(
  x = c(69.5, 59, 31, 5),
  y = c(214, 165, 110, 110),
  xend = c(72, 64, 35, 7),
  yend = c(202, 152, 102, 102)
)

# plot

png(filename = '01_sex_ratio.png', width = 1000, height = 650)

ggplot(df)+
  geom_linerange(aes(x = as.numeric(age) - 1, ymin = 100, ymax = ratio, 
                     color = dominant), size = 1.75)+
  geom_hline(yintercept = 100, color = '#3A3F4A')+
  geom_text(data = annotations, aes(x = x, y = y, label = label),
            family = 'Ubuntu Condensed', color = '#5D646F', size = 4.5, lineheight = 1)+
  geom_curve(data = segments, aes(x = x, y = y, xend = xend, yend = yend), 
             curvature = 0.1, arrow = arrow(length = unit(5, 'pt')),
             color = '#5D646F')+
  scale_x_reverse(breaks = c(0, 36, 65, 73, 80), 
                  labels = c('0 років', 36, 65, 73, '80 і більше'))+
  scale_y_continuous(limits = c(80, 265), 
                     breaks = c(95, 100, 150, 200, 250), 
                     expand = c(0.01, 0.1))+
  scale_color_manual(values = c('#de77ae', '#80cdc1'))+
  coord_flip()+
  labs(title = 'До 35 років переважають чоловіки. Після — жінки',
       subtitle = 'Графік демонструє кількість жінок на 100 чоловіків у розрізі вікових груп',
       caption = 'Дані: Державна служба статистики України, 2018 рік | Візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, color = '#5D646F'),
        axis.text.y = element_text(hjust = 0, vjust = -0.3, margin = margin(r = -60)),
        legend.position = 'none',
        panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold', size = 40, margin = margin(b = 15, t = 20)),
        plot.subtitle = element_text(size = 18, face = 'plain', margin = margin(b = 10), color = '#5D646F'),
        plot.caption = element_text(size = 14, margin = margin(t = 20, b = 10), color = '#5D646F'),
        plot.background = element_rect(fill = '#F3F7F7'),
        plot.margin = unit(c(1, 1.5, 1, 1.5), 'cm'))

dev.off()

rm(df, annotations, segments)