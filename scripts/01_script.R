library(pacman)

p_load(usethis, unibeCols, gitcreds, tidyverse, lubridate, medicaldata, cowplot, here, psych, RColorBrewer)

#BaseR "read.csv"
ins <- read.csv("https://raw.githubusercontent.com/ISPMBern/projects-in-R/main/data/raw/insurance_with_date.csv")

#tidyverse "read_csv"
ins2 <- read_csv("https://raw.githubusercontent.com/ISPMBern/projects-in-R/main/data/raw/insurance_with_date.csv")

ins2 %>% 
  select(sex) %>% 
  mutate(sex == as.factor("male", "female", "non-binary"))

ins3 <- ins2 %>% 
  mutate(sex = fct(sex),
         region = fct(region),
         smoker = case_when(X == 1 ~ NA,
                            TRUE ~ as.character("smoker")),
         children_logi = children > 2,
         smokes_yn = smoker == "no",
         date = date+months(6))


covdat <- read_csv("https://raw.githubusercontent.com/ISPMBern/projects-in-R/main/data/raw/COVID19Cases_geoRegion.csv")

covid_cantons_2020 <- covdat %>% 
  filter(datum <= ymd("2020-06-30")
         & (geoRegion == "ZH" | geoRegion == "BE" | geoRegion == "VD"))


ebodat <- read_csv("https://raw.githubusercontent.com/ISPMBern/projects-in-R/main/data/raw/ebola.csv")

ebodat2 <-  ebodat %>% 
  select(Country, Date, Cum_conf_cases) %>% 
  filter(Country %in% c("Guinea", "Liberia", "Sierra Leone")) %>% 
  filter(Date < "2015-03-31") %>% 
  group_by(Country) %>% 
  arrange(Country, Date) %>% 
  mutate(Cum_conf_cases_diff = Cum_conf_cases - lag(Cum_conf_cases)) %>% 
  ungroup()
  
ggplot(ebodat2, mapping = aes(x = Date, y = Cum_conf_cases_diff)) +
  geom_line(aes(group = Country, color = Country)) +
  labs(x = "Datum", y = "New Cases")+
  theme_classic()

ggplot(ebodat2, mapping = aes(x = Date, y = Cum_conf_cases_diff)) +
  geom_point(aes(group = Country, color = Country)) +
  labs(x = "Datum", y = "New Cases")+
  theme_classic()
  
ggplot(ebodat2, mapping = aes(x = Date, y = Cum_conf_cases_diff)) +
  geom_col(aes(group = Country, fill = Country), position = "dodge", width = 3) +
  labs(x = "Datum", y = "New Cases") +
  theme_classic()

ggplot(ebodat2, mapping = aes(x = Date, y = Cum_conf_cases)) +
  geom_line(aes(group = Country, color = Country)) +
  labs(x = "Datum", y = "New Cases")+
  theme_classic()

ggplot(ebodat2, mapping = aes(x = Date, y = Cum_conf_cases, group = Country, color = Country)) +
  geom_point(stroke = 5,
                 size = 10,
                 alpha = 0.5,
                 ) +
  geom_line(color = "pink", linewidth = 1) +
  ggtitle(label = "Confirmed covid cases") +
  labs(x = "Datum", y = "Cum. # of confirmed cases")+
  theme_classic() +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c('#fee8c8','#fdbb84','#e34a33'),
                    labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c('#fee8c8','#fdbb84','#e34a33'),
                      labels = c("Guinea", "Liberia", "Sierra Leone"))

ggplot(ebodat2, mapping = aes(x = Date, y = Cum_conf_cases)) +
  geom_col(aes(group = Country, fill = Country), position = "dodge", width = 3) +
  labs(x = "Datum", y = "New Cases") +
  theme_classic()


ins2

ggplot(ins2, aes(x = bmi, group = sex))+
  geom_density(aes(fill = sex), alpha = 0.5)
  
ggplot(ins2, aes(x = charges, y = after_stat(density), group = sex))+
  geom_histogram(aes(x = charges, color = sex), binwidth = 600) +
  geom_density(aes(x = charges, colour = sex), linewidth = 1.5) +
  geom_vline(aes(xintercept = median(charges)))

ggplot(ins2, aes(x = age, y = bmi, color = smoker))+
  geom_point() +
  geom_quantile()+
  theme(legend.position = "top")

