library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

online <- dat[x == 'online', ]
inclass <- dat[x == 'inclass', ]

mean(online$sex == 'Female')
mean(inclass$sex == 'Female')

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# Criar modelo a partir do tipo de aula:
y_hat <- ifelse(dat$type == 'online', "Male", "Female") %>%
  factor(levels = c("Female", "Male"))

# Accuracy:
mean(y_hat == dat$sex)

table(y_hat, y)

sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)

confusionMatrix(data = y_hat, reference = y)
