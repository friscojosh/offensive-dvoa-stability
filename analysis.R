### -----------------------------------------------------------------------------------------------
### Testing the year to year stability of Offensive DVOA.
### Data scraped from https://www.footballoutsiders.com/stats/teamoff
### -----------------------------------------------------------------------------------------------

library(tidyverse)
library(broom)

dvoa <- read_csv("data/off_dvoa_09-18.csv")

dvoa_year2 <- dvoa %>%
   mutate(season2 = season + 1)

dvoa_joined <-dvoa_year2 %>%
   inner_join(dvoa_year2, by = c("season2" = "season", "team"))

colnames(dvoa_joined)

### ------ Models go here ---------------------------------------------------------------------////

### Total Defensive DVOA first --------------------------------------------------------------------

total_dvoa_model <- lm(data = dvoa_joined, offenseive_dvoa.y ~ offenseive_dvoa.x)
summary(total_dvoa_model)

total_dvoa_stability <- glance(total_dvoa_model) %>%
   mutate(metric = "Total Offensive DVOA",
          r.squared = round(r.squared, 3)) %>%
   select(metric, r.squared)

### This will go in the README as a visual ---------------------------------------------------------

ggplot(data = dvoa_joined, aes(x = offenseive_dvoa.x, y = offenseive_dvoa.y)) +
   geom_point() +
   geom_smooth(method = "lm") +
   theme_bw() +
   labs(x = "Year Y Total Offensive DVOA",
        y = "Year Y + 1 Total Offensive DVOA",
        title = "Total Offensive DVOA is Reasonably Stable Year-to-Year",
        subtitle = paste("r-squared:", total_dvoa_stability$r.squared),
        caption = "Source: Football Outsiders. https://www.footballoutsiders.com/stats/teamoff")

### magic incantation to save the plot to disk -----------------------------------------------------

dev.copy(png,'total_offensive_dvoa.png')
dev.off()

### Pass DVOA -------------------------------------------------------------------------------------

pass_dvoa_model <- lm(data = dvoa_joined, pass_dvoa.y ~ pass_dvoa.x)
summary(pass_dvoa_model)

pass_dvoa_stability <- glance(pass_dvoa_model) %>%
   mutate(metric = "Offensive Passing DVOA",
          r.squared = round(r.squared, 3)) %>%
   select(metric, r.squared)

### Rush DVOA -------------------------------------------------------------------------------------

rush_dvoa_model <- lm(data = dvoa_joined, rush_dvoa.y ~ rush_dvoa.x)
summary(rush_dvoa_model)

rush_dvoa_stability <- glance(rush_dvoa_model) %>%
   mutate(metric = "Offensive Rushing DVOA",
          r.squared = round(r.squared, 3)) %>%
   select(metric, r.squared)

### Bind up the results with some twine. ---------------------------------------------------------

defensive_dvoa_results <- total_dvoa_stability %>%
   bind_rows(pass_dvoa_stability,
             rush_dvoa_stability)

write_csv(defensive_dvoa_results, "results.csv")
