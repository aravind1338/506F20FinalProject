## Stats 506 F20
## Stats 506 F20 Final Project
##
## Answering the question "Do people in the US get their macronutrient 
## intake primarily at home or from elsewhere?
##
## Author(s): Aravind Mantravadi, amantrav@umich.edu
## Updated: December 5th, 2020

# libraries: ------------------------------------------------------------------

library(tidyverse)
library(survey)

# data: -----------------------------------------------------------------------
demographics = read.csv('../data/nhanes_demo.csv')
nutrition = read.csv('../data/nutrition.csv')

# keep only the columns needed for analysis

demographics = 
  demographics %>% drop_na() %>% select(SEQN, WTINT2YR, WTMEC2YR) %>%
  rename(id = SEQN, weight_1 = WTINT2YR, weight_2 = WTMEC2YR)


#home_cooked = 1, 10, 11, 12, 18, 19, 20 -- explain this in report
nutrition = 
  nutrition %>% drop_na() %>% select(seqn, wtdrd1, wtdr2d, dr1iprot, 
                       dr1icarb, dr1isugr, dr1ifibe, dr1itfat, dr1fs) %>%
  rename(id = seqn, weight_3 = wtdrd1, weight_4 = wtdr2d, proteins = dr1iprot,
         carbohydrates = dr1icarb, sugars = dr1isugr, fibers = dr1ifibe, 
         fats = dr1itfat, source = dr1fs) %>% 
  mutate(source = if_else(source == 1 | source == 10 | source == 11 
                          | source == 12 | source == 18 
                          | source == 19 | source == 20, 
                          "home-cooked", "outside", "missing"))


# merge on id
dataset = merge(nutrition, demographics, "id")

          ### ANALYSIS ###

  # STEPS
  # 1) Create the model using svydesign
  # 2) Find the means for each macronutrient after grouping by source using
  #    svymean
  # 3) Compute the lower and upper confidence intervals


des = svydesign(ids = ~0, weights = ~weight_1 + weight_2 + weight_3 + weight_4, 
                data = dataset)

means = svyby(~proteins + carbohydrates + sugars + fibers + fats, 
              by = ~source, des, svymean)


statistic = qnorm(0.975)
# Create the confidence interval columns for each macronutrient

means = means %>% mutate(lower.proteins = proteins - se.proteins*statistic,
                             upper.proteins = proteins + se.proteins*statistic, 
                         lower.carbohydrates = carbohydrates - se.carbohydrates*statistic, 
                         upper.carbohydrates = carbohydrates + se.carbohydrates*statistic,
                         lower.sugars = sugars - se.sugars*statistic, 
                         upper.sugars = sugars + se.sugars*statistic,
                         lower.fibers = fibers - se.fibers*statistic, 
                         upper.fibers = fibers + se.fibers*statistic,
                         lower.fats = fats - se.fats*statistic, 
                         upper.fats = fats + se.fats*statistic)


means %>%
  ggplot(aes_string(x = "proteins", y = "test", color = "source")) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbarh(aes_string(xmin = "lower.proteins", xmax = "upper.proteins"),
                 position = position_dodge(width = 0.4)) +
  labs(x = "proteins", y = "idk") + 
  scale_color_manual(values = c("limegreen", "skyblue"))






