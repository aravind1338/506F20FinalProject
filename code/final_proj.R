## Stats 506 F20
## Stats 506 F20 Final Project
##
## Answering the question "Do people in the US get their macronutrient 
## intake primarily at home or from elsewhere?
##
## Author(s): Aravind Mantravadi, amantrav@umich.edu
## Updated: December 9th, 2020

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
  rename(id = seqn, weight_3 = wtdrd1, weight_4 = wtdr2d, 
         est_proteins = dr1iprot, est_carbohydrates = dr1icarb, 
         est_sugars = dr1isugr, est_fibers = dr1ifibe, 
         est_fats = dr1itfat, Source = dr1fs) %>% 
  mutate(Source = if_else(Source == 1 | Source == 10 | Source == 11 
                          | Source == 12 | Source == 18 
                          | Source == 19 | Source == 20, 
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

means = svyby(~est_proteins + est_carbohydrates + est_sugars + 
                est_fibers + est_fats, by = ~Source, des, svymean)


statistic = qnorm(0.975)
# Create the confidence interval columns for each macronutrient

means = means %>% 
  mutate(l.p = est_proteins - se.est_proteins*statistic,
         u.p = est_proteins + se.est_proteins*statistic,
         l.c = est_carbohydrates - se.est_carbohydrates*statistic,
         u.c = est_carbohydrates + se.est_carbohydrates*statistic, 
         l.s = est_sugars - se.est_sugars*statistic, 
         u.s = est_sugars + se.est_sugars*statistic, 
         l.f = est_fibers - se.est_fibers*statistic, 
         u.f = est_fibers + se.est_fibers*statistic, 
         l.fa = est_fats - se.est_fats*statistic, 
         u.fa = est_fats + se.est_fats*statistic,
         Proteins = sprintf('%4.1f (%4.1f, %4.1f)', est_proteins, l.p, u.p), 
         Carbohydrates = sprintf('%4.1f (%4.1f, %4.1f)', 
                                 est_carbohydrates, l.c, u.c), 
         Sugars = sprintf('%4.1f (%4.1f, %4.1f)', est_sugars, l.s, u.s), 
         Fibers = sprintf('%4.1f (%4.1f, %4.1f)', est_fibers, l.f, u.f), 
         Fats = sprintf('%4.1f (%4.1f, %4.1f)', est_fats, l.fa, u.fa)) %>%
  select(Source, Proteins, Carbohydrates, Sugars, Fibers, Fats)



#means %>%
#  ggplot(aes_string(x = "proteins", y = "group", color = "source")) +
#  geom_point(position = position_dodge(width = 0.4)) +
#  geom_errorbarh(aes_string(xmin = "lower.proteins", xmax = "upper.proteins"),
#                 position = position_dodge(width = 0.4)) +
#  labs(x = "proteins", y = "") + 
#  scale_color_manual(values = c("limegreen", "skyblue"))




