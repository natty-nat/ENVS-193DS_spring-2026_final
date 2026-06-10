# ENVS 193DS Final
Natalie Parker

- [Link GitHub repo](#link-github-repo)
- [Set up](#set-up)
- [Problem 1](#problem-1)
  - [1a.](#1a)
  - [1b.](#1b)
  - [1c.](#1c)
- [Problem 2](#problem-2)
  - [2a.](#2a)
  - [2b.](#2b)
  - [2c.](#2c)
  - [2d.](#2d)
  - [2e.](#2e)
  - [2f.](#2f)
  - [2g.](#2g)
  - [2h.](#2h)
  - [2i.](#2i)
  - [2j.](#2j)
  - [2k.](#2k)
- [Problem 3](#problem-3)
  - [3a.](#3a)
  - [3b.](#3b)
  - [3c.](#3c)
  - [3d.](#3d)
  - [3e.](#3e)
  - [3f.](#3f)

# Link GitHub repo

https://github.com/natty-nat/ENVS-193DS_spring-2026_final

# Set up

``` r
library(tidyverse) # general use
library(janitor) # cleaning data frames
library(here) # file/folder organization
library(readxl) # reading .xlsx files
library(ggeffects)
library(DHARMa)
# storing nest data as object "nest"
nest <- read_csv(here("data", "nest_data_final.csv"))
personal_clean <- read_csv(
  here("data", "ENVS 193DS- Personal Data - Sheet1.csv"), 
  # skipping the first 2 messed up rows
  skip = 2) |> 
  # dropping the first empty column
  select(-1) |> 
  # cleaning the names
  clean_names() |> 
 # converting sleep to a graphable value
  mutate(
    sleep_hours = 
      hour(hms(amount_of_sleep)) + (minute(hms(amount_of_sleep)) / 60),
    # converting the column "satisfaction" to numbers
    satisfaction = as.numeric(satisfaction))
```

# Problem 1

## 1a.

In part 1 they used binary/ logistic regression.

In part 2 they used a Chi-Squared test for independence.

## 1b.

American Avocet microhabitat use is determined by the distance to the
water’s edge.

The shore birds American Avocet, Foster’s Tern, and Black-necked Stilt,
have preferences for certain habitats over others.

## 1c.

An additional variable that could effect American Avocet microhabitat
choice could be their prey collected by looking in the soils. This is a
continuous numeric variable (count/m<sup>2</sup>), and it might
influence habitat use because with limited prey the carrying capacity is
lower (less food), and vice versa.

# Problem 2

## 2a.

``` r
# creating new object called "nests_clean" from "nest" data frame
nests_clean <- nest |> 
  # selecting columns of interest
  select(case_control, height_cm, ant_species) |> 
  # filtering to only include specific ant species
  filter(ant_species %in% c("AB", "RRB", "BBR")) |> 
  # making new column for scientifc names and adjusting to names to be full
  mutate(scientific_name = case_when(
    ant_species == "AB" ~ "Crematogaster sjostedti",
    ant_species == "RRB" ~ "Crematogaster mimosae",
    ant_species == "BBR" ~ "Crematogaster nigriceps"
  )) |> 
  # reordering the levels of ant species
  mutate(scientific_name = factor(scientific_name, levels = c(
    "Crematogaster sjostedti", 
    "Crematogaster mimosae", 
    "Crematogaster nigriceps"
  )))
# displaying the structure of "nests_clean"
str(nests_clean)
```

    tibble [270 × 4] (S3: tbl_df/tbl/data.frame)
     $ case_control   : num [1:270] 1 0 0 0 0 1 0 0 0 1 ...
     $ height_cm      : num [1:270] 392 310 513 154 324 ...
     $ ant_species    : chr [1:270] "RRB" "RRB" "RRB" "RRB" ...
     $ scientific_name: Factor w/ 3 levels "Crematogaster sjostedti",..: 2 2 2 2 1 2 3 2 1 3 ...

``` r
# showing 10 random rows!
nests_clean |> 
  sample_n(10)
```

    # A tibble: 10 × 4
       case_control height_cm ant_species scientific_name        
              <dbl>     <dbl> <chr>       <fct>                  
     1            0      279  RRB         Crematogaster mimosae  
     2            0      158. RRB         Crematogaster mimosae  
     3            0      105  BBR         Crematogaster nigriceps
     4            0      192  RRB         Crematogaster mimosae  
     5            0      134. RRB         Crematogaster mimosae  
     6            0      142  RRB         Crematogaster mimosae  
     7            0      216  RRB         Crematogaster mimosae  
     8            0      114. AB          Crematogaster sjostedti
     9            0      427  RRB         Crematogaster mimosae  
    10            0      194  RRB         Crematogaster mimosae  

## 2b.

The 1 and 0 in the column “case_control” tell the reader whether or not
the tree contained a nest. The 1s are affermative for nest, where the 0s
show that there was no nest.

## 2c.

Ant species is a categorical variable with 3 levels. This could
influence the probability of nest occurrence depending on the type of
ant species, and how aggressive they might be to the nesting birds. I
found this information in the introduction, paragraph 3.

Tree height is a continuous numerical value measured in cm. Tree height
could influence the probability of nest occurrence because taller trees
are more likely to have nests. I found this information in the methods
section, paragraph 2.

## 2d.

| Model Number | Model Type | Formula | Model Description |
|----|----|----|----|
| Model 1 | Additive GLM | case_control ~ height_cm + scientific_name | Looking at the independent effect of tree height and ant species on nesting probability using logistic regression. |
| Model 2 | Interactive GLM | case_control ~ height_cm \* scientific_name | Looking at whether there is an effect of tree height on nesting probability changes with ant species present using logistic regression. |

Table of models

## 2e.

``` r
#running model 1: additive logistic regression
model_additive <- glm(case_control ~ height_cm + scientific_name, 
                      data = nests_clean,
                      family = binomial)
```

## 2f.

``` r
# Running IAC to check scores
AIC (model_additive, model_intereactive)
```

                       df      AIC
    model_additive      4 258.7430
    model_intereactive  6 237.8042

The best model as determined by Akaike’s Information Criterion (AIC) is
the interactive logistic regression model. This model has a lower AIC
(237.8 compared to 258.7), which tells us there are significant changes
to nesting depending on which ant species lives there in relation to
tree height.

## 2g.

``` r
# simulate residuals for interactive model
sim_residuals <- simulateResiduals(fittedModel = model_intereactive, n = 250)
# plotting diagnostics
plot(sim_residuals)
```

![](ENVS-193DS_Final_files/figure-commonmark/checking_diaganostics-1.png)

## 2h.

``` r
# creating new object with predictions from interactive model
mod_preds <- ggpredict(model_intereactive, 
                       terms = c("height_cm [all]", "scientific_name"))
# base layer: ggplot with predictions data
ggplot(mod_preds, aes( x = x,
                       y = predicted))+ 
  # first layer: geom ribbon to show confidence intervals for each group
  geom_ribbon(aes (ymin = conf.low,
                    ymax = conf.high,
                    fill = group),
              alpha = 0.4) + 
  # second layer: geom line for the model predictions
  geom_line(aes(color = group), 
                size = 0.9)+ 
  # third layer: geom point with nests_clean data
  geom_point(data = nests_clean, 
              aes( x = height_cm,
                   y = case_control), 
             color = "grey30", # coloring by scientific names
             alpha = 0.4, 
             size = 1.5) +
  # separating the panels by type of ant
  facet_wrap (~group) +
  # adjusting color palettes for each panel
  scale_color_brewer (palette = "Set1") +
  scale_fill_brewer(palette = "Set2") +
  # adding labels for x- and y- axes
  labs(
    x = "Tree Height (cm)",
    y = "Predicted Probability of Nest Occurrence") +
  # scale
  scale_y_continuous(labels = scales::percent) +
  # adjusting the theme
  theme_classic() +
  # removing the legend and editing text
  theme(
    legend.position = "none",
    strip.text = element_text(face = "italic", size = 11),
    axis.title = element_text(size = 12))
```

![](ENVS-193DS_Final_files/figure-commonmark/interactive_model_plot-1.png)

## 2i.

Figure 1. Species of ant and height of tree influence nesting
probability. Separate panels represent different species of ants (left
panel is C. sjostedti, middle panel is C. mimosae, and right panel is C.
nigriceps). Transparent, grey points represent individual data
observations of % probability of nesting (y- axes) from tree height (cm,
x-axes) and ant species. Central, opaque line (red, blue, or green
\[from left to right\]) represents interactive logistic model
predictions of probability of nesting. Wide, transparent ribbon around
line (green, red, blue \[left to right\]) represents 95% confidence
interval for each prediction. Data from: Lujan et al. in 2023.
https://doi.org/10.5281/zenodo.8373322

## 2j.

``` r
# making new object with tree height of 600 cm from 
max_height_data <- data.frame(
  height_cm = c(600, 600, 600),
  scientific_name = factor(c("Crematogaster sjostedti", 
                             "Crematogaster mimosae", 
                             "Crematogaster nigriceps"),
                           levels = c("Crematogaster sjostedti", 
                                      "Crematogaster mimosae", 
                                      "Crematogaster nigriceps")))
# using ggpredict to get model predictions
predictions_at_600 <- ggpredict(model_intereactive, 
                                terms = "scientific_name", 
                                condition = c(tree_height_cm = 600))

# displaying table!
print(predictions_at_600)
```

    # Predicted probabilities of case_control

    scientific_name         | Predicted |     95% CI
    ------------------------------------------------
    Crematogaster sjostedti |      0.05 | 0.01, 0.36
    Crematogaster mimosae   |      0.19 | 0.14, 0.25
    Crematogaster nigriceps |      0.94 | 0.55, 1.00

    Adjusted for:
    * height_cm = 212.58

## 2k.

Our model demonstrates that tree height and species of ant significantly
interact to dictate birds nesting choices. When using the model to
predict nest occurrence in the maximum measured height of the trees (600
cm), there was a high chance of it being nested when occupied by
Crematogaster nigriceps (0.94 probability, 95% CI\[0.55, 1.00\]), and
low chances if occupied by Crematogaster mimosae (0.19 probability, 95%
CI \[0.14, 0.25\]) or Crematogaster sjostedti (0.05 prbability, 95%
CI\[0.01, 0.36\]). Tree height does play a role, but ant species appears
to have a stronger affect on nesting probability seen from the predicted
lines. This effect is due to the behavior of the ants. Both
Crematogaster nigriceps and Crematogaster mimosae are aggressive,
defenssive species, whereas Crematogaster sjostedti tend to be less
aggressive. This means that these ants will protect the bird and their
young from predators. In addition, C. nigriceps “prune” the trees, which
lowers the chance of interactions with other trees.

# Problem 3

## 3a.

In homework 2, I visualized the relationships between cardio vs. workout
length and length of workout vs. workout satisfaction, and they’re
presented on graphs. In homework 3, I visualized the relationship
between amount of sleep and satisfaction with workout using circles of
varying size and color without a graph.

I noticed a similar color pallet between all my visualizations. I use
lots of pink, purples, and blues generally.

I see that my satisfaction with my workout does increase with the more
sleep I get, but other factors (workout type, length of workout, etc.)
do not impact this vary much. These aren’t different between
visualizations because the data didn’t show different trends.

The feedback I got was to not over complicate the affective
visualization with too many variables. I implemented this by taking out
variables that were superfluous (such as time of meal before workout).

## 3b.

An appropriate analysis of amount of sleep (continuous numeric variable
in hours) and how it effects satisfaction (ordinal variable, measured
1-5) with my workout is a linear regression model. This answers my
initial question of “How does the amount of sleep I get effect my
satisfaction with my workout?” by showing trends between my workout and
the rating I gave the following workout.

## 3c.

``` r
# making new object withe data of interest; satisfaction and amount of sleep
# from personal data
workout_model <- lm(satisfaction ~ sleep_hours, data = personal_clean)
# base R residuals
par(mfrow = c(2,2))
plot(workout_model)
```

![](ENVS-193DS_Final_files/figure-commonmark/diagnostic-plots-to-check-assumptions-1.png)

``` r
# displaying statistical summary of linear regression
summary(workout_model)
```


    Call:
    lm(formula = satisfaction ~ sleep_hours, data = personal_clean)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -2.15948 -0.54012  0.04263  0.52442  1.41023 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)  
    (Intercept)   0.3791     1.2720   0.298   0.7678  
    sleep_hours   0.3972     0.1662   2.389   0.0239 *
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.7645 on 28 degrees of freedom
    Multiple R-squared:  0.1694,    Adjusted R-squared:  0.1397 
    F-statistic: 5.709 on 1 and 28 DF,  p-value: 0.02385

## 3d.

``` r
# using ggpredict to get model predictions
personal_preds <- ggpredict(workout_model, 
                            terms = "sleep_hours")
#base layer: ggplot 
ggplot() +
  # second layer: geom_point to show underlying data with personal_clean data
  geom_point( data = personal_clean, 
       aes(x = sleep_hours, 
       y = satisfaction), 
       fill = "orchid1",
       size = 4,
       stroke = 1,
       shape = 21) +
  # third layer: geom_ribbon to show CI
 # using predictions data frame
 geom_ribbon(data = personal_preds,
             aes(x = x,
                 y = predicted, 
                 ymin = conf.low,
                 ymax = conf.high),
             fill = "cornflowerblue",
             alpha = 0.1) +
  # third layer: line representing model predictions
  geom_line(data = personal_preds,
            aes(x = x,
                y = predicted),
            color = "cornflowerblue",
            linewidth = 1) +
  # adding axes titles and titles!
  labs(
    x = "Amount of Sleep (Hours)",
    y = "Workout Satisfaction Score (1-5)",
    title = "Workout Satisfaction Predicted by Sleep Duration") +
  # changing theme
  theme_classic() +
  # removing grid lines, bolding title, and changing axes text
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title = element_text(face = "plain", size = 11))
```

![](ENVS-193DS_Final_files/figure-commonmark/visualization-of-linear-regression-1.png)

## 3e.

Figure 2. Less sleep is associated with lower workout satisfaction. Data
collected from workouts taken place in the time period 4-20-2026 to
5-27-2026 (Data accessed 05-28-2026). Orchid points represent individual
observations of amount of sleep (hours) (total n = 30) plotted against
workout satisfaction score (1-5). Cornflower blue, thin line represents
the linear regression model prediction for workout satisfaction given
amount of sleep. Thick, transparent, cornflower blue ribbon represents
95% confidence interval of linear regression model.

## 3f.

It was found that amount of sleep acted as a statistically significant
predictor of my satisfaction with my workout (linear regression,
F(28, 1) = 5.709, R<sup>2</sup> = 0.1694, p = 0.02385, $\alpha$ = 0.05).
Based on the model predictions, for each 1 unit increase in sleep, it’s
expected to have a increase of 0.3972 $\pm$ 0.1662 in workout
satisfaction. Approximately 16.94% of total variance in satisfaction
scores is accounted for by the moderate effect size sleep duration has
(R<sup>2</sup> = 0.1694).
