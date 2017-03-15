## R4DS Ch 20
library(modelr)
library(tidyverse)

library(gapminder)

gapminder

## plot for life expectancy over time for each country
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
    geom_line(alpha = 1/3)

## find countries that don't fit linear growth signal
## by fitting linear model and showing residuals

## ex. with single country, New Zealand
nz <- filter(gapminder, country == "New Zealand")
nz %>%
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend + ")

nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")

## enter nested data frame for each country
## start w grouped df then nest
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

by_country

by_country$data[[1]]

## now have model-fitting function, country_model
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

## use purrr::map() to apply to each element
## list <- map(vector, function)
models <- map(by_country$data, country_model)

## create new var in by_country for
## storing model elements w/ df
by_country <- by_country %>%
  mutate(model = map(data, country_model))

by_country

## can filter and arrage w/ df semantics
by_country %>%
  filter(continent == "Europe")

by_country %>%
  arrange(continent, country)

## unnesting
## compute residuals for 142 dfs and models
by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

## turn list of dfs back into df using unnest()
resids <- unnest(by_country, resids)
resids

## with reg df can plot the residuals
resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

## faceting by country
resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) +
  facet_wrap(~continent)

## return to p 406