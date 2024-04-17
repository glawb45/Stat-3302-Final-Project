# Stat-3302-Final-Project

# Fertilizer
```{r}
fert <- fertilizer %>%
  filter(Year %in% 2002:2013) %>%
  filter(Entity %in% c("Africa", "Americas", "Asia", "Australia & New Zealand", "Caribbean", "Central America", "Eastern Africa", "Eastern Asia", "Eastern Europe", "Europe", "Europe, Western", "Micronesia (region)", "Middle Africa", "Northern Africa", "Northern America", "Oceania", "South America", "South Eastern Asia", "Southern Africa", "Southern Asia", "Southern Europe", "Western Africa", "Western Asia"))
fert <-  mutate(fert, Entity = ifelse(Entity == "Europe, Western", "Western Europe", Entity)) %>%
  group_by(Year, Entity) %>%
  summarize(yield = `Cereal yield (tonnes per hectare)`, fertilizer = `Nitrogen fertilizer use (kilograms per hectare)`)

fert[is.na(fert)] <- 0
```

```{r}
yield_glm = glm(round(Yield) ~ Entity + Year + fertilizer + land + wheat + rice + maize + soybeans + potatoes + beans + peas + cassava + barley + cocoa + bananas, family = 'poisson', data = merged_data)
summary(yield_glm)

yield_glm = glm(round(Yield) ~ Entity + Year + fertilizer + land + wheat + rice + maize + barley, family = 'poisson', data = merged_data)
summary(yield_glm)

```
