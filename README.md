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
df_crop = data.frame(Year = crop$Year, Entity = crop$Entity, wheat = crop$wheat, rice = crop$rice, maize = crop$maize, soybeans = crop$soybeans, potatoes = crop$potatoes, beans = crop$beans, peas = crop$peas, cassava = crop$cassava, barley = crop$barley, cocoa = crop$cocoa_beans, bananas = crop$bananas, total = round(crop$total))
df_fert = data.frame(Year = fert$Year, Entity = fert$Entity, fertilizer = fert$fertilizer, Yield = fert$yield)
df_air = data.frame(Year = air$Year, Entity = air$Entity, land = air$arable_land)
merged_data = full_join(df_crop, df_fert)
merged_data = full_join(merged_data, df_air)
merged_data[is.na(merged_data)] <- 0

```

```{r}
yield_glm = glm(round(Yield) ~  fertilizer + wheat + rice + maize + barley, family = 'poisson', data = merged_data)
summary(yield_glm)
```

```{r}
yield_glm = glm(round(Yield) ~  fertilizer + wheat + rice + maize + barley + soybeans + potatoes + beans + cassava + cocoa + bananas, family = 'poisson', data = merged_data)
summary(yield_glm)
```

```{r}
yield_glm = glm(round(Yield) ~ Entity + Year + fertilizer + land + wheat + rice + maize + soybeans + potatoes + beans + peas + cassava + barley + cocoa + bananas, family = 'poisson', data = merged_data)
summary(yield_glm)

yield_glm = glm(round(Yield) ~ Entity + Year + fertilizer + land + wheat + rice + maize + barley, family = 'poisson', data = merged_data)
summary(yield_glm)

```
