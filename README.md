# Stat-3302-Final-Project

# Fertilizer
```{r}
fert <- fertilizer %>%
  filter(Year %in% 2002:2013) %>%
  filter(Entity %in% c("Africa", "Americas", "Asia", "Australia & New Zealand", "Caribbean", "Central America", "Eastern Africa", "Eastern Asia", "Eastern Europe", "Europe", "Europe, Western", "Micronesia (region)", "Middle Africa", "Northern Africa", "Northern America", "Oceania", "South America", "South Eastern Asia", "Southern Africa", "Southern Asia", "Southern Europe", "Western Africa", "Western Asia"))
fert <-  mutate(fert, Entity = ifelse(Entity == "Europe, Western", "Western Europe", Entity)) %>%
  group_by(Year, Entity) %>%
  summarize(yield = `Cereal yield (tonnes per hectare)`, fertilizer = `Nitrogen fertilizer use (kilograms per hectare)`)

```
```{r}
df_crop = data.frame(Year = crop$Year, Entity = crop$Entity, wheat = crop$wheat, rice = crop$rice, maize = crop$maize, soybeans = crop$soybeans, potatoes = crop$potatoes, beans = crop$beans, peas = crop$peas, cassava = crop$cassava, barley = crop$barley, cocoa = crop$cocoa_beans, bananas = crop$bananas, total = round(crop$total))
df_fert = data.frame(Year = fert$Year, Entity = fert$Entity, fertilizer = fert$fertilizer, Yield = fert$yield)
df_air = data.frame(Year = air$Year, Entity = air$Entity, land = air$arable_land)
merged_data = full_join(df_crop, df_fert)
merged_data = full_join(merged_data, df_air)

```

```{r best entity}
(best_entity <- glm(round(Yield) ~ Entity + barley + cocoa, data = merged_data, family = "quasipoisson"))

fitted = best_entity$fitted.values
d = resid(best_entity, type = "deviance")
resid_df3 = data.frame(fitted = fitted, deviance = d)

ggplot(resid_df3, aes(x = fitted, y = deviance)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 2) + 
  labs(title = "Deviance residuals")

ggplot(resid_df3, aes(sample = deviance)) +stat_qq() + stat_qq_line() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
```

```{r best year}
(best_year <- glm(round(Yield) ~ as.factor(Year) + wheat + maize + soybeans + potatoes + cassava + barley + cocoa, data = merged_data, family = "quasipoisson"))

fitted = best_year$fitted.values
d = resid(best_year, type = "deviance")
resid_df4 = data.frame(fitted = fitted, deviance = d)

ggplot(resid_df4, aes(x = fitted, y = deviance)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 2) + 
  labs(title = "Deviance residuals")

ggplot(resid_df4, aes(sample = deviance)) +stat_qq() + stat_qq_line() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
```

```{r best arable land}
(best_air <- glm(round(Yield) ~ land + wheat + maize + potatoes + cassava + barley + cocoa, data = merged_data, family = "quasipoisson"))

fitted = best_air$fitted.values
d = resid(best_air, type = "deviance")
resid_df5 = data.frame(fitted = fitted, deviance = d)

ggplot(resid_df5, aes(x = fitted, y = deviance)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 2) + 
  labs(title = "Deviance residuals")

ggplot(resid_df5, aes(sample = deviance)) +stat_qq() + stat_qq_line() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
```

```{r best crops}
(top_crops <- (glm(round(Yield) ~ wheat + maize + soybeans + potatoes + cassava + barley + cocoa, data = merged_data, family = "quasipoisson")))

fitted = top_crops$fitted.values
d = resid(top_crops, type = "deviance")
resid_df6 = data.frame(fitted = fitted, deviance = d)

ggplot(resid_df6, aes(x = fitted, y = deviance)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 2) + 
  labs(title = "Deviance residuals")

ggplot(resid_df6, aes(sample = deviance)) +stat_qq() + stat_qq_line() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
```

```{r best fertilizer}
yield_glm2 = glm(round(Yield) ~  fertilizer+wheat+rice+maize+barley+soybeans+potatoes+beans+cassava+ cocoa+bananas, family = "quasipoisson", data = merged_data)
summary(yield_glm2)

fitted = yield_glm$fitted.values
d = resid(yield_glm, type = "deviance")
resid_df2 = data.frame(fitted = fitted, deviance = d)

ggplot(resid_df2, aes(x = fitted, y = deviance)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 2) + 
  labs(title = "Deviance residuals")

ggplot(resid_df2, aes(sample = deviance)) +stat_qq() + stat_qq_line() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
```
