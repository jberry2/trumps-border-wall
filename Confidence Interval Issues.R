
ggplot(pcts,aes(x =fct_reorder(group, pct),y = pct)) + geom_point() +
  geom_errorbar(
    aes(ymin = pct-1.96*sqrt(pct*(1-pct)/n),
        ymax = pct+1.96*sqrt(pct*(1-pct)/n))) +
  coord_flip() +
  theme_minimal() +
  labs(y = "Percent Approving Tariff",x  = "")