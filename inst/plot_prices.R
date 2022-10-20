library(ggplot2)
library(dplyr)
library(viridis)

d = read.csv("inst/ak_salmon_exvessel_prices.csv")
names(d) = c("year","area","species","price")

d$species[grep("Pink",d$species)] = "pink"

d = dplyr::filter(d, species=="pink",
                  area %in% c("Alaska Peninsula","
                              Bristol Bay",
                              "Cook Inlet",
                              "Kodiak",
                              "Prince William Sound",
                              "Southeast"))


p1 <- ggplot(d, aes(year, price, col = area)) +
  geom_line() +
  theme_bw() +
  scale_color_viridis_d(end=0.8) +
  xlab("Year") +
  ylab("Price / lb.")
ggsave("Trends_price_lb.png")
# convert to wide format
d_wide = tidyr::pivot_wider(dplyr::select(d,-species),
                            names_from = area,
                            values_from = price)
d_diff = d_wide
d_diff[1,2:6] = NA
d_diff_sd = d_wide
d_diff_sd[1,2:6] = NA
diff_sd = rep(NA, nrow(d_diff))
for(i in 2:nrow(d_diff)) {
  d_diff[i,-1] = log(d_wide[i,-1]) - log(d_wide[i-1,-1])
  if(i >= 4) {
    cormat <- cor(d_diff[(i-2):i,-1])
    diff_sd[i] = mean(cormat[lower.tri(cormat)],na.rm=T)
    for(j in 2:6) d_diff_sd[i,j] = sd(unlist(d_diff[(i-2):i,j]), na.rm=T)
  }
}

diffsd = tidyr::pivot_longer(d_diff_sd, cols = 2:6) %>%
  dplyr::rename(area = name)

p2 <- ggplot(diffsd, aes(year, value, col = area)) +
  geom_line() +
  theme_bw() +
  scale_color_viridis_d(end=0.8) +
  xlab("Year") +
  ylab("Running 3-yr sd")
ggsave("Trends_price_volatility.png")

df = data.frame(year = unique(d$year),
                cor = diff_sd)
p3 <- ggplot(df, aes(year, cor)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("") +
  ylab("YOY cor across regions")
ggsave("Trends_price_correlation.png")

