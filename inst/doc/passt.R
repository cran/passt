## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.height=3.5, fig.width=5,
                      fig.align = "center")

## ------------------------------------------------------------------------
library(passt)
set.seed(20191015)

## ------------------------------------------------------------------------
sim1 <- run_sim(patterns = diag(10), frequency = 1:10, duration = 10:1,
                lrate_onset = 0.05, lrate_drop_time = 2, lrate_drop_perc = 0)

## ------------------------------------------------------------------------
head(sim1$output)

## ------------------------------------------------------------------------
plot(x = 1:10, y = colMeans(sim1$output), xlab = "Frequency [#]", ylab = "Activation",
     pch = 19)

## ------------------------------------------------------------------------
correlations <- apply(sim1$output, 1, function(x) cor(1:10, x))
hist(correlations)

## ----warning=FALSE-------------------------------------------------------
sim2 <- run_exp(patterns = diag(10), frequency = 1:10,
                duration = 10:1,
                lrate_onset = 0.05, lrate_drop_time = 2,
                lrate_drop_perc = 0, cor_noise_sd = 0.1)

## ------------------------------------------------------------------------
sim2

## ------------------------------------------------------------------------
duration <- c(4, 2, 1, 8, 4, 2, 12, 6, 3)
frequency <- c(2, 4, 8, 2, 4, 8, 2, 4, 8)
total_duration = frequency * duration
cor(frequency, total_duration)

## ------------------------------------------------------------------------
lrate_drop_perc <- seq(0, 1, 0.04)
sim4 <- lapply(lrate_drop_perc, function(x) 
  run_exp(frequency, duration, 0.05, 2, x, diag(9), 30, 0.1))

## ------------------------------------------------------------------------
sim4 <- plyr::ldply(sim4, "data.frame")
sim4 <- cbind(sim4, lrate_drop_perc)
sim4

## ----warning=FALSE-------------------------------------------------------
library(ggplot2)
ggplot(sim4, aes(f_dv, td_dv, color = lrate_drop_perc*100)) +
  geom_point(alpha = 1) +
  guides(color = guide_colorbar(order=1)) +
  scale_color_gradient(name = "Learning parameter drop [%]", low = "grey90",
                        high = "black", breaks = seq(0, 100, 25)) +
  theme_bw() +

  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.background = element_rect(fill=alpha('white', 0.3))) +
  scale_x_continuous(name = "Influence of frequency", limits = c(-.1, 1)) +
  scale_y_continuous("Influence of duration", limits = c(-.1, 1))

