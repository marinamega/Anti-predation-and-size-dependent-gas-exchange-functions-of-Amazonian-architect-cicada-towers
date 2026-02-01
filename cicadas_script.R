library(readr)
library(ggplot2)
library(lme4)
library(DHARMa)
library(visreg)
library(emmeans)
library(scales)
library(dplyr)
library(patchwork)

setwd("") #Put your directory here

#Predation experiment

pred=read.table("Predation.csv", header = T, sep = ";")
str(pred)

# Aggregating predation and defining categories
pred$ant_occurrence <- pmax(pred$ant_ocor_t30,
                            pred$ant_ocor_t1h,
                            pred$ant_ocor_t3h)

pred$category <- ifelse(pred$height == 0, "Ground", "Tower")
pred$category <- as.factor(pred$category)

# Ant occurrence as a function of tower height

pred1 <- pred[pred$height > 0,]
summary(pred1)

# Testing relationships: bait location 
test_pred_subs <- glmer(ant_occurrence ~ category + (1|site), family=binomial, data=pred)
summary(test_pred_subs)

vr_bait <- visreg(
  test_pred_subs,
  "category",
  scale = "response",
  plot = FALSE
)

bait_location_plot <- ggplot() +
  geom_point(
    data = pred,
    aes(x = category, y = ant_occurrence),
    size = 2.5,
    shape = 16,
    position = position_jitter(width = 0.15, height = 0)
  ) +
  geom_segment(
    data = vr_bait$fit,
    aes(
      x = as.numeric(category) - 0.25,
      xend = as.numeric(category) + 0.25,
      y = visregFit,
      yend = visregFit
    ),
    color = "blue",
    linewidth = 1) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "b)",
    x = "Bait location",
    y = "Probability of ant occurrence"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Testing relationships: tower height
test_pred_height <- glmer(ant_occurrence ~ height + (1|site), family=binomial, data=pred1)
summary(test_pred_height)

vr_height <- visreg(
  test_pred_height,
  "height",
  scale = "response",
  plot = FALSE
)

tower_height_plot <- ggplot() +
  geom_point(
    data = pred,
    aes(x = height, y = ant_occurrence),
    size = 2.5,
    shape = 16
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "a)",
    x = "Tower height (cm)",
    y = "Probability of ant occurrence"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# water addition and sealing experiments
water_sealing=read.table("water_sealing.csv", header = T, sep = ";", fileEncoding = "latin1")
str(water_sealing)

col_obs <- which(names(water_sealing) == "obs")

water_sealing <- water_sealing[, 1:col_obs]

water_sealing$treatment <- as.factor(water_sealing$treatment)
water_sealing$treatment <- relevel(water_sealing$treatment, ref="control")

water = water_sealing[water_sealing$treatment == "water",]
sealing = water_sealing[water_sealing$treatment == "sealing",]
con = water_sealing[water_sealing$treatment == "control",]

#Water Addition vs Control
water_vs_control <- water_sealing[water_sealing$treatment != "sealing",]
test_water <- glm(rec_height ~ height*treatment, data=water_vs_control)
summary(test_water)
anova(test_water)

cores <- c("#21918c", "#fb9b06")

water_plot <- ggplot(
  water_vs_control,
  aes(x = height, y = rec_height, color = treatment)
) +
  geom_point(
    size = 2.5
  ) +
  scale_color_manual(values = cores) +
  scale_y_continuous(
    limits = c(-2, 8),
    breaks = seq(-2, 8, by = 2)
  ) +
  labs(
    title = "c)",
    x = "Original height (cm)",
    y = "Growth rate (cm/night)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Sealing vs control
sealing_vs_control <- water_sealing %>% 
  filter(treatment %in% c('control', 'sealing'))

test_sealing <- glm(rec_height ~ height*treatment, data=sealing_vs_control)
summary(test_sealing)
anova(test_sealing)

cores <- c("#21918c", "#320a5e")

sealing_vs_control$treatment <- droplevels(sealing_vs_control$treatment)

newdata <- expand.grid(
  height = seq(
    min(sealing_vs_control$height, na.rm = TRUE),
    max(sealing_vs_control$height, na.rm = TRUE),
    length.out = 100
  ),
  treatment = levels(sealing_vs_control$treatment)
)

pred <- predict(test_sealing, newdata, se.fit = TRUE)

newdata <- newdata %>%
  mutate(
    fit = pred$fit,
    se  = pred$se.fit,
    lwr = fit - 1.96 * se,
    upr = fit + 1.96 * se
  )

sealing_plot <- ggplot() +
  geom_point(
    data = sealing_vs_control,
    aes(x = height, y = rec_height, color = treatment),
    size = 2.5
  ) +
  geom_ribbon(
    data = newdata,
    aes(
      x = height,
      ymin = lwr,
      ymax = upr,
      fill = treatment
    ),
    alpha = 0.25,
    colour = NA
  ) +
  geom_line(
    data = newdata,
    aes(x = height, y = fit, color = treatment),
    linewidth = 1
  ) +
  scale_color_manual(values = cores) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(
    limits = c(-2, 8),
    breaks = seq(-2, 8, by = 2)
  ) +
  labs(
    title = "d)",
    x = "Original height (cm)",
    y = "Growth rate (cm/night)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

coef_mod <- coef(test_sealing)

b2 <- coef_mod["treatmentsealing"]
b3 <- coef_mod["height:treatmentsealing"]

x_cross <- -b2 / b3
x_cross

final_fig <- (tower_height_plot + bait_location_plot)/(water_plot + sealing_plot)

ggsave(
  filename = "Figure_2.pdf",
  plot = final_fig,
  width = 180,      # mm
  height = 180,     # mm
  units = "mm",
  device = cairo_pdf
)

ggsave(
  filename = "Figure_2.png",
  plot = final_fig,
  width = 180,
  height = 180,
  units = "mm",
  dpi = 600,
  bg = "white"
)
