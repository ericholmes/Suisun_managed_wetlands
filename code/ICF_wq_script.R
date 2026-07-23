##ICF wq/zoop script
library(tidyverse)

icf_wq <- readxl::read_excel("C:/Users/eholmes/Documents/R/Projects/Suisun_managed_wetlands/data/DWR_SuisunMarsh_WaterQualityData_WORKING_7.15.26.xlsx")
icf <- janitor::clean_names(icf_wq)
unique(icf$location)
icf$locfac <- factor(icf$location, levels = c("Inlet", "Outlet", "2m",     "20m",  "200m"  ))
colnames(icf)
str(icf)

icf$f_dom_qsu <- as.numeric(icf$f_dom_qsu)

icflong <- icf %>%
  pivot_longer(
    cols = c(
      temp_c,
      do_percent,
      do_mg_l,
      spc_m_s_cm,
      tds_mg_l,
      sal_psu,
      p_h,
      turb_fnu,
      pc_mg_l,
      chl_mg_l,
      f_dom_qsu,
      zoop_score_1_5
    ),
    names_to = "variable",
    values_to = "value"
  ) %>%
  select(date, locfac, pond, variable, value)

ggplot(icf, aes(x = locfac, y = do_percent)) + geom_line(aes(color = date, group = date)) + facet_grid(pond ~ .)

ggplot(icflong[icflong$pond == "North Lower Joice",], aes(x = locfac, y = value)) + 
  geom_line(aes(color = date, group = date)) + facet_wrap(. ~ variable, scales = "free")

# --- Build individual plots ---
png("output/ICF_wq_point%02d.png", family = "serif", height = 4.5, width = 7, units = "in", res = 1000)
(p1 <- ggplot(icflong[icflong$pond == "North Lower Joice",], 
              aes(x = date, y = value, color = locfac)) +
    geom_point() + scale_x_date(date_labels = "%b-%d") + theme_bw() +
    geom_line(aes(group = locfac)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(. ~ variable, scales = "free_y") +
    labs(title = "North Lower Joice", x = NULL, color = "Location"))

(p2 <- ggplot(icflong[icflong$pond == "Denverton Big Unit",], 
              aes(x = date, y = value, color = locfac)) +
    geom_point() + scale_x_date(date_labels = "%m/%d") +
    geom_line(aes(group = locfac)) +
    facet_wrap(. ~ variable, scales = "free_y") +
    theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Denverton Big Unit", x = NULL, color = "Location"))

(p3 <- ggplot(icflong[icflong$pond == "Denverton Small Unit",], 
              aes(x = date, y = value, color = locfac)) +
    geom_point() + scale_x_date(date_labels = "%m/%d") +
    geom_line(aes(group = locfac)) +
    facet_wrap(. ~ variable, scales = "free_y") +
    theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Denverton Small Unit", x = NULL, color = "Location"))

dev.off()

png("output/ICF_wq_point_date%02d.png", family = "serif", height = 5, width = 7, units = "in", res = 1000)

(p1d <- ggplot(icflong[icflong$pond == "North Lower Joice",], 
               aes(x = locfac, y = value, color = date)) +
    geom_point() + theme_bw() +
    geom_line(aes(group = date)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(. ~ variable, scales = "free_y") +
    labs(title = "North Lower Joice", x = NULL))

(p2d <- ggplot(icflong[icflong$pond == "Denverton Big Unit",], 
               aes(x = locfac, y = value, color = date)) +
    geom_point() +
    geom_line(aes(group = date)) +
    facet_wrap(. ~ variable, scales = "free_y") +
    theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Denverton Big Unit", x = NULL))

(p3d <- ggplot(icflong[icflong$pond == "Denverton Small Unit",], 
               aes(x = locfac, y = value, color = date)) +
    geom_point() +
    geom_line(aes(group = date)) +
    facet_wrap(. ~ variable, scales = "free_y") +
    theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Denverton Small Unit", x = NULL))

dev.off()

# --- Extract the shared legend ---
shared_legend <- cowplot::get_legend(
  p1 + theme(legend.position = "bottom")
)

# --- Remove legends from each panel ---
p1_nl <- p1 + theme(legend.position = "none")
p2_nl <- p2 + theme(legend.position = "none")
p3_nl <- p3 + theme(legend.position = "none")

# --- Combine panels ---
combined <- cowplot::plot_grid(
  p1_nl, p2_nl, p3_nl,
  nrow = 1,
  rel_widths = c(1, 1, 1)
)

# --- Add shared legend below (or above) ---
final_plot <- cowplot::plot_grid(
  combined,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

final_plot

