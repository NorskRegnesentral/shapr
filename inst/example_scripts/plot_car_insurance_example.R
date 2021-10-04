

library(ggplot2)
library(shapr)
library(data.table)


explanation_group <- readRDS("~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/groupShapley-paper/car_insurance_data/car_insurance_explanations.rds")

size = 7 # this is a good size for the paper
theme_set(theme_bw()) # this makes a white background
p1 = plot(explanation_group, plot_phi0 = F,include_title = F) +
  ggplot2::facet_wrap(~header,  labeller = "label_value", ncol = 2) + # scales = "free_x",
  ggplot2::theme(
    legend.text = element_text(size = size),
    legend.title = element_text(size = size),
    axis.text = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title = element_text(size = size),
    strip.text = element_text(size = size)
  ) + xlab("Feature group") + ylab("Feature group contribution")+
  theme(plot.margin=unit(c(0,0,0,0),"cm"))
p1
#
ggsave( # new because the old figure is just IDs = c(1, 2, 3, 4)
  "car-insurance-glm-3-groups-new4.png",
  plot = p1,
  device = 'png',
  path = 'plots',
  scale = 1.1,
  width = 13,
  height = 6,
  units = "cm"
)



