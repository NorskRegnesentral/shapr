library(data.table)
library(ggplot2)
library(stringr)

calc_lines = function(data, start, end){
  c1_100 = data[start]
  c1_200 = data[end]
  a1 = c1_100
  b1 = (c1_200 - c1_100) / end - start
  l1 = seq(a1, c1_200, length.out = (end - start + 1))
  print(a1)
  print(c1_200)
  return(l1)
}

## (1) Simulate random walk and plot different series and adjusted series

run <- function(p, start) {
  values = current = start
  for(i in 1:364){
    current <- current + ifelse(runif(1) < p, 1, -1)
    values <- c(values, current)
  }
  values
}

N <- 3
p <- 0.47
set.seed(1055)
vlist = data.table(indiv = run(0.48, 50) + sin(2))
vlist[, sample1 := run(0.52, 70) + sin(2)] # blue
vlist[, sample2 := run(0.48, 50) + sin(2)] # red
vlist[, day := 1:365]

start = 91 # interval to predict
end = 91 + 91
l1 = calc_lines(vlist[['indiv']], start, end) # straight line
l1_all = c(rep(NA, start - 1), l1, rep(NA, end + 1)) # with NAs for ggplot

l2 = calc_lines(vlist[['sample1']], start, end) # straight line
l2_all = c(rep(NA, start - 1), l2, rep(NA, end + 1)) # with NAs for ggplot
d2 = vlist[['sample1']][start:end] - l2 # difference between straight line and sampled curve

d1_2 = l1 + d2 # adjusted line

d1_2_all = c(rep(NA, start - 1), d1_2, rep(NA, 365 - end)) # with NAs for ggplot


d1_2_all_df = data.frame(day = 1:365, variable = "sample1",  value = d1_2_all, linetype = 1, col = 3)
l1_2_all_df = data.frame(day = 1:365, variable = "sample1_line",  value = l2_all, linetype = 1, col = 2)
l1_all_df = data.frame(day = 1:365, variable = "indv_line",  value = l1_all, linetype = 1, col = 1)

vlist_long = melt(vlist[, c("indiv", "sample1", "day")], id.vars = c("day"),
                  measure.vars = c("indiv", "sample1"))
vlist_long[, linetype := 0]
vlist_long[, col := c(rep(1, 365), rep(2, 365))]


vlist_long = rbind(vlist_long, d1_2_all_df, l1_all_df, l1_2_all_df) # l1_2_all,

# ggplot() + theme_bw() + xlab("Day") +
#   geom_line(data = vlist_long, aes(y = value, x = day, col = factor(col), linetype = factor(linetype)), size = 1) +
#   geom_rect(aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill="grey", alpha = 0.2) +
#   scale_color_discrete(name = "", labels = c("Individ", "Sample 1", "Sample 1 adjust")) +
#   scale_linetype_discrete(name = "", labels = c("True", "Estimated line"))

ggplot() + theme_bw() + xlab("Day") + ylab("Balance (scaled)") +
  geom_line(data = vlist_long, aes(y = value, x = day, col = factor(col), linetype = factor(linetype)), size = 1) +
  geom_rect(aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill="grey", alpha = 0.2) +
  scale_color_discrete(name = "", labels = c("Individ", "Sample 1", "Sample 1 adjust")) +
  scale_linetype_discrete(name = "", labels = c("True", "Estimated line"))

## (2) Read in DNB data for four bank account series and calculate and plot adjusted series


vlist = fread("/nr/project/stat/BI_PersonalisedMarketing/Explanations/Martin/groupShapley-paper/time_series_data/non-sensitive/x_explain_new_fixed_csv.csv")
names = c("variable_num", "8_new", "9_new", "40_new", "837_new")
vlist[, (names) := lapply(.SD, function(x) str_replace(x, ",", ".")), .SDcols = names]
vlist[, (names) := lapply(.SD, as.numeric), .SDcols = names]

# This is so the adjusted series falls above 0 so we don't have to cut it off
vlist[, `40_new` := ifelse(variable_num >= 91 & variable_num <= 182 & `40_new` <= 0.6, `40_new` + 0.12, `40_new`)]
vlist[, `8_new` := ifelse(variable_num >= 150 & variable_num <= 182 & `8_new` >= 0.6, `8_new` - 0.1, `8_new`)]


# fwrite(vlist, "/nr/project/stat/BI_PersonalisedMarketing/Explanations/Martin/x_explain_new_fixed_Annabelle.csv")

# setnames(vlist, c("variable_num", "8_new", "9_new", "40_new", "837_new"), c("day", "indiv", "sample1", "sample2", "sample3"))
# setnames(vlist, c("variable_num", "8_new", "9_new", "40_new", "837_new"), c("day","sample1",   "sample3",   "sample2", "indiv"))

setnames(vlist, c("variable_num", "8_new", "9_new", "40_new", "837_new"), c("day","sample1",   "sample3", "indiv",   "sample2"))
start = 91 # interval to predict
end = 91 + 91

l1 = calc_lines(vlist[['indiv']], start, end) # line for individual
l1_all = c(rep(NA, start - 1), l1, rep(NA, end + 1)) # for ggplot

l2 = calc_lines(vlist[['sample1']], start, end) # for sample
l2_all = c(rep(NA, start - 1), l2, rep(NA, end + 1)) # for ggplot
d2 = vlist[['sample1']][start:end] - l2 # distance to line

d1_2 = l1 + d2 # adjusted sampled line
d1_2_all = c(rep(NA, start - 1), d1_2, rep(NA, 365 - end)) # for ggplot

# d1_2_all_df = data.frame(day = 1:365, variable = "sample1",  value = d1_2_all, linetype = 0, col = 3, size = 0)
# l1_2_all_df = data.frame(day = 1:365, variable = "sample1_line",  value = l2_all, linetype = 1, col = 2, size = 0)
# l1_all_df = data.frame(day = 1:365, variable = "indv_line",  value = l1_all, linetype = 1, col = 1, size = 0)
#
# vlist_long = melt(vlist[, c("indiv", "sample1", "day")], id.vars = c("day"),
#                   measure.vars = c("indiv", "sample1"))
# vlist_long[, linetype := 0]
# vlist_long[, col := c(rep(1, 365), rep(2, 365))]
# vlist_long[, size := c(rep(0.000002, 365), rep(0, 365))]
#
# vlist_long = rbind(vlist_long, d1_2_all_df, l1_all_df, l1_2_all_df) # l1_2_all,

# ggplot() + theme_bw() + xlab("Days") + ylab("Balance (scaled)") +
#   geom_line(data = vlist_long[variable == "indiv"], aes(y = value, x = day, col = factor(col), linetype = factor(linetype)), size = 2) +
#   geom_line(data = vlist_long[variable != "indiv"], aes(y = value, x = day, col = factor(col), linetype = factor(linetype)), size = 1) +
#   geom_rect(aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill="grey", alpha = 0.3) +
#   scale_color_discrete(name = "", labels = c("Individual", "Sample 1", "Sample 1 adjusted")) +
#   scale_linetype_discrete(name = "", labels = c("True time series", "Straight line"))


# red solid size 1
# red solid size 0
# red dashed size
# green solid size 0
# green dashed size 0
# blue solid size 2



vlist1 = copy(vlist)[, linetype := 0][, col := 1][, ] # size 1
vlist6 = data.table(day = 1:365,   indiv = d1_2_all, linetype = 0, col = 3) # size 1

vlist_12 = rbind(vlist1[(day < 91 | day > 179), c("day", "indiv", "col")], vlist6[(day >= 91 & day <= 179), c("day", "indiv", "col")])

vlist2 = copy(vlist)[, indiv := ifelse((day >= 91 & day < 180), indiv, NA)][, linetype := 0][, col := 1] # size 0
vlist3 = copy(vlist)[, c("day", "sample1")][, linetype := 0][, col := 2] # size 0

vlist4 = data.table(day = 1:365, variable = "indv_line",  value = l1_all, linetype = 1, col = 1) # size 0
vlist5 = data.table(day = 1:365, variable = "sample1_line",  value = l2_all, linetype = 1, col = 2) # size 0



ggplot() + theme_bw() + xlab("Days") + ylab("Balance (scaled)") +
  # geom_line(data = vlist1, aes(y = indiv, x = day, col = factor(col), linetype = factor(linetype)), size = 1.5) +
  # geom_line(data = vlist2, aes(y = indiv, x = day, col = factor(col), linetype = factor(linetype)), size = 0.70) +
  geom_path(data = vlist_12[order(day)], aes(y = indiv, x = day, col = factor(col), group = 1), linetype = 1, size = 1.5) +

  geom_line(data = vlist2, aes(y = indiv, x = day, col = factor(col)), linetype = 1, size = 0.70) +
  geom_line(data = vlist3, aes(y = sample1, x = day, col = factor(col)), linetype = 1, size = 0.70) +
  geom_line(data = vlist4, aes(y = value, x = day, col = factor(col)), linetype = 2, size = 0.70) +
  geom_line(data = vlist5, aes(y = value, x = day, col = factor(col)), linetype = 2, size = 0.70) +
  geom_rect(aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill="grey", alpha = 0.2) +
  scale_color_discrete(name = "", labels = c("Individual time series", "Sample 1 time series", "Sample 1 adjusted time series")) +
  scale_linetype_discrete(name = "", labels = c("True time series", "Straight line")) +
  theme(plot.margin=unit(c(0.05,0,0,0),"cm"))


ggsave("plots/real_data_time_series_plot_5.pdf",
       width = 20, height = 8, units = "cm")










###################
# vlist = data.table(indiv = sin(seq(-pi, 3*pi, length.out = 365)) + 1, sample1 = sin(seq(-2*pi, 2*pi, length.out = 365) - 1 ), day = 1:365)
#
# start = 91
# end = 91+91
# l1 = calc_lines(vlist[['indiv']], start, end)
# l1_all = c(rep(NA, start - 1), l1, rep(NA, end + 1))
#
# l2 = calc_lines(vlist[['sample1']], start, end)
# l2_all = c(rep(NA, start - 1), l2, rep(NA, end + 1))
# d2 = vlist[['sample1']][start:end] - l2
#
# # l3 = calc_lines(vlist[['sample2']], start, end)
# # l3_all = c(rep(NA, start - 1), l3, rep(NA, end + 1))
# # d3 = vlist[['sample2']][start:end] - l3
#
# d1_2 = l1 + d2
# # d1_3 = l1 + d3
#
# d1_2_all = c(rep(NA, start - 1), d1_2, rep(NA, 365 - end))
# # d1_3_all = c(rep(NA, start - 1), d1_3, rep(NA, 365 - end))
#
#
# d1_2_all_df = data.frame(day = 1:365, variable = "sample1",  value = d1_2_all, linetype = 1, col = 3)
# l1_2_all_df = data.frame(day = 1:365, variable = "sample1_line",  value = l2_all, linetype = 1, col = 2)
# l1_all_df = data.frame(day = 1:365, variable = "indv_line",  value = l1_all, linetype = 1, col = 1)
#
# vlist_long = melt(vlist[, c("indiv", "sample1", "day")], id.vars = c("day"),
#                   measure.vars = c("indiv", "sample1"))
# vlist_long[, linetype := 0]
# vlist_long[, col := c(rep(1, 365), rep(2, 365))]
#
#
# vlist_long = rbind(vlist_long, d1_2_all_df, l1_all_df, l1_2_all_df) # l1_2_all,
#
#
# ggplot() + theme_bw() + xlab("Day") +
#   geom_line(data = vlist_long, aes(y = value, x = day, col = factor(col), linetype = factor(linetype)), size = 1) +
#   geom_rect(aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill="grey", alpha = 0.2) +
#   scale_color_discrete(name = "", labels = c("Individ", "Sample 1", "Sample 1 adjust")) +
#   scale_linetype_discrete(name = "", labels = c("True", "Estimated line"))

