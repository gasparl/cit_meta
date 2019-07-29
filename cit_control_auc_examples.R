library("neatStats")
library("ggpubr")
normal_perfect = function( n, mean = 0, sd = 1 ) {
    stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd )
}

truth_1 = normal_perfect(100, mean = 30, sd = 10)
lies_1 = normal_perfect(100, mean = 60, sd = 10)

p_1 = t_neat(
    lies_1,
    truth_1,
    plot_densities = T,
    auc_added = T,
    x_label = NULL,
    factor_name = NULL,
    var_names = c("liars", "truthtellers"),
    reverse = T
)$density_plot + scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + scale_y_continuous(limits = c(0, 0.078)) +
    theme(plot.margin = unit(c(0.2, 0, 0, 0), "npc"))

lies_2 = normal_perfect(50, mean = 70, sd = 10)

p_2 = t_neat(
    lies_2,
    truth_1,
    plot_densities = T,
    auc_added = T,
    x_label = NULL,
    factor_name = NULL,
    var_names = c("liars", "truthtellers"),
    reverse = T
)$density_plot + scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))  + scale_y_continuous(limits = c(0, 0.078)) +
    theme(plot.margin = unit(c(0.2, 0, 0, 0), "npc"))

truth_2 = normal_perfect(50, mean = 30, sd = 5)

p_3 = t_neat(
    lies_2,
    truth_2,
    plot_densities = T,
    auc_added = T,
    x_label = "lie indicator value",
    factor_name = NULL,
    var_names = c("liars", "truthtellers"),
    reverse = T
)$density_plot + scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + scale_y_continuous(limits = c(0, 0.078)) +
    theme(plot.margin = unit(c(0.2, 0, 0, 0), "npc"))

ggarrange(
    p_1,
    p_2,
    p_3,
    labels = c("A", "B", "C"),
    nrow = 3,
    common.legend = T,
    legend = "top",
    label.x = 0,
    label.y = 1,
    font.label = list(size = 18)
)
