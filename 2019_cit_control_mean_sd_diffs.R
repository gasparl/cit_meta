library('neatStats')
library('bayestestR')

calc_diffs_aucs = function(g_sd_lower,
                           i_sd_lower,
                           g_sd_upper,
                           i_sd_upper,
                           m_g_diff,
                           m_g_avg = 37.16998) {
    m_g_lower =  m_g_avg - m_g_diff / 2
    m_g_upper = m_g_avg + m_g_diff / 2
    # smaller g-i difference but smaller SDs:
    g_1 = distribution_normal(n = 1000, sd = g_sd_lower, mean = m_g_lower)
    i_1 = distribution_normal(n = 1000, sd = i_sd_lower, mean = 0)

    # larger g-i difference but larger SDs:
    g_2 = distribution_normal(n = 1000, sd = g_sd_upper, mean = m_g_upper)
    i_2 = distribution_normal(n = 1000, sd = i_sd_upper, mean = 0)


    cat('\n1. Smaller guilty-innocent difference but smaller SDs:', fill = T)
    cat('- Guilty mean:',
        m_g_lower,
        ' Guilty SD:',
        g_sd_lower,
        ' Innocent SD:',
        i_sd_lower,
        fill = T)
    fig1 = t_neat(
        g_1,
        i_1,
        auc_added = T,
        bf_added = F,
        plot_densities = T
    )$density_plot +
        scale_x_continuous(limits = c(-100, 150),
                           breaks = seq(-50, 50, by = 50)) + scale_y_continuous(limits = c(0, 0.026))

    cat('\n2. Larger guilty-innocent difference but larger SDs:', fill = T)
    cat('- Guilty mean:',
        m_g_upper,
        ' Guilty SD:',
        g_sd_upper,
        ' Innocent SD:',
        i_sd_upper,
        fill = T)
    fig2 = t_neat(
        g_2,
        i_2,
        auc_added = T,
        bf_added = F,
        plot_densities = T
    )$density_plot  +    scale_x_continuous(limits = c(-100, 150),
                                            breaks = seq(-50, 50, by = 50)) +  scale_y_continuous(limits = c(0, 0.026))

    cat('\nDifferences between two guilty groups:', fill = T)
    t_neat(g_1, g_2, bf_added = F)

    show(ggpubr::ggarrange(
        fig1,
        fig2,
        ncol = 1,
        nrow = 2,
        common.legend = T,
        labels = c('A', 'B'), hjust = -3, vjust = 2.5
    ))
}


calc_diffs_aucs(
    g_sd_lower = 19.87472,
    i_sd_lower = 15.23694,
    g_sd_upper = 43.64081,
    i_sd_upper = 29.47704,
    m_g_diff = 20
)


calc_diffs_aucs(
    g_sd_lower = 29.08648,
    i_sd_lower = 20.32164,
    g_sd_upper = 37.86013,
    i_sd_upper = 26.04692,
    m_g_diff = 20
)

calc_diffs_aucs(
    g_sd_lower = 29.08648,
    i_sd_lower = 20.32164,
    g_sd_upper = 37.86013,
    i_sd_upper = 26.04692,
    m_g_diff = 10, # 9,
    m_g_avg =  60
)

# g sd mean: 33.4733 CI [29.08648, 37.86013 ]
# g_sd_max = 43.64081
# g_sd_min = 19.87472

# g sd mean: 23.18428 CI [20.32164, 26.04692]
# i_sd_max = 29.47704
# i_sd_min = 15.23694

# m_g_avg = 37.16998
# m_g_diff = 30
