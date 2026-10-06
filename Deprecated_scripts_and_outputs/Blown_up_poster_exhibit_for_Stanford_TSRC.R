COLOR_NONPOL <- "#6B7280"
COLOR_POL    <- "#3D5A80"

base_theme_clean <- theme_pubr() +
  theme(
    axis.title       = element_text(face = "bold", size = 24),
    plot.title       = element_text(size = 18, face = "bold"),  
    axis.text        = element_text(size = 20),
    legend.title     = element_blank(),
    legend.position  = "top",
    legend.text      = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_vr_compare <- ggplot(desc_compare_vr,
                       aes(x = Civility, y = Mean, fill = ContentType)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65, color = "white", linewidth = 0.3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = position_dodge(width = 0.75),
                width = 0.15, linewidth = 0.5, color = "#333") +
  geom_text(data = ~dplyr::filter(.x, Civility != "Uncivil"),
            aes(y = CI95_upper, label = sprintf("%.1f%%", Mean * 100)),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 7.0, color = "#444") +
  scale_fill_manual(values = c("Non-political" = COLOR_NONPOL, "Political" = COLOR_POL),
                    labels = c("Non-political baseline",
                               "Political content (avg. of aligned + opposed)")) +
  scale_y_continuous(limits = c(0, 1.10),
                     breaks = c(0, 0.25, 0.5, 0.75, 1.0),
                     labels = c("0%", "25%", "50%", "75%", "100%"),
                     expand = c(0, 0)) +
  labs(title = "Political vs. Non-political: Violation Recognition",
       x = "Civility Level", y = "Violation Recognition Rate") +
  base_theme_clean

p_es_compare <- ggplot(desc_compare_es,
                       aes(x = Civility, y = Mean, fill = ContentType)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65, color = "white", linewidth = 0.3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = position_dodge(width = 0.75),
                width = 0.15, linewidth = 0.5, color = "#333") +
  geom_text(data = ~dplyr::filter(.x, Civility != "Uncivil"),
            aes(y = CI95_upper, label = sprintf("%.2f", Mean)),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 7.0, color = "#444") +
  scale_fill_manual(values = c("Non-political" = COLOR_NONPOL, "Political" = COLOR_POL),
                    labels = c("Non-political baseline",
                               "Political content (avg. of aligned + opposed)")) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5), expand = c(0, 0)) +
  labs(title = "Political vs. Non-political: Enforcement Severity",
       x = "Civility Level", y = "Enforcement Severity (0-4)") +
  base_theme_clean

combined_compare <- (p_vr_compare | p_es_compare) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(
  "Graph_output_results/Political_vs_NonPolitical_Combined_poster.png",
  combined_compare,
  width  = 14.6,
  height = 6.1,
  dpi    = 300,
  bg     = "white"
)