# https://emcrit.org/pulmcrit/mythbusting-sensitivity-specificity/

library(ggplot2)


evidence_strength_plot <- function(sens, spec) {
  break_1 <- .75
  break_2 <- .90909

  plr <- round(sens/(1 - spec), 2)
  nlr <- round((1 - sens)/spec, 2)

  point <- data.frame(sens = sens,
                      spec = spec,
                      label = paste0("+LR = ", plr, ", -LR = ", nlr))

  data.frame(
    x = c(0, 0, 1, 0, break_1, 1, 0, break_2, 1, break_1, 0, 1, 1, break_2),
    y = c(1, 0, 0, 1, break_1, 0, 1, break_2, 0, break_1, 1, 1, 0, break_2),
    id = factor(
      c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),
      levels = c(1, 2, 3, 4),
      labels = c(
        "Misleading",
        "Can provide weak evidence",
        "Can provide moderate evidence",
        "Can provide strong evidence"
      )
    )
  ) -> coords

  ggplot() +
    scale_y_continuous(
      limits = c(0, 1),
      name = "Specificity",
      labels = scales::percent,
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      name = "Sensitivity",
      labels = scales::percent,
      expand = c(0, 0)
    ) +
    geom_polygon(aes(
      x = x,
      y = y,
      group = id,
      fill = id
    ), data = coords) +
    scale_fill_manual(
      guide = guide_legend(title = NULL),
      values = c("#FF0400", "#FFFF00", "#FFC000", "#68FF66")
    ) +
    geom_point(aes(
      x = sens,
      y = spec),
      size = 4,
      data = point,
      show.legend = FALSE
    ) +
    geom_label_repel(aes(x = sens, y = spec, label = label), data = point) +
    labs(title = "Classification of tests based on strongest level of evidence that they can provide")
}
