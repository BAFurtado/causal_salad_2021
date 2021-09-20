# from https://gist.github.com/andrewheiss/610830f87e225966abfe9727732bebbd

library(tidyverse)
library(ggdag)
library(patchwork)
library(ggplot2)

data_confounder <- dagify(
  Y ~ Z + X,
  X ~ Z,
  coords = list(x = c(X = 1, Y = 3, Z = 2),
                y = c(X = 1, Y = 1, Z = 2))
) %>%
  tidy_dagitty() %>%
  mutate(arrow_color = ifelse(name == "Z", "#F05B12", "grey80")) %>%
  mutate(node_color = ifelse(name == "Z", TRUE, FALSE))

confounder <- ggplot(data_confounder, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_colour = arrow_color), edge_width = 1) +
  geom_dag_point(aes(color = node_color), size = 12) +
  geom_dag_text(data = filter(data_confounder, name != "Z"), size = 4) +
  geom_dag_text(data = filter(data_confounder, name == "Z"), color = "white", size = 4) +
  scale_color_manual(values = c("grey80", "#F05B12"), guide = "none") +
  coord_cartesian(ylim = c(0.95, 2.05)) +
  labs(title = "Confounder", subtitle = "(Fork)") +
  theme_dag(base_family = "Assistant") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#F05B12"),
        plot.subtitle = element_text(hjust = 0.5, color = "#F05B12"))

data_mediator <- dagify(
  Y ~ X + Z,
  Z ~ X,
  coords = list(x = c(X = 1, Y = 3, Z = 2),
                y = c(X = 1, Y = 1, Z = 2))
) %>%
  tidy_dagitty() %>%
  mutate(arrow_color = case_when(
    name == "Z" & to == "Y" ~ "#30123B",
    name == "X" & to == "Z" ~ "#30123B",
    TRUE ~ "grey80")) %>%
  mutate(node_color = ifelse(name == "Z", TRUE, FALSE))

mediator <- ggplot(data_mediator, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_colour = arrow_color), edge_width = 1) +
  geom_dag_point(aes(color = node_color), size = 12) +
  geom_dag_text(data = filter(data_mediator, name != "Z"), size = 4) +
  geom_dag_text(data = filter(data_mediator, name == "Z"), color = "white", size = 4) +
  scale_color_manual(values = c("grey80", "#30123B"), guide = "none") +
  coord_cartesian(ylim = c(0.95, 2.05)) +
  labs(title = "Mediator", subtitle = "(Chain)") +
  theme_dag(base_family = "Assistant") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#30123B"),
        plot.subtitle = element_text(hjust = 0.5, color = "#30123B"))

data_collider <- dagify(
  Y ~ X,
  Z ~ X + Y,
  coords = list(x = c(X = 1, Y = 3, Z = 2),
                y = c(X = 1, Y = 1, Z = 2))
) %>%
  tidy_dagitty() %>%
  mutate(arrow_color = ifelse(to == "Z", "#7A0403", "grey80")) %>%
  mutate(node_color = ifelse(name == "Z", TRUE, FALSE))

collider <- ggplot(data_collider, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_colour = arrow_color), edge_width = 1) +
  geom_dag_point(aes(color = node_color), size = 12) +
  geom_dag_text(data = filter(data_collider, name != "Z"), size = 4) +
  geom_dag_text(data = filter(data_collider, name == "Z"), color = "white", size = 4) +
  scale_color_manual(values = c("grey80", "#7A0403"), guide = "none") +
  coord_cartesian(ylim = c(0.95, 2.05)) +
  labs(title = "Collider", subtitle = "(Inverted fork)") +
  theme_dag(base_family = "Assistant") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#7A0403"),
        plot.subtitle = element_text(hjust = 0.5, color = "#7A0403"))

combined_associations <- (confounder | plot_spacer() | mediator | plot_spacer() | collider) +
  plot_layout(widths = c(0.32, 0.02, 0.32, 0.02, 0.32))
combined_associations