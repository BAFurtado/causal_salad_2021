# Originally by Gregor Mathes https://gregor-mathes.netlify.app/2021/02/02/rethinking-chapter-6/
# There are conflicts on using the libraries... Python has namespaces that fixes this, I guess.

library(tidyverse)
library(rethinking)
library(ggdag)
library(dagitty)

tribble(
  ~ name,  ~ x,  ~ y,
    "A",    1,     3,
    "U",    0,     2,
    "C",    2,     2,
    "V",    3,     1,
    "B",    1,     1,
    "X",    0,     0,
    "Y",    2,     0
) %>%
  dagify(
    Y ~ X + C + V,
         C ~ V + A,
         B ~ C + U,
         U ~ A,
         X ~ U,
         coords = .) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(internal_colour = "blue", alpha = 0.8, colour = "white") +
  geom_dag_text(aes(label = name), color = "brown", size = 5) +
  geom_dag_edges(edge_color = "brown") +
  labs(caption = "Figure 1: Adjusted DAG from the chapter, including V.") +
  theme_void()