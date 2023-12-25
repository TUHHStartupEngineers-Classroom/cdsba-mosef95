library(tidyverse)
library(dagitty)
library(ggdag)
# Confounder
confounding <- dagify(
  X ~ Z,
  Y ~ Z,
  Y ~ X,
  coords = list(x = c(Y = 3, Z = 2, X = 1),
                y = c(Y = 0, Z = 1, X = 0)),
  labels = list(X = "Parking spot available",
                Y = "Sales",
                Z = "Customers")
)

# Plot DAG
ggdag(confounding) +
  #geom_dag_point(color = ggthemr::swatch()[2]) +
  geom_dag_text(color = "white") +
  geom_dag_edges(edge_color = "white") +
  geom_dag_label_repel(aes(label = label))