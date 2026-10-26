# Exercise rendering, not just namespace loading: ggproto defaults serialized
# against ggplot2 4 can load but fail when rendered under ggplot2 3.
stopifnot(getRversion() >= "4.4", getRversion() < "4.5")
packages <- c("ggplot2", "ggforce", "ggrepel", "ggalluvial", "ggraph")
versions <- vapply(packages, function(p) as.character(utils::packageVersion(p)), character(1))
print(versions)
if (utils::packageVersion("ggplot2") >= "4.0.0") {
  stop("Bioconductor 3.20 requires the configured pre-ggplot2-4 CRAN snapshot.")
}
points <- data.frame(x = 1:3, y = 3:1, label = letters[1:3])
plots <- list(
  point = ggplot2::ggplot(points, ggplot2::aes(x, y, colour = label)) + ggplot2::geom_point(),
  label = ggplot2::ggplot(points, ggplot2::aes(x, y, label = label, colour = label)) +
    ggrepel::geom_text_repel(),
  circle = ggplot2::ggplot(data.frame(x = 0, y = 0, r = 1, group = "circle")) +
    ggforce::geom_circle(ggplot2::aes(x0 = x, y0 = y, r = r, fill = group)),
  alluvial = ggplot2::ggplot(data.frame(first = c("A", "B"), last = c("C", "D"), n = 1),
                           ggplot2::aes(axis1 = first, axis2 = last, y = n)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = first)) + ggalluvial::geom_stratum(),
  network = ggraph::ggraph(igraph::make_ring(3), layout = "circle") +
    ggraph::geom_edge_link() + ggraph::geom_node_point()
)
local({
  figure <- tempfile(fileext = ".pdf")
  grDevices::pdf(figure)
  on.exit({ grDevices::dev.off(); unlink(figure) }, add = TRUE)
  for (name in names(plots)) {
    grid::grid.newpage()
    grid::grid.draw(ggplot2::ggplotGrob(plots[[name]]))
    message("R 4.4 plotting compatibility passed: ", name)
  }
})
