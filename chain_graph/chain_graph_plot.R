source("chain_graph.R")
lay <- rbind(trend = c(0, 2), cosyear = c(-1.2, 0.95), sinyear = c(1.2, 0.95),
             temp = c(-3.6, 0), atemp = c(-1.2, 0), windspeed = c(1.2, 0), hum = c(3.6, 0))[V(g)$name, ]
et <- E(g)$type
png("chain_graph.png", width = 1600, height = 900, res = 150)
par(mar = c(0, 0, 1.5, 0))
plot(g, layout = lay, rescale = FALSE, xlim = c(-4.0, 4.0), ylim = c(-1.1, 2.3), asp = 0,
     vertex.shape = "rectangle", vertex.size = 72, vertex.size2 = 20,
     vertex.color = c("grey90", "mistyrose", "mistyrose", rep("lightblue", 4)),
     vertex.frame.color = "grey40", vertex.label.color = "black", vertex.label.cex = 0.9,
     vertex.label.family = "sans",
     edge.arrow.mode = c(directed = 2, bidirected = 3, undirected = 0)[et],
     edge.lty   = c(directed = 1, bidirected = 2, undirected = 1)[et],
     edge.color = c(directed = "grey55", bidirected = "red", undirected = "blue")[et],
     edge.width = c(directed = 1, bidirected = 2, undirected = 2)[et],
     edge.arrow.size = 0.5,
     edge.curved = ifelse(et == "undirected" & abs(match(ends(g, E(g))[, 1], V(g)$name) - match(ends(g, E(g))[, 2], V(g)$name)) > 1, -0.45, 0),
     main = "Causal chain graph (bike sharing)")
legend("bottomleft", bty = "n", cex = 0.8, lwd = c(1, 2, 2), lty = c(1, 2, 1), col = c("grey55", "red", "blue"),
       legend = c("causal (directed)", "confounded (bidirected)", "mutual interaction (undirected)"))
dev.off()
