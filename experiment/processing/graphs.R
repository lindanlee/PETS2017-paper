library(ggplot2)
library(scales)
library(RColorBrewer)

source("common.R")

##################
# INITIALIZATION #
##################

height <- 1.5

participants <- filter_participants(read_participants())
participants$label <- factor(sprintf("%s-%s", participants$env, participants$version), levels=c("E1-NEW", "E1-OLD", "E2-NEW", "E2-OLD", "E3-NEW", "E3-OLD"))
participants$pid <- factor(sprintf("%s-%s-%s-%s", participants$env, participants$version, participants$seat, participants$session))
userid_order <- rev(order(participants$env, participants$version, participants$time_to_success))
participants$userid <- sprintf("P%d", participants$userid)
participants$userid <- factor(participants$userid, levels=participants$userid[userid_order])

edges <- filter_edges(read_edges(), participants)
if (any(is.na(edges$userid))) {
  stop("found NAs in edges$userid")
}
# Ignore "not_running" and "starting", so they just show up as blank.
edges <- droplevels(edges[!(edges$dst %in% c("not_running", "starting")), ])

state.palette <- brewer.pal(length(levels(edges$dst)), "Set1")

###################
# TIME TO SUCCESS #
###################

time_to_success_plot <- function(participants) {
  minutes_to_success <- participants$time_to_success/60
  max_minutes <- max(minutes_to_success, na.rm=T)+9
  p <- ggplot(data.frame(participants, minutes_to_success), aes(x=label, y=minutes_to_success), environment=environment())
  p <- p + geom_boxplot(color="gray70", outlier.size=0)
  p <- p + geom_point(alpha=0.6, aes(size=success, color=success, shape=success))
  p <- p + scale_size_manual(values=c("TRUE"=0.8, "FALSE"=1.0), guide=F)
  p <- p + scale_color_manual(values=c("TRUE"="black", "FALSE"="red3"), guide=F)
  p <- p + scale_shape_manual(values=c("TRUE"=19, "FALSE"=4), guide=F)
  p <- p + coord_cartesian(ylim=c(0, max_minutes))
  p <- p + coord_flip()
  p <- p + scale_x_discrete(limits=rev(levels(participants$label)))
  p <- p + labs(title=NULL, x=NULL, y="Minutes to success")
  counts <- aggregate(data.frame(participants, n=1)[, c("success", "n")], by=list(label=participants$label), sum)
  p <- p + geom_text(data=counts, aes(x=label, y=max_minutes, label=sprintf("%d/%d DNF", n-success, n)), hjust=1, size=2, alpha=0.6)
  p <- common_theme(p)
  p <- p + theme(panel.grid.major.y=element_blank())
  p
}

p <- time_to_success_plot(participants)
ggsave("time_to_success.pdf", p, width=columnwidth, height=height, device=cairo_pdf)

p <- time_to_success_plot(clamp_time_to_success(participants, maxtime))
ggsave("time_to_success_clamped.pdf", p, width=columnwidth, height=height, device=cairo_pdf)


# Assign each environment a color (E1=blue, E2=orange, E3=green), with NEW
# being more saturated and OLD being less saturated.
palette <- c(
	brewer.pal(3, "Blues")[c(3,2)],
	brewer.pal(3, "Oranges")[c(3,2)],
	brewer.pal(3, "BuGn")[c(3,2)]
)
names(palette) <- levels(participants$label)

# Artificially add a success time that is way off the scale for all
# participants that were not successful. Otherwise the ecdf will ignore the NAs
# and cause all the curves to rise to 100%. The artificial points will be
# hidden off the edge of the graph by coord_cartesian.
minutes_to_success <- pmin(participants$time_to_success, maxtime*2, na.rm=T) / 60
p <- ggplot(participants, aes(x=minutes_to_success, color=label))
p <- p + geom_step(stat="ecdf", size=0.5)
p <- p + coord_cartesian(xlim=c(0, maxtime)/60)
p <- p + scale_y_continuous(labels=percent)
p <- p + scale_color_manual(values=palette)
# p <- p + guides(color=guide_legend(override.aes=list(size=2, title=NULL)))
p <- p + guides(color=F)
p <- p + labs(title=NULL, x="Minutes elapsed", y="Successful participants")
p <- p + annotate(geom="text", x=c(1, 3.3, 14, 15.3, 23, 24.7), y=c(0.86, 0.87, 0.91, 0.71, 0.61, 0.32), label=c("E1-\nNEW", "E1-OLD", "E2-NEW", "E2-OLD", "E3-NEW", "E3-OLD"), hjust=c(1, 0, 0, 0, 0, 0), vjust=0, size=2, lineheight=0.8)
p <- common_theme(p)
ggsave("time_to_success_ecdf.pdf", p, width=columnwidth, height=height, device=cairo_pdf)

# GRAPH TIME ON EACH SCREEN 

state.palette <- brewer.pal(length(levels(edges$dst)), "Set1")
data <- aggregate(edges$duration, list(env=edges$env, version=edges$version, seat=edges$seat, runid=edges$runid, dst=edges$dst), FUN=sum)
p <- ggplot(data, aes(x=dst, y=x/60, color=dst))
p <- p + geom_boxplot(outlier.size=0)
p <- p + geom_point(size=1.0, alpha=0.6)
p <- p + scale_color_manual(values=state.palette, guide=F)
p <- p + scale_x_discrete("Current screen", labels=c(
  "first"="F",
  "bridges"="B1",
  "bridgeSettings"="B2\nB",
  "bridgeHelp"="BH",
  "proxy"="P1",
  "proxyYES"="P2\nP",
  "summary"="S",
  "progress"="Pr",
  "error"="E"
))
p <- p + coord_flip()
p <- common_theme(p)
p <- p + facet_grid(version ~ env)
ggsave("time_per_screen.pdf", p, width=textwidth, height=3, device=cairo_pdf)

#############
# ALL EDGES #
#############

p <- ggplot()
p <- p + geom_segment(data=participants, size=0.2, color="black", aes(x=userid, xend=userid, y=0, yend=ifelse(!is.na(time_to_success), time_to_success, maxtime)/60))
p <- p + geom_segment(data=edges, size=1.5, lineend="butt", aes(x=userid, xend=userid, y=time_from_start/60, yend=(time_from_start+duration)/60, color=dst))
# p <- p + geom_point(color="black", size=1.5, shape="|")
p <- p + geom_point(data=participants[is.na(participants$time_to_success), ], aes(x=userid, y=maxtime/60), shape=4)
p <- p + coord_flip()
p <- p + scale_y_continuous(breaks=pretty_breaks(n=10))
p <- p + scale_color_manual("Current screen", values=state.palette, labels=c(
	"first"="first (F)",
	"bridges"="bridge yes/no (B1)",
	"bridgeSettings"="bridge settings (B2/B)",
	"bridgeHelp"="bridge help (BH)",
	"proxy"="proxy yes/no (P1)",
	"proxyYES"="proxy settings (P2/P)",
	"summary"="summary (S)",
	"progress"="progress (Pr)",
	"error"="error"
))
p <- p + labs(title=NULL, x=NULL, y="Minutes elapsed")
p <- common_theme(p)
p <- p + theme(panel.grid.minor.x=element_blank())
p <- p + theme(panel.grid.major.y=element_blank())
p <- p + theme(axis.text.y=element_text(color="gray70"))
p
ggsave("all-participant-edges.pdf", p, width=textwidth, height=textheight-0.75, device=cairo_pdf)
