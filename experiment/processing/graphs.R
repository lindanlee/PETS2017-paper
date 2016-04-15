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
participants$pid <- factor(sprintf("%s-%s-%s-%s", participants$env, participants$version, participants$seat, participants$runid))
participants$pid <- factor(participants$pid, levels=participants$pid[order(participants$env, participants$version, participants$time_to_success)])

# This is the length of the experiment, after which to cut off measurements.
# It's actually just a few seconds after 40 minutes, because one E3 just
# squeaked by.
maxtime <- max(40*60, participants$time_to_success, na.rm=T)

edges <- read_edges()
edges <- merge(edges, participants, by=c("seat", "runid"), all.y=T)
if (any(is.na(edges$pid))) {
  stop("found NAs in edges$pid")
}

###################
# TIME TO SUCCESS #
###################

# Assign DNFs a maximum time_to_success.
clamp_time_to_success <- function(participants, maxtime) {
  df <- data.frame(participants)
  df$time_to_success[is.na(df$time_to_success)] <- maxtime
  df
}

time_to_success_plot <- function(participants) {
  minutes_to_success <- participants$time_to_success/60
  max_minutes <- max(minutes_to_success, na.rm=T)+9
  p <- ggplot(data.frame(participants, minutes_to_success), aes(x=label, y=minutes_to_success), environment=environment())
  p <- p + geom_boxplot(color="gray70", outlier.size=0)
  p <- p + geom_point(size=0.8, alpha=0.6)
  p <- p + coord_cartesian(ylim=c(0, max_minutes))
  p <- p + coord_flip()
  p <- p + scale_x_discrete(limits=rev(levels(participants$label)))
  p <- p + labs(title=NULL, x=NULL, y="Minutes to first success")
  counts <- aggregate(data.frame(participants, n=1)[, c("success", "n")], by=list(label=participants$label), sum)
  p <- p + geom_text(data=counts, aes(x=label, y=max_minutes, label=sprintf("%d/%d DNF", n-success, n)), hjust=1, size=2, alpha=0.6)
  p <- common_theme(p)
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

##################
# SUMMARY SCREEN # (HACKY)
##################

summary_edges <- edges[edges[,"src"]== "summary",]

# % time on summary screen = time spent on summary screen / total time spent on screens * 100 
sum(summary_edges$duration)/sum(edges$duration)*100

# dst from summary distribution
sum(summary_edges$dst == "bridgeSettings")/nrow(summary_edges)*100
sum(summary_edges$dst == "progress")/nrow(summary_edges)*100
sum(summary_edges$dst == "summary")/nrow(summary_edges)*100
barplot(height=table(summary_edges$dst))

################
# PROXY SCREEN # (HACKY)
################

proxy_edges <- edges[edges[,"src"] %in% c("proxy","proxyYES"),]

# % time on proxy screen (overall) = time spent on proxy screen / total time spent on screens * 100 
sum(proxy_edges$duration)/sum(edges$duration)*100

# e1 % on proxy screen (overall)
sum(proxy_edges[proxy_edges[,"env"]=="E1",]$duration)/sum(edges[edges[,"env"]=="E1",]$duration)*100

# e2 % on proxy screen (overall)
sum(proxy_edges[proxy_edges[,"env"]=="E2",]$duration)/sum(edges[edges[,"env"]=="E2",]$duration)*100

# e3 % on proxy screen (overall)
sum(proxy_edges[proxy_edges[,"env"]=="E3",]$duration)/sum(edges[edges[,"env"]=="E3",]$duration)*100

# new % on proxy screen (overall)
sum(proxy_edges[proxy_edges[,"version"]=="NEW",]$duration)/sum(edges[edges[,"version"]=="NEW",]$duration)*100

# old % on proxy screen (overall)
sum(proxy_edges[proxy_edges[,"version"]=="OLD",]$duration)/sum(edges[edges[,"version"]=="OLD",]$duration)*100


# "active time" is anytime that people are not on the progress screen.
active_edges <- edges[edges[,"src"] != "progress",]

# % time on proxy screen ("active time") = time spent on proxy screen / total time spent on screens * 100 
sum(proxy_edges$duration)/sum(active_edges$duration)*100

# e1 % on proxy screen ("active time")
sum(proxy_edges[proxy_edges[,"env"]=="E1",]$duration)/sum(active_edges[active_edges[,"env"]=="E1",]$duration)*100

# e2 % on proxy screen ("active time")
sum(proxy_edges[proxy_edges[,"env"]=="E2",]$duration)/sum(active_edges[active_edges[,"env"]=="E2",]$duration)*100

# e3 % on proxy screen ("active time")
sum(proxy_edges[proxy_edges[,"env"]=="E3",]$duration)/sum(active_edges[active_edges[,"env"]=="E3",]$duration)*100

# new % on proxy screen ("active time")
sum(proxy_edges[proxy_edges[,"version"]=="NEW",]$duration)/sum(active_edges[active_edges[,"version"]=="NEW",]$duration)*100

# old % on proxy screen ("active time")
sum(proxy_edges[proxy_edges[,"version"]=="OLD",]$duration)/sum(active_edges[active_edges[,"version"]=="OLD",]$duration)*100

###################
# PROGRESS SCREEN # (HACKY)
###################

progress_edges <- edges[edges[,"src"]== "progress",]

# % time on progress screen = time spent on progress screen / total time spent on screens * 100
sum(progress_edges$duration)/sum(edges$duration)*100

# e1 % on progress screen
sum(progress_edges[progress_edges[,"env"]=="E1",]$duration)/sum(edges[edges[,"env"]=="E1",]$duration)*100

# e2 % on progress screen
sum(progress_edges[progress_edges[,"env"]=="E2",]$duration)/sum(edges[edges[,"env"]=="E2",]$duration)*100

# e3 % on progress screen
sum(progress_edges[progress_edges[,"env"]=="E3",]$duration)/sum(edges[edges[,"env"]=="E3",]$duration)*100

# new % on progress screen
sum(progress_edges[progress_edges[,"version"]=="NEW",]$duration)/sum(edges[edges[,"version"]=="NEW",]$duration)*100

# old % on progress screen
sum(progress_edges[progress_edges[,"version"]=="OLD",]$duration)/sum(edges[edges[,"version"]=="OLD",]$duration)*100

#############
# ALL EDGES #
#############

# Canonicalize screen names from the OLD and NEW interfaces.
canonicalize_screens <- function(version, x) {
	y <- factor(x)
	# In the NEW interface, the screen that's logged as "bridges"
	# corresponds more closely to the "bridgeSettings" screen in the OLD
	# interface than it does the OLD "bridges" screen. Similarly NEW
	# "proxy" corresponds to OLD "proxyYES".
	y[version=="NEW" & y=="bridges"] <- "bridgeSettings"
	y[version=="NEW" & y=="proxy"] <- "proxyYES"
	levels(y) <- list(
		"not_running"="not_running",
		"starting"="starting",
		"first"="first",
		"bridges"="bridges",
		"bridgeSettings"="bridgeSettings",
		"bridgeHelp"="bridgeHelp",
		"proxy"="proxy",
		"proxyYES"="proxyYES",
		"summary"="summary",
		"progress"=c("progress_bar", "inlineprogress"),
		"error"="errorPanel"
	)
	droplevels(y)
}

# Remove edges that start after maxtime, and trim those that overlap it
# so their start time plus their duration exactly reaches maxtime.
trim_edges <- function(edges, maxtime) {
	edges <- edges[edges$time_from_start <= maxtime, ]
	edges$duration <- ifelse(edges$time_from_start + edges$duration > maxtime, maxtime - edges$time_from_start, edges$duration)
	edges
}

edges <- edges[order(edges$sequence, edges$time_from_start), ]
edges$duration <- ave(edges$time_from_start, edges$seat, edges$runid, FUN=function(z) {
	c(z[2:length(z)], z[length(z)]) - z
})
# Keep only the edges up to the first success.
edges <- edges[is.na(edges$time_to_success) | edges$time_from_start <= edges$time_to_success, ]
# Cut off the logs at our time limit.
edges <- trim_edges(edges, maxtime)
# Ignore "not_running" and "starting", so they just show up as blank.
edges <- edges[!(edges$dst %in% c("not_running", "starting")), ]
edges$src <- canonicalize_screens(edges$version, edges$src)
edges$dst <- canonicalize_screens(edges$version, edges$dst)

state.palette <- brewer.pal(length(levels(edges$dst)), "Set1")

p <- ggplot()
p <- p + geom_segment(data=participants, size=0.2, color="black", aes(x=pid, xend=pid, y=0, yend=ifelse(!is.na(time_to_success), time_to_success, maxtime)/60))
p <- p + geom_segment(data=edges, size=1.5, lineend="butt", aes(x=pid, xend=pid, y=time_from_start/60, yend=(time_from_start+duration)/60, color=dst))
# p <- p + geom_point(color="black", size=1.5, shape="|")
p <- p + geom_point(data=participants[is.na(participants$time_to_success), ], aes(x=pid, y=maxtime/60), shape=4)
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
p <- p + labs(title=NULL, x="Participants", y="Minutes elapsed")
p <- common_theme(p)
p <- p + theme(panel.grid.minor.x=element_blank())
p <- p + theme(panel.grid.major.y=element_blank())
ggsave("all-participant-edges.pdf", p, width=textwidth, height=textheight-0.75, device=cairo_pdf)
