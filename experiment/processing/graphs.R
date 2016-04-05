library(ggplot2)

source("common.R")

height <- 1.5

# Assign DNFs a maximum time_to_success.
clamp_time_to_success <- function(participants, maxtime) {
	df <- data.frame(participants)
	df$time_to_success[is.na(df$time_to_success)] <- maxtime
	df
}

time_to_success_plot <- function(participants) {
	minutes_to_success <- participants$time_to_success/60
	p <- ggplot(data.frame(participants, minutes_to_success), aes(x=label, y=minutes_to_success), environment=environment())
	p <- p + geom_boxplot(color="gray70", outlier.size=0)
	p <- p + geom_point(size=0.8, alpha=0.6)
	p <- p + coord_flip()
	p <- p + scale_y_continuous(limits=c(0, max(minutes_to_success, na.rm=T)+9))
	p <- p + labs(title=NULL, x=NULL, y="Minutes to first success")
	counts <- aggregate(data.frame(participants, n=1)[, c("success", "n")], by=list(label=participants$label), sum)
	p <- p + geom_text(data=counts, aes(x=label, y=max(minutes_to_success, na.rm=T)+9, label=sprintf("%d/%d DNF", n-success, n)), hjust=1, size=2, alpha=0.6)
	p <- common_theme(p)
	p
}

participants <- filter_participants(read_participants())
participants$label <- factor(sprintf("%s-%s", participants$env, participants$version), levels=c("E3-OLD", "E3-NEW", "E2-OLD", "E2-NEW", "E1-OLD", "E1-NEW"))

p <- time_to_success_plot(participants)
ggsave("time_to_success.pdf", p, width=textwidth, height=height)

p <- time_to_success_plot(clamp_time_to_success(participants, 40*60))
ggsave("time_to_success_clamped.pdf", p, width=textwidth, height=height)
