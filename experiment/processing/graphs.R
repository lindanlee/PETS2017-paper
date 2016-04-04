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
	p <- ggplot(participants, aes(x=label, y=time_to_success/60))
	p <- p + geom_boxplot(color="gray70", outlier.size=0)
	p <- p + geom_point(size=0.8, alpha=0.6)
	p <- p + coord_flip()
	p <- p + labs(title=NULL, x=NULL, y="Minutes to first success")
	p <- common_theme(p)
	p
}

participants <- filter_participants(read_participants())
participants$label <- factor(sprintf("%s-%s", participants$env, participants$version), levels=c("E3-OLD", "E3-NEW", "E2-OLD", "E2-NEW", "E1-OLD", "E1-NEW"))

p <- time_to_success_plot(participants)
ggsave("time_to_success.pdf", p, width=textwidth, height=height)

p <- time_to_success_plot(clamp_time_to_success(participants, 40*60))
ggsave("time_to_success_clamped.pdf", p, width=textwidth, height=height)
