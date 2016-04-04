library(ggplot2)

source("common.R")

# Assign DNFs a maximum time_to_success.
cap_time_to_success <- function(participants, maxtime) {
	df <- data.frame(participants)
	df$time_to_success[is.na(df$time_to_success)] <- maxtime
	df
}

participants <- filter_participants(read_participants())

p <- ggplot(cap_time_to_success(participants, 40*60), aes(x=sprintf("%s-%s", env, version), y=time_to_success, color=success))
p <- p + geom_point(size=1, alpha=0.6, position=position_jitter(width=0.2))
p <- p + coord_flip()
p <- p + xlab("environment and version")
p <- p + theme_minimal()
ggsave("time_to_success.pdf", p, width=5, height=4)
