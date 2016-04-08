library(ggplot2)

# From template.cls:
# %%% 2)  Side and top margins of 4.5pc, bottom margin of 6pc, column gutter of
# %%%     2pc, hence columns are 20pc wide and 55.5pc tall.  (6pc = 1in, approx)
textwidth <- 20 / 6

# Read all the participant CSV files into one big data frame.
read_participants <- function() {
	participants <- read.csv("participants.csv")
	sessions <- read.csv("sessions.csv")
	success <- read.csv("success.csv")
	edges <- read.csv("edges.csv")

	participants <- merge(participants, sessions, by=c("session"), all=T)
	participants <- merge(participants, success, by=c("seat", "runid"), all=T)
	participants.edges <- merge(participants, edges, by=c("seat","runid"), all.y=T)
	participants.edges.order = participants.edges[order(
	  participants.edges$env,participants.edges$version,participants.edges$time_to_success),]
	participants
}

# Return only the participants marked "good".
filter_participants <- function(participants) {
	participants[participants$good, ]
}

common_theme <- function(p) {
	p <- p + theme_minimal()
	p <- p + theme(text=element_text(size=8))
	p <- p + theme(plot.margin=margin(0,0,0,0,"mm"))
	p
}
