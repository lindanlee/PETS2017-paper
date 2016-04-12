library(ggplot2)

# Got these values by:
#   \the\textwidth   % 483.6967pt
#   \the\textheight  % 661.97234pt
#   \the\columnwidth % 236.1578pt
textwidth <- 483.6967 / 72.27
textheight <- 661.97234 / 72.27
columnwidth <- 236.1578 / 72.27

# Read all the participant CSV files into one big data frame.
read_participants <- function() {
	participants <- read.csv("participants.csv")
	sessions <- read.csv("sessions.csv")
	success <- read.csv("success.csv")

	participants <- merge(participants, sessions, by=c("session"), all=T)
	participants <- merge(participants, success, by=c("seat", "runid"), all=T)
	participants
}

# Return only the participants marked "good".
filter_participants <- function(participants) {
	participants[participants$good, ]
}

# Read edges.csv into one big data frame.
read_edges <- function() {
	read.csv("edges.csv")
}

common_theme <- function(p) {
	p <- p + theme_minimal()
	p <- p + theme(text=element_text(size=8))
	p <- p + theme(plot.margin=margin(0,0,0,0,"mm"))
	p
}
