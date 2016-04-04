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
