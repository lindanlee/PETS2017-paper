library(ggplot2)

options(width=160)

participants <- read.csv("participants.csv")
sessions <- read.csv("sessions.csv")
success <- read.csv("success.csv")

participants <- merge(participants, sessions, by=c("session"), all=T)
participants <- merge(participants, success, by=c("seat", "runid"), all=T)

pctsummary <- function(x) {
	sprintf("%d/%d = %5.1f%%", sum(x), length(x), 100*mean(x))
}

# Assign DNFs a maximum time_to_success.
cap_time_to_success <- function(participants, maxtime) {
	df <- data.frame(participants)
	df$time_to_success[is.na(df$time_to_success)] <- maxtime
	df
}

cat(sprintf("number of participants: %d (%d good, %d bad)\n",
	length(participants$good), sum(participants$good), sum(!participants$good)))

# From now on we use only the good participants.
participants <- participants[participants$good, ]

p <- ggplot(cap_time_to_success(participants, 40*60), aes(x=sprintf("%s-%s", env, version), y=time_to_success, color=success))
p <- p + geom_point(size=1, alpha=0.6, position=position_jitter(width=0.2))
p <- p + coord_flip()
p <- p + xlab("environment and version")
p <- p + theme_minimal()
ggsave("time_to_success.pdf", p, width=5, height=4)

# participants

cat("\n")
cat(sprintf("number of good participants per condition\n"))
with(participants, aggregate(success, list(env=env, version=version), length))

cat("\n")
cat(sprintf("success rate per condition\n"))
with(participants, aggregate(success, list(env=env, version=version), pctsummary))

cat("\n")
cat(sprintf("success rate per condition per pool\n"))
with(participants, aggregate(success, list(env=env, version=version, pool=pool), pctsummary))

cat("\n")
cat(sprintf("ANOVA of success by env and version\n"))
summary(aov(success ~ env+version, data=participants))

for (env in levels(participants$env)) {
	cat("\n")
	cat(sprintf("t-test of success rate in env %s\n", as.character(env)))
	try(print(with(participants[participants$env==env, ], t.test(success[version=="NEW"], success[version=="OLD"]))))
}

cat("\n")
cat(sprintf("ANOVA of time_to_success by env and version\n"))
summary(aov(time_to_success ~ env+version, data=participants))

for (env in levels(participants$env)) {
	cat("\n")
	cat(sprintf("t-test of time_to_success in env %s\n", as.character(env)))
	try(print(with(participants[participants$env==env, ], t.test(time_to_success[version=="NEW"], time_to_success[version=="OLD"]))))
}
