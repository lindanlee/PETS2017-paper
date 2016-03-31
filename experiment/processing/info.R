options(width=160)

participants <- read.csv("participants.csv")
sessions <- read.csv("sessions.csv")
success <- read.csv("success.csv")

participants <- merge(participants, sessions, by=c("session"), all=T)
participants <- merge(participants, success, by=c("seat", "runid"), all=T)

pctsummary <- function(x) {
	sprintf("%d/%d = %5.1f%%", sum(x), length(x), 100*mean(x))
}

cat(sprintf("number of participants: %d (%d good, %d bad)\n",
	length(participants$good), sum(participants$good), sum(!participants$good)))

# From now on we use only the good participants.
participants <- participants[participants$good, ]

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
	cat(sprintf("t-test of means in env %s\n", as.character(env)))
	try(print(with(participants[participants$env==env, ], t.test(success[version=="NEW"], success[version=="OLD"]))))
}
