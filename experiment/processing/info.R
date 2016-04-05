source("common.R")

participants <- read_participants()

pctsummary <- function(x) {
	sprintf("%d/%d = %5.1f%%", sum(x), length(x), 100*mean(x))
}

format_minutes <- function(x) {
	sprintf("%dm%04.1fs", x %/% 60, x %% 60)
}

median_time_summary <- function(x) {
	format_minutes(median(x, na.rm=T))
}

cat(sprintf("number of participants: %d (%d good, %d bad)\n",
	length(participants$good), sum(participants$good), sum(!participants$good)))

# From now on we use only the good participants.
participants <- filter_participants(participants)

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


cat("\n")
cat(sprintf("median time to success per condition\n"))
with(participants, aggregate(time_to_success, list(env=env, version=version), median_time_summary))
