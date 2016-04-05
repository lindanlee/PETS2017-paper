source("common.R")

participants <- read_participants()

format_percent <- function(x) {
	sprintf("%5.1f%%", 100*x)
}

pctsummary <- function(x) {
	sprintf("%d/%d = %s", sum(x), length(x), format_percent(mean(x)))
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


time_ecdf <- function(participants, env, version) {
	ecdf(
		ifelse(!is.na(participants$time_to_success),
			participants$time_to_success,
			2*max(participants$time_to_success, na.rm=T)
		)[participants$env==env & participants$version==version]
	)
}

ecdf.e1.new <- time_ecdf(participants, "E1", "NEW")
ecdf.e1.old <- time_ecdf(participants, "E1", "OLD")
ecdf.e2.new <- time_ecdf(participants, "E2", "NEW")
ecdf.e2.old <- time_ecdf(participants, "E2", "OLD")
ecdf.e3.new <- time_ecdf(participants, "E3", "NEW")
ecdf.e3.old <- time_ecdf(participants, "E3", "OLD")

cat("\n")
max.e1.new <- max(participants$time_to_success[participants$env=="E1" & participants$version=="NEW"])
cat(sprintf("maximum time for E1-NEW: %s\n", format_minutes(max.e1.new)))
for (t in c(max.e1.new, 90, 10*60, 20*60)) {
	cat("\n")
	cat(sprintf("success rate of E1-NEW after %s: %s\n", format_minutes(t), format_percent(ecdf.e1.new(t))))
	cat(sprintf("success rate of E1-OLD after %s: %s\n", format_minutes(t), format_percent(ecdf.e1.old(t))))
	cat(sprintf("success rate of E2-NEW after %s: %s\n", format_minutes(t), format_percent(ecdf.e2.new(t))))
	cat(sprintf("success rate of E2-OLD after %s: %s\n", format_minutes(t), format_percent(ecdf.e2.old(t))))
	cat(sprintf("success rate of E3-NEW after %s: %s\n", format_minutes(t), format_percent(ecdf.e3.new(t))))
	cat(sprintf("success rate of E3-OLD after %s: %s\n", format_minutes(t), format_percent(ecdf.e3.old(t))))
}
