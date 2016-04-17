source("common.R")

participants <- read_participants()

format_percent <- function(x) {
	sprintf("%5.1f%%", 100*x)
}

pctsummary <- function(x) {
	sprintf("%d/%d = %s", sum(x), length(x), format_percent(mean(x)))
}

pct <- function(x) {
  mean(x)
}

format_minutes <- function(x) {
	sprintf("%dm%04.1fs", x %/% 60, x %% 60)
}

format_minutes_colons <- function(x) {
	sprintf("%d:%02.f", x %/% 60, x %% 60)
}

median_time_summary <- function(x) {
	format_minutes(median(x, na.rm=T))
}

cat("\n\n\n\n\n\n")
cat("***********\n")
cat("* SUMMARY *\n")
cat("***********\n")

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
cat(sprintf("median time to success per condition\n"))
with(participants, aggregate(time_to_success, list(env=env, version=version), median_time_summary))

cat("\n")
max.e1.new <- max(participants$time_to_success[participants$env=="E1" & participants$version=="NEW"])
cat(sprintf("maximum time for E1-NEW: %s\n", format_minutes(max.e1.new)))

cat("\n\n\n\n\n\n")
cat("*********************\n")
cat("* STATISTICAL TESTS *\n")
cat("*********************\n")

# Mann-Whitney-Wilcox:
    # does not assume a normal distribution.
    # compares 2 groups of independent measures.
    # measures must be continuous (success rates, time to success)

# Kruskal-Wallis: 
    # does not assume a normal distribution. 
    # compares >2 groups of independent measures. 
    # measures must be continuous (success rates, time to success)

# Logistic regression: 
    # is a classifier (can predict success/fail based on ver, env, pool)

# Linear regression: 
    # is a model (can predict time to success based on ver, env, pool)

# x is success rate
success_rates = with(participants, aggregate(success, list(env=env, version=version), pct))

cat("\n")
cat("impact of environment on success rates\n")
kruskal.test(x ~ env, data=success_rates)

cat("impact of version on success rates \n")
wilcox.test(x ~ version, data=success_rates)

cat("\n\n\n\n")
cat("impact of environment on times to completion \n")
kruskal.test(time_to_success ~ env, data=participants)

cat("impact of version on times to completion \n")
wilcox.test(time_to_success ~ version, data=participants)

cat("\n\n\n\n")
cat("logistic regression predicting success by ver, env, pool \n")
#just env, version, pool, and success columns
participants_glm_subset <- subset(participants,select=c(5,6,9,10))
model <- glm(success ~.,family=binomial(link='logit'),data=participants_glm_subset)
summary(model)
#cat("\n\n\n")
#anova(model)

cat("\n\n\n\n")
cat("linear regression predicting time to success by ver, env, pool \n")
#just env, version, pool, and time_to_success columns
participants_lm_subset <- subset(participants,select=c(5,6,9,11))
model <- lm(time_to_success ~.,data=participants_lm_subset)
summary(model)

cat("\n\n\n\n\n\n")
cat("***************\n")
cat("* OTHER STUFF *\n")
cat("***************\n")

cat("\n")
# replace "participants" with "clamp_time_to_success(participants, maxtime)" to get median success times with DNFs = 40mins.
tt <- with(participants, aggregate(success, list(env=env, version=version), FUN=length))
tt <- merge(tt, with(participants, aggregate(success, list(env=env, version=version), FUN=sum)), by=c("env", "version"))
tt <- merge(tt, with(participants, aggregate(time_to_success, list(env=env, version=version), FUN=function(z) {median(z, na.rm=T)})), by=c("env", "version"))
names(tt) <- c("env", "version", "n", "n_success", "median_time_to_success")

cat("\n")
cat("\\begin{tabular}{l r r r r}\n")
cat("& \\multicolumn{2}{c}{success rate} & \\multicolumn{1}{c}{median time} \\\\\n")
cat("& \\multicolumn{2}{c}{after 40 minutes} & \\multicolumn{1}{c}{to success} \\\\\n")
cat("\\noalign{\\hrule}\n")
for (i in 1:nrow(tt)) {
	cat(sprintf("%s-%s & %d/%d & %.f\\%% & %s \\\\\n",
		tt$env[[i]], tt$version[[i]],
		tt$n_success[[i]], tt$n[[i]], 100 * tt$n_success[[i]] / tt$n[[i]],
		format_minutes_colons(tt$median_time_to_success[[i]])))
}
cat("\\end{tabular}\n")


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

ecdfs <- list(
	"E1-NEW"=ecdf.e1.new,
	"E1-OLD"=ecdf.e1.old,
	"E2-NEW"=ecdf.e2.new,
	"E2-OLD"=ecdf.e2.old,
	"E3-NEW"=ecdf.e3.new,
	"E3-OLD"=ecdf.e3.old
)

cat("\n")
cat("\\begin{tabular}{l r r r r}\n")
cat("& \\multicolumn{1}{c}{1.5~m} & \\multicolumn{1}{c}{10~m} & \\multicolumn{1}{c}{20~m} & \\multicolumn{1}{c}{40~m} \\\\\n")
cat("\\noalign{\\hrule}\n")
for (e in names(ecdfs)) {
	cat(e)
	# We fudge the 40 minute column because the last E3-OLD finished just a few seconds after.
	for (t in c(90, 10*60, 20*60, max(participants$time_to_success, na.rm=T))) {
		cat(sprintf(" & %.f\\%%", 100*ecdfs[[e]](t)))
	}
	cat(sprintf(" \\\\\n"))
}
cat("\\end{tabular}\n")
cat("\n\n\n\n\n\n")
