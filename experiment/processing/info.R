library(coin)
csource("common.R")

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
cat("****************************\n")
cat("* SCREEN SPECIFIC ANALYSES *\n")
cat("****************************\n")

# SETUP 
# 1: group edges by their source
# 2: get the total time each user spent on a particular screen 
# 3: create data frame "screen time per user" which has the time a user spent on each screen 
# 4: (TODO) historgram plot function

edges <- filter_edges(read_edges(), participants)

# 1 
active_edges <- edges[edges[,"src"] != "progress",] # "active time" is anytime that people are not on the progress screen.
progress_edges <- edges[edges[,"src"]== "progress",]
first_edges <- edges[edges[,"src"]== "first",]
proxy_edges <- edges[edges[,"src"] %in% c("proxy","proxyYES"),]
bridge_edges <-  edges[edges[,"src"] %in% c("bridges","bridgeSettings","bridgeHelp"),]
summary_edges <- edges[edges[,"src"]== "summary",]

# 2
total_time_per_user <- aggregate(edges$duration ~ edges$userid, edges, sum)
active_time_per_user <- aggregate(active_edges$duration ~ active_edges$userid, active_edges, sum)
progress_time_per_user <- aggregate(progress_edges$duration ~ progress_edges$userid, progress_edges, sum)
first_time_per_user <- aggregate(first_edges$duration ~ first_edges$userid, first_edges, sum)
proxy_time_per_user <- aggregate(proxy_edges$duration ~ proxy_edges$userid, proxy_edges, sum)
bridge_time_per_user <- aggregate(bridge_edges$duration ~ bridge_edges$userid, bridge_edges, sum)
summary_time_per_user <- aggregate(summary_edges$duration ~ summary_edges$userid, summary_edges, sum)

# 3 
screen_time_per_user <- participants[c("userid","env","version","pool")]
screen_time_per_user <- merge(screen_time_per_user, total_time_per_user, by=c(1), all=T)
screen_time_per_user <- merge(screen_time_per_user, active_time_per_user, by=c(1), all=T)
screen_time_per_user <- merge(screen_time_per_user, progress_time_per_user, by=c(1), all=T)
screen_time_per_user <- merge(screen_time_per_user, proxy_time_per_user, by=c(1), all=T)
screen_time_per_user <- merge(screen_time_per_user, bridge_time_per_user, by=c(1), all=T)
screen_time_per_user <- merge(screen_time_per_user, summary_time_per_user, by=c(1), all=T)
screen_time_per_user[is.na(screen_time_per_user)] <- 0

subreport <- function(label, sublabel, selected_edges, denom_edges, f) {
	cat(sprintf("%s\t%s\t%s\n", sublabel, label,
		format_percent(sum(selected_edges[f(selected_edges),]$duration)/sum(denom_edges[f(denom_edges),]$duration))))
}

report_total <- function(label, selected_edges) {
	subreport_total <- function(sublabel, f) {
		subreport(label, sublabel, selected_edges, edges, f)
	}

	# % TOTAL time on screen, across users. (total, e1, e2, e3,new, old)
	subreport_total("TOTAL\tAll", function(z) { T })
	subreport_total("TOTAL\tE1", function(z) { z$env=="E1" })
	subreport_total("TOTAL\tE2", function(z) { z$env=="E2" })
	subreport_total("TOTAL\tE3", function(z) { z$env=="E3" })
	subreport_total("TOTAL\tNEW", function(z) { z$version=="NEW" })
	subreport_total("TOTAL\tOLD", function(z) { z$version=="OLD" })
}

report_active <- function(label, selected_edges) {
	subreport_active <- function(sublabel, f) {
		subreport(label, sublabel, selected_edges, active_edges, f)
	}

	# % ACTIVE time on screen, across users. (total, e1, e2, e3,new, old)
	subreport_active("ACTIVE\tAll", function(z) { T })
	subreport_active("ACTIVE\tE1", function(z) { z$env=="E1" })
	subreport_active("ACTIVE\tE2", function(z) { z$env=="E2" })
	subreport_active("ACTIVE\tE3", function(z) { z$env=="E3" })
	subreport_active("ACTIVE\tNEW", function(z) { z$version=="NEW" })
	subreport_active("ACTIVE\tOLD", function(z) { z$version=="OLD" })
}

report <- function(label, selected_edges) {
	report_total(label, selected_edges)
	report_active(label, selected_edges)
}

# PROGRESS SCREEN 
# 1: minutes  TOTAL time on PROGRESS screen, across users. (total, e1, e2, e3,new, old)
# 2: % TOTAL time on PROGRESS screen, across users. (total, e1, e2, e3,new, old)
# 3: minutes TOTAL time on PROGRESS screen, by user. (total, e1, e2, e3,new, old)
# 4: % TOTAL time on PROGRESS screen, by user. (total, e1, e2, e3,new, old)

# 1 
sum(progress_edges$duration)/60
sum(progress_edges[progress_edges[,"env"]=="E1",]$duration)/60
sum(progress_edges[progress_edges[,"env"]=="E2",]$duration)/60
sum(progress_edges[progress_edges[,"env"]=="E3",]$duration)/60
sum(progress_edges[progress_edges[,"version"]=="NEW",]$duration)/60
sum(progress_edges[progress_edges[,"version"]=="OLD",]$duration)/60

# 2
report_total("PROGRESS", progress_edges)

# 3
user_progress_time_all <- screen_time_per_user$`progress_edges$duration`/60
user_progress_time_e1 <- screen_time_per_user[screen_time_per_user[,"env"]=="E1",]$`progress_edges$duration`/60
user_progress_time_e2 <- screen_time_per_user[screen_time_per_user[,"env"]=="E2",]$`progress_edges$duration`/60
user_progress_time_e3 <- screen_time_per_user[screen_time_per_user[,"env"]=="E3",]$`progress_edges$duration`/60
user_progress_time_new <- screen_time_per_user[screen_time_per_user[,"version"]=="NEW",]$`progress_edges$duration`/60
user_progress_time_old <- screen_time_per_user[screen_time_per_user[,"version"]=="OLD",]$`progress_edges$duration`/60

# 4
p_user_progress_time_all <- screen_time_per_user$`progress_edges$duration`/screen_time_per_user$`edges$duration`*100
p_user_progress_time_e1 <- screen_time_per_user[screen_time_per_user[,"env"]=="E1",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"env"]=="E1",]$`edges$duration`*100
p_user_progress_time_e2 <- screen_time_per_user[screen_time_per_user[,"env"]=="E2",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"env"]=="E2",]$`edges$duration`*100
p_user_progress_time_e3 <- screen_time_per_user[screen_time_per_user[,"env"]=="E3",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"env"]=="E3",]$`edges$duration`*100
p_user_progress_time_new <- screen_time_per_user[screen_time_per_user[,"version"]=="NEW",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"version"]=="NEW",]$`edges$duration`*100
p_user_progress_time_old <- screen_time_per_user[screen_time_per_user[,"version"]=="OLD",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"version"]=="OLD",]$`edges$duration`*100

# FIRST SCREEN
report("FIRST", first_edges)

# PROXY SCREEN
report("PROXY", proxy_edges)

# BRIDGE SCREEN
report("BRIDGE", bridge_edges)

# SUMMARY SCREEN 
report("SUMMARY", summary_edges)

# 3: distribution of destinations from summary screen 
# 4: if old interface users clicked back from proxyYES/proxy.

# 3
sum(summary_edges$dst == "bridgeSettings")/nrow(summary_edges)*100
sum(summary_edges$dst == "progress")/nrow(summary_edges)*100
barplot(height=table(summary_edges$dst))

#4 
proxy_edges_transitions <- proxy_edges[!proxy_edges[,"dst"] %in% c("proxy","proxyYES"),]
proxy_edges_transitions <- proxy_edges_transitions[proxy_edges_transitions[,"version"]=="OLD",]
sum(proxy_edges_transitions$dst == "progress")/nrow(proxy_edges_transitions)*100
sum(proxy_edges_transitions$dst != "progress")/nrow(proxy_edges_transitions)*100

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
cat("impact of version on success rates \n")
wilcox.test(x ~ version, data=success_rates)

cat("impact of version on times to completion \n")
wilcox.test(time_to_success ~ version, data=clamp_time_to_success(participants, maxtime))

cat("impact of version on active time \n")
wilcox.test(screen_time_per_user$`active_edges$duration` ~ version, data=screen_time_per_user)
wilcox_test(screen_time_per_user$`active_edges$duration` ~ version, data=screen_time_per_user)


cat("\n\n\n\n")
cat("impact of environment on success rates\n")
kruskal.test(x ~ env, data=success_rates)

cat("impact of environment on times to completion \n")
kruskal.test(time_to_success ~ env, data=clamp_time_to_success(participants, maxtime))

cat("impact of environment on active time \n")
kruskal.test(screen_time_per_user$`active_edges$duration` ~ env, data=screen_time_per_user)


cat("\n\n\n\n")
cat("logistic regression predicting success by ver, env, pool \n")
#just env, version, pool, and success columns
participants_glm_subset <- subset(clamp_time_to_success(participants, maxtime),select=c(5,6,9,10))
model <- glm(success ~.,family=binomial(link='logit'),data=participants_glm_subset)
summary(model)
#cat("\n\n\n")
#anova(model)

cat("\n\n\n\n")
cat("linear regression predicting time to success by ver, env, pool \n")
#just env, version, pool, and time_to_success columns
participants_lm_subset <- subset(clamp_time_to_success(participants, maxtime),select=c(5,6,9,11))
model <- lm(time_to_success ~.,data=participants_lm_subset)
summary(model)

cat("\n\n\n\n\n\n")
cat("***************\n")
cat("* OTHER STUFF *\n")
cat("***************\n")

cat("\n")
tt <- with(clamp_time_to_success(participants, maxtime), merge(merge(
	aggregate(success, list(env=env, version=version), FUN=length),
	aggregate(success, list(env=env, version=version), FUN=sum), by=c("env", "version")),
	aggregate(time_to_success, list(env=env, version=version), FUN=function(z) {median(z, na.rm=T)}), by=c("env", "version")))
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
