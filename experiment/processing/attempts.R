source("common.R")

participants <- filter_participants(read_participants())
attempts <- read.csv("attempts.csv")

attempts <- merge(attempts, participants, by=c("seat", "runid"), all.y=T)

attempts <- attempts[order(attempts$seat, attempts$runid, attempts$time_from_start), ]
# Remove attempts after the first successful one.
attempts <- attempts[is.na(attempts$time_to_success) | attempts$time_from_start <= attempts$time_to_success, ]

attempts$bridge_default_selection[!attempts$bridge_yesno] <- NA

# Show all attempts (up to first success).
# attempts[, c("env", "version", "runid", "attempt_successful", "time_from_start", "bridge_yesno", "bridge_radio_state", "bridge_default_selection", "bridge_custom_entry", "proxy_yesno", "proxy_type", "proxy_addr", "proxy_port")]

# Show first successful attempts.
# attempts[attempts$attempt_successful, c("env", "version", "runid", "attempt_successful", "time_from_start", "bridge_yesno", "bridge_radio_state", "bridge_default_selection", "bridge_custom_entry", "proxy_yesno", "proxy_type", "proxy_addr", "proxy_port")]

successful.attempts <- attempts[attempts$attempt_successful, ]
successful.attempts$label <- sprintf("%s, %s",
	# bridge label
	ifelse(successful.attempts$bridge_yesno,
		ifelse(successful.attempts$bridge_radio_state == "default",
			as.character(successful.attempts$bridge_default_selection),
			"custom bridge"),
		"no bridge"),
	# proxy label
	ifelse(successful.attempts$proxy_yesno,
		"3rd-party proxy",
		"no proxy")
)

# Convert a number to a decimal string, but map 0 to "".
omitzero <- function(n) {
	ifelse(n == 0, "", sprintf("%d", n))
}

tt <- table(successful.attempts[, c("env", "version", "label")])
df <- as.data.frame(tt)

dnf.tt <- table(participants[!participants$success, c("env", "version")])

cat("\n")
cat("\\begin{tabular}{r c c c c c c}\n")
for (env in c("E1", "E2", "E3")) {
	for (version in c("NEW", "OLD")) {
		cat(sprintf("& \\rotatebox{90}{%s-%s} ", env, version))
	}
}
cat(sprintf("\\\\\n"))
for (label in unique(df$label)) {
	cat(sprintf("%s ", label))
	for (env in c("E1", "E2", "E3")) {
		for (version in c("NEW", "OLD")) {
			cat(sprintf("& %s ", omitzero(tt[env, version, label])))
		}
	}
	cat(sprintf("\\\\\n"))
}
cat(sprintf("DNF ", label))
for (env in c("E1", "E2", "E3")) {
	for (version in c("NEW", "OLD")) {
		cat(sprintf("& %s ", omitzero(dnf.tt[env, version])))
	}
}
cat(sprintf("\\\\\n"))
cat("\\end{tabular}\n")
