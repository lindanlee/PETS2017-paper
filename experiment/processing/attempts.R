source("common.R")

options(width=400)

participants <- filter_participants(read_participants())
attempts <- read.csv("attempts.csv")
attempts$bridge_custom_entry = substr(attempts$bridge_custom_entry, 1, 30)

attempts <- merge(attempts, participants, by=c("seat", "runid"), all.y=T)

attempts <- attempts[order(attempts$seat, attempts$runid, attempts$time_from_start), ]
attempts <- attempts[is.na(attempts$time_to_success) | attempts$time_from_start <= attempts$time_to_success, ]

attempts$bridge_default_selection[!attempts$bridge_yesno] <- NA

# Show all attempts (up to first success).
attempts[, c("env", "version", "runid", "attempt_successful", "time_from_start", "bridge_yesno", "bridge_radio_state", "bridge_default_selection", "bridge_custom_entry", "proxy_yesno", "proxy_type", "proxy_addr", "proxy_port")]

# Show first successful attempts.
# attempts[attempts$attempt_successful, c("env", "version", "runid", "attempt_successful", "time_from_start", "bridge_yesno", "bridge_radio_state", "bridge_default_selection", "bridge_custom_entry", "proxy_yesno", "proxy_type", "proxy_addr", "proxy_port")]
