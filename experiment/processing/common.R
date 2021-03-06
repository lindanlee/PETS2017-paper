library(ggplot2)

# Got these values by:
#   \the\textwidth   % 483.6967pt
#   \the\textheight  % 661.97234pt
#   \the\columnwidth % 236.1578pt
textwidth <- 483.6967 / 72.27
textheight <- 661.97234 / 72.27
columnwidth <- 236.1578 / 72.27

# This is the length of the experiment, after which to cut off measurements.
# It's actually just a few seconds after 40 minutes, because one E3 just
# squeaked by.
maxtime <- 40*60 + 8

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

# Read edges.csv into one big data frame and compute durations.
read_edges <- function() {
	edges <- read.csv("edges.csv")
	edges <- edges[order(edges$sequence, edges$time_from_start), ]
	edges$duration <- ave(edges$time_from_start, edges$seat, edges$runid, FUN=function(z) {
		c(z[2:length(z)], z[length(z)]) - z
	})
	edges
}

# Canonicalize screen names from the OLD and NEW interfaces.
canonicalize_screens <- function(version, x) {
	y <- factor(x)
	# In the NEW interface, the screen that's logged as "bridges"
	# corresponds more closely to the "bridgeSettings" screen in the OLD
	# interface than it does the OLD "bridges" screen. Similarly NEW
	# "proxy" corresponds to OLD "proxyYES".
	y[version=="NEW" & y=="bridges"] <- "bridgeSettings"
	y[version=="NEW" & y=="proxy"] <- "proxyYES"
	levels(y) <- list(
		"not_running"="not_running",
		"starting"="starting",
		"first"="first",
		"bridges"="bridges",
		"bridgeSettings"="bridgeSettings",
		"bridgeHelp"="bridgeHelp",
		"proxy"="proxy",
		"proxyYES"="proxyYES",
		"summary"="summary",
		"progress"=c("progress_bar", "inlineprogress"),
		"error"=c("errorPanel", "error_dialog")
	)
	droplevels(y)
}

filter_edges <- function(edges, participants) {
	edges <- merge(edges, participants, by=c("seat", "runid"), all.y=T)
	# Keep only the edges up to the first success.
	edges <- edges[is.na(edges$time_to_success) | edges$time_from_start < edges$time_to_success, ]
	# Cut off the logs at our time limit.
	edges <- trim_edges(edges, maxtime)
	# Pad out the last edge if needed.
	edges <- pad_edges(edges, maxtime)
	edges$src <- canonicalize_screens(edges$version, edges$src)
	edges$dst <- canonicalize_screens(edges$version, edges$dst)
	edges
}

# Assign DNFs a maximum time_to_success.
clamp_time_to_success <- function(participants, maxtime) {
  df <- data.frame(participants)
  df$time_to_success[is.na(df$time_to_success)] <- maxtime
  df
}

# Remove edges that start after maxtime, and trim those that overlap it
# so their start time plus their duration exactly reaches maxtime.
trim_edges <- function(edges, maxtime) {
	edges <- edges[edges$time_from_start < maxtime, ]
	edges$duration <- ifelse(edges$time_from_start + edges$duration > maxtime, maxtime - edges$time_from_start, edges$duration)
	edges
}

# For those participants with is.na(time_to_success), add an additional
# edge that goes from the latest edge up to maxtime, whose src and dst
# are the same as the dst of the latest edge.
pad_edges <- function(edges, maxtime) {
	# Get a data frame consisting of only the latest edge for each (seat, runid).
	# suppressWarnings because ave(..., FUN=max) warns "no non-missing
	# arguments to max; returning -Inf"; presumably it internally creates
	# some zero-length vectors and passes them to FUN, even if not using
	# their output.
	new.edges <- suppressWarnings(edges[edges$sequence == ave(edges$sequence, edges$seat, edges$runid, FUN=max), ])
	# Throw out those that don't need padding.
	new.edges <- new.edges[is.na(new.edges$time_to_success) & new.edges$time_from_start + new.edges$duration < maxtime, ]
	# Modify the new edge so that it follows the latest edge.
	new.edges$sequence <- new.edges$sequence + 1
	new.edges$src <- new.edges$dst
	new.edges$time_from_start <- new.edges$time_from_start + new.edges$duration
	new.edges$duration <- maxtime - new.edges$time_from_start
	new.edges$event_type <- NA
	new.edges$event_target <- NA
	new.edges$event_value <- NA

	# Tack the new edges on to the end of the list.
	rbind(edges, new.edges)
}

is_active_edge <- function(edges) {
	!(edges$dst %in% c("not_running", "starting", "progress"))
}

common_theme <- function(p) {
	p <- p + theme_minimal()
	p <- p + theme(text=element_text(size=8))
	p <- p + theme(plot.margin=margin(0,0,0,0,"mm"))
	p <- p + theme(axis.ticks=element_blank(), axis.ticks.length=unit(0,"lines"))
	p <- p + theme(strip.background=element_rect(fill="gray90", color=NA))
	p
}
