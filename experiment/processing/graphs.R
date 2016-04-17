library(ggplot2)
library(scales)
library(RColorBrewer)

source("common.R")

##################
# INITIALIZATION #
##################

height <- 1.5

participants <- filter_participants(read_participants())
participants$label <- factor(sprintf("%s-%s", participants$env, participants$version), levels=c("E1-NEW", "E1-OLD", "E2-NEW", "E2-OLD", "E3-NEW", "E3-OLD"))
participants$pid <- factor(sprintf("%s-%s-%s-%s", participants$env, participants$version, participants$seat, participants$session))
participants$pid <- factor(participants$pid, levels=participants$pid[rev(order(participants$env, participants$version, participants$time_to_success))])

# This is the length of the experiment, after which to cut off measurements.
# It's actually just a few seconds after 40 minutes, because one E3 just
# squeaked by.
maxtime <- max(40*60, participants$time_to_success, na.rm=T)

edges <- read_edges()
edges <- merge(edges, participants, by=c("seat", "runid"), all.y=T)
if (any(is.na(edges$pid))) {
  stop("found NAs in edges$pid")
}

# Remove edges that start after maxtime, and trim those that overlap it
# so their start time plus their duration exactly reaches maxtime.
trim_edges <- function(edges, maxtime) {
  edges <- edges[edges$time_from_start <= maxtime, ]
  edges$duration <- ifelse(edges$time_from_start + edges$duration > maxtime, maxtime - edges$time_from_start, edges$duration)
  edges
}

edges <- edges[order(edges$sequence, edges$time_from_start), ]
edges$duration <- ave(edges$time_from_start, edges$seat, edges$runid, FUN=function(z) {
  c(z[2:length(z)], z[length(z)]) - z
})
# Keep only the edges up to the first success.
edges <- edges[is.na(edges$time_to_success) | edges$time_from_start <= edges$time_to_success, ]
# Cut off the logs at our time limit.
edges <- trim_edges(edges, maxtime)
# Ignore "not_running" and "starting", so they just show up as blank.
edges <- edges[!(edges$dst %in% c("not_running", "starting")), ]

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
    "error"="errorPanel"
  )
  droplevels(y)
}

edges$src <- canonicalize_screens(edges$version, edges$src)
edges$dst <- canonicalize_screens(edges$version, edges$dst)

state.palette <- brewer.pal(length(levels(edges$dst)), "Set1")

###################
# TIME TO SUCCESS #
###################

# Assign DNFs a maximum time_to_success.
clamp_time_to_success <- function(participants, maxtime) {
  df <- data.frame(participants)
  df$time_to_success[is.na(df$time_to_success)] <- maxtime
  df
}

time_to_success_plot <- function(participants) {
  minutes_to_success <- participants$time_to_success/60
  max_minutes <- max(minutes_to_success, na.rm=T)+9
  p <- ggplot(data.frame(participants, minutes_to_success), aes(x=label, y=minutes_to_success), environment=environment())
  p <- p + geom_boxplot(color="gray70", outlier.size=0)
  p <- p + geom_point(alpha=0.6, aes(size=success, color=success, shape=success))
  p <- p + scale_size_manual(values=c("TRUE"=0.8, "FALSE"=1.0), guide=F)
  p <- p + scale_color_manual(values=c("TRUE"="black", "FALSE"="red3"), guide=F)
  p <- p + scale_shape_manual(values=c("TRUE"=19, "FALSE"=4), guide=F)
  p <- p + coord_cartesian(ylim=c(0, max_minutes))
  p <- p + coord_flip()
  p <- p + scale_x_discrete(limits=rev(levels(participants$label)))
  p <- p + labs(title=NULL, x=NULL, y="Minutes to first success")
  counts <- aggregate(data.frame(participants, n=1)[, c("success", "n")], by=list(label=participants$label), sum)
  p <- p + geom_text(data=counts, aes(x=label, y=max_minutes, label=sprintf("%d/%d DNF", n-success, n)), hjust=1, size=2, alpha=0.6)
  p <- common_theme(p)
  p <- p + theme(panel.grid.major.y=element_blank())
  p
}

p <- time_to_success_plot(participants)
ggsave("time_to_success.pdf", p, width=columnwidth, height=height, device=cairo_pdf)

p <- time_to_success_plot(clamp_time_to_success(participants, maxtime))
ggsave("time_to_success_clamped.pdf", p, width=columnwidth, height=height, device=cairo_pdf)


# Assign each environment a color (E1=blue, E2=orange, E3=green), with NEW
# being more saturated and OLD being less saturated.
palette <- c(
	brewer.pal(3, "Blues")[c(3,2)],
	brewer.pal(3, "Oranges")[c(3,2)],
	brewer.pal(3, "BuGn")[c(3,2)]
)
names(palette) <- levels(participants$label)

# Artificially add a success time that is way off the scale for all
# participants that were not successful. Otherwise the ecdf will ignore the NAs
# and cause all the curves to rise to 100%. The artificial points will be
# hidden off the edge of the graph by coord_cartesian.
minutes_to_success <- pmin(participants$time_to_success, maxtime*2, na.rm=T) / 60
p <- ggplot(participants, aes(x=minutes_to_success, color=label))
p <- p + geom_step(stat="ecdf", size=0.5)
p <- p + coord_cartesian(xlim=c(0, maxtime)/60)
p <- p + scale_y_continuous(labels=percent)
p <- p + scale_color_manual(values=palette)
# p <- p + guides(color=guide_legend(override.aes=list(size=2, title=NULL)))
p <- p + guides(color=F)
p <- p + labs(title=NULL, x="Minutes elapsed", y="Successful participants")
p <- p + annotate(geom="text", x=c(1, 3.3, 14, 15.3, 23, 24.7), y=c(0.86, 0.87, 0.91, 0.71, 0.61, 0.32), label=c("E1-\nNEW", "E1-OLD", "E2-NEW", "E2-OLD", "E3-NEW", "E3-OLD"), hjust=c(1, 0, 0, 0, 0, 0), vjust=0, size=2, lineheight=0.8)
p <- common_theme(p)
ggsave("time_to_success_ecdf.pdf", p, width=columnwidth, height=height, device=cairo_pdf)

############################
# SCREEN SPECIFIC ANALYSES #
############################

# SETUP 
# 1: group edges by their source
# 2: get the total time each user spent on a particular screen 
# 3: create data frame "screen time per user" which has the time a user spent on each screen 
# 4: (TODO) historgram plot function

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

# 4
# time_histogram <- function(time) {
#   p <- hist(time)
#   p <- p + xlim=c(0,40)
#   p <- p + ylim=c(0,120)
#   p <- p + xlab="Minutes" 
#   p <- p + ylab="Number of Participants"
#   p <- common_theme(p)
#   p
# }

# PROGRESS SCREEN 
  # 1: minutes  TOTAL time on PROGRESS screen, across users. (total, e1, e2, e3,new, old)
  # 2: % TOTAL time on PROGRESS screen, across users. (total, e1, e2, e3,new, old)
  # 3: minutes TOTAL time on PROGRESS screen, by user. (total, e1, e2, e3,new, old)
  # 4: % TOTAL time on PROGRESS screen, by user. (total, e1, e2, e3,new, old)

# 1 
progress_time_all = sum(progress_edges$duration)/60
progress_time_e1 = sum(progress_edges[progress_edges[,"env"]=="E1",]$duration)/60
progress_time_e2 =sum(progress_edges[progress_edges[,"env"]=="E2",]$duration)/60
progress_time_e3 =sum(progress_edges[progress_edges[,"env"]=="E3",]$duration)/60
progress_time_new =sum(progress_edges[progress_edges[,"version"]=="NEW",]$duration)/60
progress_time_old =sum(progress_edges[progress_edges[,"version"]=="OLD",]$duration)/60

# 2
p_progress_time_all = sum(progress_edges$duration)/sum(edges$duration)*100
p_progress_time_e1 = sum(progress_edges[progress_edges[,"env"]=="E1",]$duration)/sum(edges[edges[,"env"]=="E1",]$duration)*100
p_progress_time_e2 =sum(progress_edges[progress_edges[,"env"]=="E2",]$duration)/sum(edges[edges[,"env"]=="E2",]$duration)*100
p_progress_time_e3 =sum(progress_edges[progress_edges[,"env"]=="E3",]$duration)/sum(edges[edges[,"env"]=="E3",]$duration)*100
p_progress_time_new =sum(progress_edges[progress_edges[,"version"]=="NEW",]$duration)/sum(edges[edges[,"version"]=="NEW",]$duration)*100
p_progress_time_old =sum(progress_edges[progress_edges[,"version"]=="OLD",]$duration)/sum(edges[edges[,"version"]=="OLD",]$duration)*100


# 3
user_progress_time_all <- screen_time_per_user$`progress_edges$duration`/60
user_progress_time_e1 <- screen_time_per_user[screen_time_per_user[,"env"]=="E1",]$`progress_edges$duration`/60
user_progress_time_e2 <- screen_time_per_user[screen_time_per_user[,"env"]=="E2",]$`progress_edges$duration`/60
user_progress_time_e3 <- screen_time_per_user[screen_time_per_user[,"env"]=="E3",]$`progress_edges$duration`/60
user_progress_time_new <- screen_time_per_user[screen_time_per_user[,"version"]=="NEW",]$`progress_edges$duration`/60
user_progress_time_old <- screen_time_per_user[screen_time_per_user[,"version"]=="OLD",]$`progress_edges$duration`/60

hist(user_progress_time_all, xlim=c(0,40), ylim=c(0,80))
hist(user_progress_time_e1, xlim=c(0,40), ylim=c(0,40))
hist(user_progress_time_e2, xlim=c(0,40), ylim=c(0,40))
hist(user_progress_time_e3, xlim=c(0,40), ylim=c(0,40))
hist(user_progress_time_new, xlim=c(0,40), ylim=c(0,40))
hist(user_progress_time_old, xlim=c(0,40), ylim=c(0,40))

# 4
p_user_progress_time_all <- screen_time_per_user$`progress_edges$duration`/screen_time_per_user$`edges$duration`*100
p_user_progress_time_e1 <- screen_time_per_user[screen_time_per_user[,"env"]=="E1",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"env"]=="E1",]$`edges$duration`*100
p_user_progress_time_e2 <- screen_time_per_user[screen_time_per_user[,"env"]=="E2",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"env"]=="E2",]$`edges$duration`*100
p_user_progress_time_e3 <- screen_time_per_user[screen_time_per_user[,"env"]=="E3",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"env"]=="E3",]$`edges$duration`*100
p_user_progress_time_new <- screen_time_per_user[screen_time_per_user[,"version"]=="NEW",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"version"]=="NEW",]$`edges$duration`*100
p_user_progress_time_old <- screen_time_per_user[screen_time_per_user[,"version"]=="OLD",]$`progress_edges$duration`/screen_time_per_user[screen_time_per_user[,"version"]=="OLD",]$`edges$duration`*100

hist(p_user_progress_time_all, xlim=c(0,100), ylim=c(0,20))
hist(p_user_progress_time_e1, xlim=c(0,100), ylim=c(0,15))
hist(p_user_progress_time_e2, xlim=c(0,100), ylim=c(0,15))
hist(p_user_progress_time_e3, xlim=c(0,100), ylim=c(0,15))
hist(p_user_progress_time_new, xlim=c(0,100), ylim=c(0,15))
hist(p_user_progress_time_old, xlim=c(0,100), ylim=c(0,15))

# FIRST SCREEN
# 1: % TOTAL time on FIRST screen, across users. (total, e1, e2, e3,new, old)
# 2: % ACTIVE time on FIRST screen, across users. (total, e1, e2, e3,new, old)

# 1
sum(first_edges$duration)/sum(edges$duration)*100
sum(first_edges[first_edges[,"env"]=="E1",]$duration)/sum(edges[edges[,"env"]=="E1",]$duration)*100
sum(first_edges[first_edges[,"env"]=="E2",]$duration)/sum(edges[edges[,"env"]=="E2",]$duration)*100
sum(first_edges[first_edges[,"env"]=="E3",]$duration)/sum(edges[edges[,"env"]=="E3",]$duration)*100
sum(first_edges[first_edges[,"version"]=="NEW",]$duration)/sum(edges[edges[,"version"]=="NEW",]$duration)*100
sum(first_edges[first_edges[,"version"]=="OLD",]$duration)/sum(edges[edges[,"version"]=="OLD",]$duration)*100

# 2
sum(first_edges$duration)/sum(active_edges$duration)*100
sum(first_edges[first_edges[,"env"]=="E1",]$duration)/sum(active_edges[active_edges[,"env"]=="E1",]$duration)*100
sum(first_edges[first_edges[,"env"]=="E2",]$duration)/sum(active_edges[active_edges[,"env"]=="E2",]$duration)*100
sum(first_edges[first_edges[,"env"]=="E3",]$duration)/sum(active_edges[active_edges[,"env"]=="E3",]$duration)*100
sum(first_edges[first_edges[,"version"]=="NEW",]$duration)/sum(active_edges[active_edges[,"version"]=="NEW",]$duration)*100
sum(first_edges[first_edges[,"version"]=="OLD",]$duration)/sum(active_edges[active_edges[,"version"]=="OLD",]$duration)*100

# PROXY SCREEN
# 1: % TOTAL time on PROXY screen, across users. (total, e1, e2, e3,new, old)
# 2: % ACTIVE time on PROXY screen, across users. (total, e1, e2, e3,new, old)

# 1
sum(proxy_edges$duration)/sum(edges$duration)*100
sum(proxy_edges[proxy_edges[,"env"]=="E1",]$duration)/sum(edges[edges[,"env"]=="E1",]$duration)*100
sum(proxy_edges[proxy_edges[,"env"]=="E2",]$duration)/sum(edges[edges[,"env"]=="E2",]$duration)*100
sum(proxy_edges[proxy_edges[,"env"]=="E3",]$duration)/sum(edges[edges[,"env"]=="E3",]$duration)*100
sum(proxy_edges[proxy_edges[,"version"]=="NEW",]$duration)/sum(edges[edges[,"version"]=="NEW",]$duration)*100
sum(proxy_edges[proxy_edges[,"version"]=="OLD",]$duration)/sum(edges[edges[,"version"]=="OLD",]$duration)*100

# 2
sum(proxy_edges$duration)/sum(active_edges$duration)*100
sum(proxy_edges[proxy_edges[,"env"]=="E1",]$duration)/sum(active_edges[active_edges[,"env"]=="E1",]$duration)*100
sum(proxy_edges[proxy_edges[,"env"]=="E2",]$duration)/sum(active_edges[active_edges[,"env"]=="E2",]$duration)*100
sum(proxy_edges[proxy_edges[,"env"]=="E3",]$duration)/sum(active_edges[active_edges[,"env"]=="E3",]$duration)*100
sum(proxy_edges[proxy_edges[,"version"]=="NEW",]$duration)/sum(active_edges[active_edges[,"version"]=="NEW",]$duration)*100
sum(proxy_edges[proxy_edges[,"version"]=="OLD",]$duration)/sum(active_edges[active_edges[,"version"]=="OLD",]$duration)*100


# BRIDGE SCREEN
# 1: % TOTAL time on BRIDGE screen, across users. (total, e1, e2, e3,new, old)
# 2: % ACTIVE time on BRIDGE screen, across users. (total, e1, e2, e3,new, old)

# 1
sum(bridge_edges$duration)/sum(edges$duration)*100
sum(bridge_edges[bridge_edges[,"env"]=="E1",]$duration)/sum(edges[edges[,"env"]=="E1",]$duration)*100
sum(bridge_edges[bridge_edges[,"env"]=="E2",]$duration)/sum(edges[edges[,"env"]=="E2",]$duration)*100
sum(bridge_edges[bridge_edges[,"env"]=="E3",]$duration)/sum(edges[edges[,"env"]=="E3",]$duration)*100
sum(bridge_edges[bridge_edges[,"version"]=="NEW",]$duration)/sum(edges[edges[,"version"]=="NEW",]$duration)*100
sum(bridge_edges[bridge_edges[,"version"]=="OLD",]$duration)/sum(edges[edges[,"version"]=="OLD",]$duration)*100

# 2
sum(bridge_edges$duration)/sum(active_edges$duration)*100
sum(bridge_edges[bridge_edges[,"env"]=="E1",]$duration)/sum(active_edges[active_edges[,"env"]=="E1",]$duration)*100
sum(bridge_edges[bridge_edges[,"env"]=="E2",]$duration)/sum(active_edges[active_edges[,"env"]=="E2",]$duration)*100
sum(bridge_edges[bridge_edges[,"env"]=="E3",]$duration)/sum(active_edges[active_edges[,"env"]=="E3",]$duration)*100
sum(bridge_edges[bridge_edges[,"version"]=="NEW",]$duration)/sum(active_edges[active_edges[,"version"]=="NEW",]$duration)*100
sum(bridge_edges[bridge_edges[,"version"]=="OLD",]$duration)/sum(active_edges[active_edges[,"version"]=="OLD",]$duration)*100


# SUMMARY SCREEN 
# 1: % TOTAL time on SUMMARY screen, across users. (total, e1, e2, e3,new, old)
# 2: % ACTIVE time on SUMMARY screen, across users. (total, e1, e2, e3,new, old)
# 3: distribution of destinations from summary screen 

# 1
sum(summary_edges$duration)/sum(edges$duration)*100
sum(summary_edges[summary_edges[,"env"]=="E1",]$duration)/sum(edges[edges[,"env"]=="E1",]$duration)*100
sum(summary_edges[summary_edges[,"env"]=="E2",]$duration)/sum(edges[edges[,"env"]=="E2",]$duration)*100
sum(summary_edges[summary_edges[,"env"]=="E3",]$duration)/sum(edges[edges[,"env"]=="E3",]$duration)*100
sum(summary_edges[summary_edges[,"version"]=="NEW",]$duration)/sum(edges[edges[,"version"]=="NEW",]$duration)*100
sum(summary_edges[summary_edges[,"version"]=="OLD",]$duration)/sum(edges[edges[,"version"]=="OLD",]$duration)*100

# 2 
sum(summary_edges$duration)/sum(active_edges$duration)*100
sum(summary_edges[summary_edges[,"env"]=="E1",]$duration)/sum(active_edges[active_edges[,"env"]=="E1",]$duration)*100
sum(summary_edges[summary_edges[,"env"]=="E2",]$duration)/sum(active_edges[active_edges[,"env"]=="E2",]$duration)*100
sum(summary_edges[summary_edges[,"env"]=="E3",]$duration)/sum(active_edges[active_edges[,"env"]=="E3",]$duration)*100
sum(summary_edges[summary_edges[,"version"]=="NEW",]$duration)/sum(active_edges[active_edges[,"version"]=="NEW",]$duration)*100
sum(summary_edges[summary_edges[,"version"]=="OLD",]$duration)/sum(active_edges[active_edges[,"version"]=="OLD",]$duration)*100

# 3
summary_edges <- summary_edges[summary_edges$dst != "summary", ]
sum(summary_edges$dst == "bridgeSettings")/nrow(summary_edges)*100
sum(summary_edges$dst == "progress")/nrow(summary_edges)*100
barplot(height=table(summary_edges$dst))

# GRAPH TIME ON EACH SCREEN 

state.palette <- brewer.pal(length(levels(edges$dst)), "Set1")
data <- aggregate(edges$duration, list(env=edges$env, version=edges$version, seat=edges$seat, runid=edges$runid, dst=edges$dst), FUN=sum)
p <- ggplot(data, aes(x=dst, y=x/60, color=dst))
p <- p + geom_boxplot(outlier.size=0)
p <- p + geom_point(size=1.0, alpha=0.6)
p <- p + scale_color_manual(values=state.palette, guide=F)
p <- p + scale_x_discrete("Current screen", labels=c(
  "first"="F",
  "bridges"="B1",
  "bridgeSettings"="B2\nB",
  "bridgeHelp"="BH",
  "proxy"="P1",
  "proxyYES"="P2\nP",
  "summary"="S",
  "progress"="Pr",
  "error"="E"
))
p <- p + coord_flip()
p <- common_theme(p)
p <- p + facet_grid(version ~ env)
p
ggsave("time_per_screen.pdf", p, width=textwidth, height=3, device=cairo_pdf)

#############
# ALL EDGES #
#############

p <- ggplot()
p <- p + geom_segment(data=participants, size=0.2, color="black", aes(x=pid, xend=pid, y=0, yend=ifelse(!is.na(time_to_success), time_to_success, maxtime)/60))
p <- p + geom_segment(data=edges, size=1.5, lineend="butt", aes(x=pid, xend=pid, y=time_from_start/60, yend=(time_from_start+duration)/60, color=dst))
# p <- p + geom_point(color="black", size=1.5, shape="|")
p <- p + geom_point(data=participants[is.na(participants$time_to_success), ], aes(x=pid, y=maxtime/60), shape=4)
p <- p + coord_flip()
p <- p + scale_y_continuous(breaks=pretty_breaks(n=10))
p <- p + scale_color_manual("Current screen", values=state.palette, labels=c(
	"first"="first (F)",
	"bridges"="bridge yes/no (B1)",
	"bridgeSettings"="bridge settings (B2/B)",
	"bridgeHelp"="bridge help (BH)",
	"proxy"="proxy yes/no (P1)",
	"proxyYES"="proxy settings (P2/P)",
	"summary"="summary (S)",
	"progress"="progress (Pr)",
	"error"="error"
))
p <- p + labs(title=NULL, x="Participants", y="Minutes elapsed")
p <- common_theme(p)
p <- p + theme(panel.grid.minor.x=element_blank())
p <- p + theme(panel.grid.major.y=element_blank())
ggsave("all-participant-edges.pdf", p, width=textwidth, height=textheight-0.75, device=cairo_pdf)
