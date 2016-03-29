# (echo "env,ver,good"; egrep -c '"value":.?100.?' *.log | sed -e 's/\([^-]*\)-\([^-]*\)-.*:\(.*\)/\1 \2 \3/' | awk '{print $1 "," $2 "," ($3 != 0)}') > data.csv

aggregate(x$good, list(env=x$env), mean)
aggregate(x$good, list(ver=x$ver), mean)
aggregate(x$good, list(env=x$env, ver=x$ver), mean)

aggregate(x$good, list(env=x$env), length)
aggregate(x$good, list(ver=x$ver), length)
aggregate(x$good, list(env=x$env, ver=x$ver), length)
