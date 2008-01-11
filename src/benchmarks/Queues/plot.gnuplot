reset
set terminal x11
set output
#set terminal png
#set output "queues.png"
set datafile separator " "
set title  "NBAda queue benchmark"
set ylabel "Time (seconds)"
set xlabel "#threads"

#set yrange [0:100]

plot \
  "queue_test.ebmr_result_dork_Linux_2007-09-12_17:36.log.res"\
    using "%lf %*lf %*lf %*lf %lf"\
    title "M&S Queue/EBMR" with linespoints\
 ,"queue_test.hpmr_result_dork_Linux_2007-09-12_17:36.log.res"\
    using "%lf %*lf %*lf %*lf %lf"\
    title "M&S Queue/HPMR" with linespoints\
 ,"queue_test.lfmr_result_dork_Linux_2007-09-12_17:36.log.res"\
    using "%lf %*lf %*lf %*lf %lf"\
    title "M&S Queue/LFMR" with linespoints\
 ,"queue_test.lfrc_result_dork_Linux_2007-09-12_17:36.log.res"\
    using "%lf %*lf %*lf %*lf %lf"\
    title "M&S Queue/LFRC" with linespoints\

exit