set term postscript eps enhanced color
set output "multi-handler.eps"
set title "Rudy Request Response Time from Multi Handler Connections"
set xlabel "Request Attempt"
set ylabel "Response Time"
set xrange [0:10]
set yrange [0:1100000]

plot "multi-handler.dat" using 1:2 with linespoints title "Response Time"

