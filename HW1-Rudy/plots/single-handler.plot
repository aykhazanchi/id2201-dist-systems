set term postscript eps enhanced color
set output "single-handler.eps"
set title "Rudy Request Response Time from Single Handler Connections"
set xlabel "Request Attempt"
set ylabel "Response Time"
set xrange [0:10]
set yrange [0:50000]

plot "single-handler.dat" using 1:2 with linespoints title "Response Time"

