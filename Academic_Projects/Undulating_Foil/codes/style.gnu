
set terminal pngcairo transparent enhanced font "Calibre ,10" fontscale 1.0 size 1366, 768
set grid
# ------------------- Panel ------------------- 
set xlabel 'X-axis'
set ylabel 'Y-axis'
set xrange [0:3]
set yrange [-1:1]
name = "pan%a"
set output "../outputs/".name.".png"
set title name
plot "../outputs/".name.".dat" using 1:2 with lines

set xrange [0:15]
set yrange [-10:10]
name = "tippanel"
#set output "../outputs/".name.".png"
set title name
#plot "../outputs/".name.".dat" using 1:2 with lines


# ------------------- Panel Parameter vs Time Plots ------------------------------
set xlabel 'Time'
name = system("ls -1 ../outputs/*_x.dat ../outputs/*_y.dat ../outputs/vn.dat")

do for [i=1:words(name)] {
	link = "< paste ../outputs/Time.dat ". word(name, i)
	#set output word(name, i).'.png'
	set title word(name, i)
#	plot for [i=20:24] link using 1:i title 'i = '.(i-1) with lines
}

# ------------------- Panel Parameter vs Panel Plots ------------------------------

set xlabel 'Panel'
name = system("ls -1 ../outputs/transp/*_inv.dat")

do for [i=1:words(name)] {
	link = "< paste ../outputs/Panels.dat ".word(name, i)
	#set output word(name, i).'.png'
	set title word(name, i)
#	plot for [i=2:4] link using 1:i title 't = '.(i-2).' s' with lines
}



# ------------------- Vorticies Plots ------------------------------

set xlabel 'X-axis'
set ylabel 'Y-axis'

set xrange [0:15]
set yrange [-10:10]

name = "wakepanel"
#set output "../outputs/".name.".png"
set title name
#plot "../outputs/".name.".dat" using 1:2 with lines

#name = system("ls -1 ../outputs/vort-*.dat ../outputs/vort_u-*.dat ")
#name = system("ls -1 ../outputs/vort-*.dat")

do for [i=1:words(name)] {
#	set output word(name, i).'.png'
	set title word(name, i)
#	plot word(name, i) using 1:2 with lines
}

# ------------------- Thrust Plots ------------------------------

# name = system("ls -1 ../outputs/thrust*.dat")

#do for [i=1:words(name)] {
#	link = "< paste ../outputs/Time.dat ".word(name, i)
#	set output word(name, i).'.png'
#	set title word(name, i)
#	plot for [i=2:4:1] link using 1:i title " Axis".(i-1) with lines
#}

# ------------------- Work Plots ------------------------------

# name = system("ls -1 ../outputs/work*.dat")

#do for [i=1:words(name)] {
#link = "< paste ../outputs/Time.dat ".word(name, i)
#	set output word(name, i).'.png'
#	set title word(name, i)
#	plot link using 1:2 with lines
#}
