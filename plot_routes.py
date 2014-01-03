# Plots all routes written to output file by the main program.

import matplotlib.pyplot as plt

input_file = "main.out" # Output from Fortran main program

plt.autoscale(tight=True)

f = open(input_file)
lines = f.readlines()
for i in range(0, len(lines), 3): # Step of 3 to get both x and y along with generation
	# Read x and y in. Yes, I know exec can be dangerous, but you should not run programs with unfamiliar inputs anyway.
	exec(lines[i+1])
	exec(lines[i+2])
	# Plot them
	fig = plt.figure()
	fig.canvas.manager.set_window_title(str(lines[i])) # Name the plots
	plt.scatter(x, y, marker = 'o') # Plot the cities
	plt.plot(x, y, 'k-') # Plot the route
	#plt.xlim([0,1])
	#plt.ylim([0,1])
	fig.show()

raw_input() # Otherwise the plots disappear instantly
