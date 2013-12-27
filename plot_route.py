import matplotlib.pyplot as plt

input_file = "main.out" # Output from Fortran main program



f = open(input_file)
lines = f.readlines()
for i in range(0, len(lines), 2): # Step of 2 to get both x and y
	# Read x and y in. I know exec can be dangerous, but lazyness wins.
	exec(lines[i])
	exec(lines[i+1])
	# Plot them
	fig = plt.figure(i / 2)
	plt.scatter(x, y, marker = 'o') # Plot cities
	plt.plot(x, y, 'k-') # Plot the route
	fig.show()

raw_input() # Otherwise the plots disappear instantly
