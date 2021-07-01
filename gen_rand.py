#131923 - 33k - 30k - Years 2013&14, at least 1 marked helpful
#80307 - 20k - 15k - 2012, at least 1 marked helpful
import random
n, m = 15000, 1000
a = random.sample(range(1,n), m)
for i in range(1, n):
	line = input("")
	if i in a:
		print(line)