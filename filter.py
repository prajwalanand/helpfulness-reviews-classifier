from json import loads
d = input("")
while(d[0]=='{'):
	g = loads(d)
	if (int(g['helpful'][1]) > 0) and (int(g['reviewTime'].split(" ")[2]) == 2012):
		print(d)
	d = input("")