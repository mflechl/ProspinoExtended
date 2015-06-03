import sys

filename = sys.argv[1]

f = open(filename, "r")

lines = []

import re

for line in f:
	res = re.search("(\d+\.\d+)(?!d0)", line)
	while res:
		line = line[:res.end()] + "d0" + line[res.end():]
		res = re.search("(\d+\.\d+)(?!d0)", line)

	res = re.search("(\d+\.\d*)e(([-+]?)\d+)", line)
	while res:
		line = line[:res.start()] + res.group(1) + "d" + res.group(2) + line[res.end():]
		res = re.search("(\d+\.\d+)e(([-+]?)\d+)", line)

	lines.append(line)

f.close()

f = open(filename, "w")

f.write("".join(lines))

f.close()

