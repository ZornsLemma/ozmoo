# List all the constants referenced in !if etc statements; the idea is that by doing
# sort | uniq (-c) on the output, typos will be identifiable.

import sys

with open(sys.argv[1], "r") as f:
    for line in f:
        line = line[:-1]
        components = line.split()
        in_condition = False
        #print "X", line
        for x in components:
            if x.startswith(";"):
                break
            if x.lower().startswith("!if"):
                in_condition = True
            elif x == "{":
                in_condition = False
            elif in_condition:
                if len(x) > 2:
                    print x
