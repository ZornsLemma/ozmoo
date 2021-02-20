import sys

line = ""
depth = 0
in_comment = False
with open(sys.argv[1], "r") as f:
    while True:
        c = f.read(1)
        if not c:
            break
        line += c
        if c == '\n':
            line += '.' * (depth * 4)
            in_comment = False
        if c == ';':
            in_comment = True
        if not in_comment:
            if c == '{':
                depth += 1
            elif c == '}':
                depth -= 1
                if depth < 0:
                    line += 'NEGATIVEDEPTH'
print depth
if depth != 0:
    with open("foo.txt", "w") as f:
        f.write(line)
