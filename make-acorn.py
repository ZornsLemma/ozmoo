# SFTODO: Do proper argument parsing etc

from __future__ import print_function
import sys

labels = {}
with open("temp/acme_labels.txt", "r") as f:
    for line in f.readlines():
        line = line[:-1]
        components = line.split("=")
        value = components[1].strip()
        i = value.find(";")
        if i != -1:
            value = value[:i]
        labels[components[0].strip()] = int(value.strip().replace("$", "0x"), 0)
print(labels)


with open("temp/base.ssd", "rb") as f:
    ssd = f.read()
