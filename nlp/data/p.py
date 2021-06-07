inp = open("placenames_annotated.txt", "r")

out = open("placenames.txt", "w")

for line in inp.readlines():
  l = line.split(":")
  out.write(l[0] + "\n")

inp.close()
out.close()
