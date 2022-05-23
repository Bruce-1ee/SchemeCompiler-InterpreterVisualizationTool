f = open("Visualization/old file/python demo/a.txt","a")
f2 = open("Visualization/old file/python demo/b.txt","r")

c = f2.read()
print(c)
f.write(c)