import sys
import re

tests = []
user_id = 0

grade = 0
for line in sys.stdin:
	if "Valid profile id:" in line:
		_, _, after = line.partition("Valid profile id:")
		user_id,_,_ = after.partition('')

	else:
		if "  + " in line:
			_, _, after = line.partition("(")
			points, _, _ = after.partition("p")
			grade += int(points)


f = open(user_id+".csv", "w")
f.write(user_id+","+str(grade)+"\n")
print(user_id,grade)
f.close()
