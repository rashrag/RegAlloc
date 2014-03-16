import quadruples
setOfLines = []
lineNumber= []
leaderNumber = [1]
blockList = []
def readFile(filename):
# opening files and reading the lines:
	f = open(filename,"r")
	global setOfLines 
	setOfLines = f.readlines()
	print '*'*100
	print 
	print "The 3-address code generated is as follows : "
	for i in setOfLines:
		print i
	print
	print
	print '*'*100
def basicBlocks():
	global lineNumber
	global leaderNumber
	global blockList
	global setOfLines	
	for line in setOfLines:
		if "goto" in line:
			lineNumber.append(int(line.split(":")[0]))
			leaderNumber.append(int(line.split("goto")[1].strip()))
	for i in lineNumber :
		leaderNumber.append(int(i)+1)
	leaderNumber = list(set(leaderNumber)) 
	leaderNumber.sort()
	print "The List of LeaderNumber instructions are : ",leaderNumber 
	count = 1
	for i in range(len(leaderNumber)-1):
		print "Block: ", str(count) 
		temp = []
		for j in range(leaderNumber[i]-1,leaderNumber[i+1]-1):
			print setOfLines[j]
			temp.append(setOfLines[j])
		blockList.append(temp)
		count+=1
		print
		print "-"*100
	k = leaderNumber[len(leaderNumber)-1]
	temp = []	
	print "Block", str(count)
	for a in range(k-1,len(setOfLines)):
		print setOfLines[a]
		temp.append(setOfLines[a])
	print '-'*100	
	blockList.append(temp)
	print
	print
	quadruples.create_quad(blockList)



