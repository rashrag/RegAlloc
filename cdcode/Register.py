
def register(setOfLines):
	use = list()
	definitions = list()
	next_use=list()
	live = []
	print " The quadruple created for the block is as follows : "
	for line in setOfLines:	
		print line
	print
	print
	#global use, definitions, live,next_use	
	for i in range(len(setOfLines)):
		use.append([setOfLines[i][1],setOfLines[i][2]])
		definitions.append(setOfLines[i][3])

	for i in range(len(definitions)):
		for j in range(len(use)):
			if(definitions[i]==use[j][0] or definitions[i] == use[j][1]):
				live.append([definitions[i],i,j])
 
	a = live
	i=0;j=0
	flag = 0
	while(i<len(a)):
		j=i+1
		while(j<len(a)):	
			if(i<len(a)and j< len(a)):
				if(a[i][0]==a[j][0]):
					if(a[i][1]==a[j][1]):
						if(a[i][2]>a[j][2]):
							a.remove(a[j])
							flag = 1
							i=0;j=1
							break
						else:
							a.remove(a[i])
							flag = 1
							i=0;j=1
							break
					else:
						flag = 0	
				else:
					flag = 0			
			j+=1
		if(flag==0):
			i+=1
	print " The list of variables which are live in the block is as follows"
	for variable in a:	
		print variable
	print	
	c = a
	reg = []
	temp = []
	i = 0
	while(i<len(c)):	
		lower  = c[i][1]
		upper = c[i][2]
		temp = []
		j = i+1
		temp.append(c[i])
		while(j<len(c)):		
			low1 = c[j][1]
			up1 = c[j][2]
			list1 = list(range(lower,upper))
			list2 = list(range(low1,up1))
			for k in list1:
				if(k in list2):
					break
			else:
				temp.append(c[j])
				c.remove(c[j])
			j+=1
	
		reg.append(temp)
		i+=1
	registers = ['r0','r1','r2','r3','r4','r5','r6','r7','r8','r9','r10']
	for i in range(len(reg)):
		for j in reg[i]:
			print j[0] ,"uses register", registers[i]
			print
	print '*'*100
	print
	for i in reg:
		reg.remove(i)
