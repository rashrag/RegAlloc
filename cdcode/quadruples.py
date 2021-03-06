from Register import *


def create_quad(text_list):
    quad = []
    jump_list = {'<':'JLT', '>':'JGT', '<=':'JLE','>=':'JGT', '!=':'JNE', '==':'JEQ'}
    operators = ['+','-','/','*']

    for i in text_list:
	quad = []
	for j in i:
		j = j[j.index(':')+1:]
        	j = j.split()
		if(j[0] == 'end'):
		    break
		if('goto' in j):
			if(len(j) > 2):
				#print("j[2] =", j[2])
		 		quad.append([jump_list[j[2]],j[1],j[3],j[5]])
			else:
				quad.append(["JMP", "None","None",j[-1]])
			
		elif (len(j)>3):
		    quad.append([j[3], j[2], j[4], j[0]])
		else:
		    quad.append([j[2], j[-1], 'None', j[0]])
	
	register(quad)


    
