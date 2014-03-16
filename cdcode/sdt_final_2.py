import ply.lex as lex
import ply.yacc as yacc
import quadruples
from Blocks import *

tokens = ('ID','PLUS','MULT', 'EQUALS', 'MINUS', 'OPBR','CLBR','RELOP','AND','OR','NOT','WHILE','ENDWHILE','IF','ENDIF','ELSE','DIV')
t_PLUS = r'\+'
t_MULT = r'\*'
t_EQUALS = r'\='
t_MINUS = r'\-'
t_OPBR = r'\('
t_CLBR = r'\)'
t_DIV = r'/'
t_ignore=' \t\n'

lastdef = ""
tempvar = 0
tok = list()
labelvar = 0
lineno = 1
falsearr = []
endifarr = []
whilearr = []

def t_RELOP(t):
    r'<|>|==|<=|>=|!='
    return t

def t_WHILE(t):
    r'while'
    return t

def t_ENDWHILE(t):
    r'endwhile'
    global lineno, whilearr
    last_while = whilearr.pop()
    tok.append((lineno, 'goto '+str(last_while-1)))
    lineno+=1
    tok[last_while-1] = (tok[last_while-1][0], tok[last_while-1][1]+str(lineno))
    if(lastdef == 'endif'):
        last = falsearr.pop()
        tok[last[1]-1] = (tok[last[1]-1][0], tok[last[1]-1][1]+str(lineno))
        if(len(endifarr)!=0):
            first = endifarr[0]
            endifarr.remove(endifarr[0])
            tok[first-1] = (tok[first-1][0], tok[first-1][1]+str(lineno))
            
    elif (lastdef == 'endelse'):
        last = endifarr[-1]
        first = endifarr[0]
        endifarr.remove(endifarr[-1])
        endifarr.remove(endifarr[0])
        tok[last-1] = (tok[last-1][0], tok[last-1][1]+str(lineno-1))
        tok[first-1] = (tok[first-1][0], tok[first-1][1]+str(lineno-1))
    
    
def t_IF(t):
    r'if'
    global endifarr, falsearr, lastdef, lineno
    if(lastdef == 'endif'):
        last = falsearr.pop()
        tok[last[1]-1] = (tok[last[1]-1][0], tok[last[1]-1][1]+str(lineno))
    elif (lastdef == 'endelse'):
        last = endifarr[-1]
        first = endifarr[0]
        endifarr.remove(endifarr[1])
        endifarr.remove(endifarr[0])
        tok[last-1] = (tok[last-1][0], tok[last-1][1]+str(lineno))
        tok[first-1] = (tok[first-1][0], tok[first-1][1]+str(lineno))
    return t


def t_else(t):
    r'else'
    global lineno, endif, falsearr, lastdef
    if(lastdef == 'endif'):
        last = falsearr.pop()
        tok[last[1]-1] = (tok[last[1]-1][0], tok[last[1]-1][1]+str(lineno))
    

def t_endelse(t):
    r'endelse'
    global lineno
    endifarr.append(lineno)
    tok.append((lineno, 'goto '))
    lineno+=1

def t_endif(t):
    r'endif'
    global endifarr, lineno
    tok.append((lineno, 'goto '))
    endifarr.append(lineno)
    lineno+=1


def t_DO(t):
    r'do'
    print(t)
    return t

def t_ID(t):
    r'\w+'
    return t

def p_sentence(p):
    """sentence : ID EQUALS expression
              | ID"""
    global lineno
    if(lastdef == 'endif'):
        last = falsearr.pop()
        tok[last[1]-1] = (tok[last[1]-1][0], tok[last[1]-1][1]+str(lineno-1))
        if(len(endifarr)!=0):
            first = endifarr[0]
            endifarr.remove(endifarr[0])
            tok[first-1] = (tok[first-1][0], tok[first-1][1]+str(lineno-1))
            
    elif (lastdef == 'endelse'):
        last = endifarr[-1]
        first = endifarr[0]
        endifarr.remove(endifarr[-1])
        endifarr.remove(endifarr[0])
        tok[last-1] = (tok[last-1][0], tok[last-1][1]+str(lineno-1))
        tok[first-1] = (tok[first-1][0], tok[first-1][1]+str(lineno-1))

    if(len(p)==2):
        p[0] = p[1]
        tok.append((lineno, p[0]))
    else:
        fin = str(p[1])+" = "+str(p[3])
        fin = fin.split("=")
        f = fin[0]+" = "+fin[1]
        p[0] = f
        tok.append((lineno, f))
        lineno+=1

def get_code(p, temp):
    var1 = None
    var2 = None
    if('t' in str(p[3])):
        var1 = str(p[3])[str(p[3]).index('t'):str(p[3]).index('=')]
    else:
        var1 = str(p[3])
    if('t' in str(p[1])):
        var2 = str(p[1])[str(p[1]).index('t'):str(p[1]).index('=')]
    else:
        var2 = str(p[1])
    return (temp+" = "+var2+" "+str(p[2])+" "+var1)

def get_expr(p):
    global tok, lineno
    temp = newtemp()
    p[0] = get_code(p, temp)
    tok.append((lineno,str(p[0])))
    lineno+=1

def p_expression_add(p):
    'expression : expression PLUS expression'
    get_expr(p)
   

def p_expression_mult(p):
    'expression : expression MULT expression'
    get_expr(p)

def p_expression_minus(p):
    'expression : expression MINUS expression'
    get_expr(p)

def p_expression_divide(p):
    'expression : expression DIV expression'
    get_expr(p)

def p_expression_brackets(p):
    'expression : OPBR expression CLBR'
    global tok, lineno
    p[0] = str(p[2])

def p_expression(p):
    'expression : ID'
    global tok
    p[0] = p[1]

def newtemp():
    global tempvar
    tempvar+=1
    return ("t"+str(tempvar))

def p_if(p):
    'sentence : IF boo'
    global lineno, falsearr
    p[0] = 'if '+str(p[2])+' goto '+str((lineno+2))
    tok.append((lineno, p[0]))
    lineno+=1
    falsearr.append(('if',lineno))
    tok.append((lineno, 'goto '))
    lineno+=1

def p_relop(p):
    """boo : boo RELOP boo
           | ID
    """
    if(len(p)>2):
        p[0] = str(p[1])+" "+str(p[2])+" "+str(p[3])
    else:
        p[0] = str(p[1])

def p_while(p):
    'sentence : WHILE boo'
    global lineno, whilearr
    p[0] = 'if '+str(p[2])+' goto '+str((lineno+2))
    tok.append((lineno, p[0]))
    lineno+=1
    whilearr.append(lineno)
    tok.append((lineno, 'goto '))
    lineno+=1

lexer = lex.lex()
parser = yacc.yacc()

f = open('testing.txt','r')
lines = f.readlines()
lines.append('end')

for i in lines:
    i = i.strip()
    parser.parse(i)
    lastdef = i.strip()

while(len(endifarr)!=0):
    last = endifarr.pop()
    tok[last-1] = (tok[last-1][0], tok[last-1][1]+str(lineno))

while(len(falsearr)!=0):
    last = falsearr.pop()
    tok[last[1]-1] = (tok[last[1]-1][0], tok[last[1]-1][1]+str(lineno-1))

g = open('finaltest.txt','a')

for i in tok:
    g.write(str(i[0])+" : "+str(i[1])+"\n")
    
f.close()
g.close()
readFile('finaltest.txt')


basicBlocks()

