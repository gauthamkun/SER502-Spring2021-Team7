import sys
from functools import reduce
from tokenize import tokenize, untokenize, NUMBER, STRING, NAME, OP
from io import BytesIO
import re

def lexer(file):
    lexer = ' ['
   
    process = open(file, 'r').read()
    value = tokenize(BytesIO(process.encode('utf-8')).readline)
    listvalue = []
    for toknum, tokval, _, _, _ in value:
        if(len(tokval) != 0):
            if tokval != "\n" and tokval != "utf-8" and tokval != "\t" and tokval != '"':
                if tokval == '[':
                    listvalue.append(tokval)
                  
                  
                    continue
                elif tokval == ']':
                    listvalue.append(tokval)
                    lexer += ("".join(listvalue))
                    listvalue = []
                    
                else:
                    if tokval.startswith('"'):
                        x = tokval[1:-1]
                        print(x)
                        lexer +="'"+'"'+"'"+','+"'"+x+"'"+','+"'"+'"'+"'"

                    elif tokval == '!=':
                        lexer += "'"+tokval+"'"
                    elif tokval == ')' or tokval == '(' or tokval == '{' or tokval == '}':
                        lexer += "'"+tokval+"'"
                    else:
                        lexer += tokval
                lexer += ","
    lexer = lexer[:-1]
    lexer += " ]"
    print(lexer)
    
    return lexer
    
if __name__ == "__main__":

    fileName = sys.argv[1]
    tokens = lexer(fileName)   