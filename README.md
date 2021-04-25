# 🦈 GIAA Programming Language
# SER 502 Spring 2021 - Project Team 7

[![N|Solid](http://eu.swi-prolog.org/icons/swipl.png)](http://eu.swi-prolog.org/icons/swipl.png)

[![Build Status](https://travis-ci.org/joemccann/dillinger.svg?branch=master)](https://travis-ci.org/joemccann/dillinger)

### GIAA is a simple programming language developed to compute trivial arithmetic operations, conditions, and loops.

#### Team Members:
1. 👨🏻‍💻 Abhishek Mohabe
2. 👩🏻‍💻 Apoorva Giliyal 
3. 👩🏻‍💻 Itiparna Mahala
4. 👨🏻‍💻 Gautham Krishna

## ⚙ Tools Used
- SWI-Prolog Desktop Application
- Python3

## 🕶 Project Video Link

- Youtube Video Link - ([Link 🚀](https://youtu.be/MsYPDBumuR8))

## ⚙ How to Execute it

- Clone the git repository into your local machine
- Install python 3.9 and SWI-Prolog 8.3.22 on your local machine
- SWI-Prolog Download ([Link 🚀](https://www.swi-prolog.org/Download.html))
- Python3 Download ([Link 🚀](https://www.python.org/downloads/))
- Sample test programs are saved in the 'data' folder with .GIAA extension
- Open the terminal and execute the below command
```
User-MacBook:~ user$ swipl
```
- Enter the path to the .pl file
```
?- ['/Users/user/Desktop/SER502-Spring2021-Team7/src/GIAA.pl'].  
```
- Run the sample program by parsing tokens through a lexer
```
?- giaa('/Users/user/Desktop/SER502-Spring2021-Team7/src/Lexer.py','/Users/user/Desktop/SER502-Spring2021-Team7/data/boolVar.GIAA').
```
