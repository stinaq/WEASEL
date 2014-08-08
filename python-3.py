#!/usr/bin/python
# An obfuscated python implementation in a single line to demonstrate the
# flexibility of functional python as well as several interesting abuses
# of language features
from random import random as r, choice as ch
from sys import stdout as so
target = 'methinks it is like a weasel'
alpha = 'abcdefghijklmnopqrstuvwxyz '
rate = 0.04
generation_size = 100

for i in enumerate(iter(lambda:[ch(alpha) for i in range(len(target))] if 'gs' not in globals() else None if globals().get('l',False) else max(map(lambda c:(c,sum(x==y for x,y in zip(c,target))), [map(lambda d:ch(alpha) if r()<rate else d,gs) for i in range(generation_size)]), key=lambda x:x[1])[0],None)): gs,globals()['l'],t=i[1],globals().get('gs')==list(target),so.write('%d %s\n'%(i[0]+1,''.join(i[1])))
