#!/usr/bin/python

## This is just a quick script to read in all unigram constraints
## and then output all combinations as bigram constraints

def readConstraints(filename = "../sublex/sublex-constraints-unigram.txt"):
    f = open(filename, "r")
    constraints = []
    for line in f:
        constraints.append(line.strip())
    f.close()
    return [x for x in constraints if len(x) > 0]


def bigramizeConstraints(constraints):
    faithfulness = [x for x in constraints if x[0] == 'F']
    markedness =   [x for x in constraints if x[0] != 'F']
    bigramMarkedness = [x+" "+y for x in markedness for y in markedness]
    return bigramMarkedness + faithfulness

def writeConstraints(constraints,
                     outfile = "../sublex/sublex-constraints-bigram.txt"):
    f = open(outfile, "w")
    for c in constraints:
        f.write(c + "\n")
    f.close()


def main():
    cons = readConstraints()
    bigramcons = bigramizeConstraints(cons)
    writeConstraints(bigramcons)

if __name__ == "main":
    main()
