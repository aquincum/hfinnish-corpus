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


def separateFM(constraints):
    faithfulness = [x for x in constraints if x[0] == 'F']
    markedness =   [x for x in constraints if x[0] != 'F']
    return (faithfulness,markedness)

def bigramizeConstraints(constraints):
    (faithfulness,markedness) = separateFM(constraints)
    bigramMarkedness = [x+" "+y for x in markedness for y in markedness]
    return bigramMarkedness + faithfulness

def writeConstraints(constraints,
                     outfile = "../sublex/sublex-constraints-bigram.txt"):
    f = open(outfile, "w")
    for c in constraints:
        f.write(c + "\n")
    f.close()

    
class Vowel:
    def __init__(self, symbol, features):
        self.symbol = symbol
        self.features = features
    def __repr__(self):
        return "/" + self.symbol + "/"
    def isVowel(self, other_features):
        for f in other_features:
            pm = f[0]
            fname = f[1:]
            pmother = "+" if pm == "-" else "-"
            if pmother+fname in self.features:
                return False
        return True

class Finnish:
    def __init__(self):
        self.vowels = [
            Vowel("a",["-front","-rounded","-high","+low"]),
            Vowel("o",["-front","+rounded","-high","-low"]),
            Vowel("u",["-front","+rounded","+high","-low"]),
            Vowel("e",["+front","-rounded","-high","-low"]),
            Vowel("i",["+front","-rounded","+high","-low"]),
            Vowel("ä",["+front","-rounded","-high","+low"]),
            Vowel("ö",["+front","+rounded","-high","-low"]),
            Vowel("y",["+front","+rounded","+high","-low"])
        ]
    def subset(self, features):
        return [v for v in self.vowels if v.isVowel(features)]
        
finnish = Finnish()

def build2FeatConstraints():
    fs = ["+front","-front","+rounded","-rounded","+high","-high","+low","-low"]
    subsets = []
    for f1 in fs:
        for f2 in fs:
            selected = finnish.subset([f1,f2])
            if selected not in subsets:
                subsets.append(selected)
                if f1 == f2:
                    print "["+f1+"]",
                else:
                    print "["+f1+","+f2+"]",
                print "... selecting " + str(selected)
            else:
                print "NOT SELECTED: " + f1 + "," + f2 + ", selecting" + str(selected)
                
    

def main():
    cons = readConstraints()
    bigramcons = bigramizeConstraints(cons)
    writeConstraints(bigramcons)




    
if __name__ == "main":
    main()

### Playing with bundle creation :(

feats = ['front', 'rounded', 'high', 'low']
allfeat = [x+y for x in ['-','+'] for y in feats]

def legal(bundle):
    if len(set(bundle)) < len(bundle): return False
    for f in feats:
        if "+"+f in bundle and "-"+f in bundle:
            return False
    if "+low" in bundle and "+high" in bundle: return False
    if "+low" in bundle and "-high" in bundle: return False
    if "-low" in bundle and "+high" in bundle: return False
    return True

unigram1 = [x for x in allfeat if legal([x])]        
