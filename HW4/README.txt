Run Wumpus.java or Unicorn.java can test Wumpus world or Unicorn. 

Problem 7
CNFconvert.java can convert logic sentence to CNF sentence.

Use static method CNFconvet.getCNF(sentence) to get a CNF sentence.

Use CNFExtractor to get CNF clauses from the CNF sentence. Set sentence by construct method, and use getNext() method to get clause. 

The element string in logic sentence can use all characters except operators and white space. For example, a sentence can like this: "B[1,1] <=> (P[1,2] | P[2,1])", and it will be converted to CNF sentence like this: "(!B[1,1] P[1,2] P[2,1]) (!P[1,2] B[1,1]) (!P[2,1] B[1,1])"

