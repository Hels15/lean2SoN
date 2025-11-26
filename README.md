# lean2SoN


## Chapter1

The SoN graph consists of these nodes:

Let's examine the following expression:
```
return 1;
```


ConstantNode: Represents the one, its control input is the StartNode and it holds the constant as a field.

StartNode: Specail node, has no inputs, it has only outputs its the source node for the graph(graph theory).
ReturnNode: Its control is the startNode and the second input is the constant node.


Data cycle: The constantNode is using the ReturnNode as an input, because the return node already uses the constant node so there would be a data cycle between them them that blows all kinds of invariants during layout.

The nature of the null as an input: Not present in this chapter because everything is binded


## Chapter2

Poset = partially ordered sets
Power set = the set of all possible subsets of a given set, including the set itself and the empty set.

Review here about hasse diagrams and order theory
Order theory(partial order) and hasse diagrams.
- reflexivity
- transivity
- anti-symmetry
Type lattice confusions(terminologies in Simple compared to what others say):

Example(powerset)

For us becuase the lattice is inverted the meet has to go down as usual(less detailed type) for us its the union(join) join has to go up more detailed type and that is the intersection as opposed to the union.

## GraphViz



![Alt text](../image.png)