# Generic checker for CPSolver's constraints
In constraint programming, the filtering of constraints are of the upmost importance. 
Therefore, filtering algorithms for each of those constraints are constantly researched.
The tool in this repository can be used to test those filtering algorithms for global constraints.
## How it works

## code example
To test the filtering of a constraint, you always need to give 2 informations.
* A checker telling if a solution is accepted or not by the constraint tested.
* Your own filtering algorithm of the constraint.
There are also other option that can be used such as 
* modifying the generator of variables.
* setting a seed for the tests.
* getting statistics of the results of this tool.

This tool has already be used on two different CPSolver's: Choco and OscaR.
Here is an example for the alldifferent constraint in OscaR.
```
ctrl+v
```
Here is an exemple for the alldifferent constraint in Choco.
