# Generic checker for CPSolver's constraints

[![Build Status](https://travis-ci.org/vrombouts/Generic-checker-for-CP-Solver-s-constraints.svg?branch=master)](https://travis-ci.org/vrombouts/Generic-checker-for-CP-Solver-s-constraints)
[![codecov](https://codecov.io/gh/vrombouts/Generic-checker-for-CP-Solver-s-constraints/branch/master/graph/badge.svg)](https://codecov.io/gh/vrombouts/Generic-checker-for-CP-Solver-s-constraints)


In constraint programming, the filtering of constraints are of the upmost importance. 
Therefore, developers are constantly creating filtering algorithms for each of those constraints.
The tool in this repository can be used to test and debug those filtering algorithms for global constraints.
This tool is coded in Scala and can be used to tests filtering algorithms written in Java and Scala.

## Why using this tool?

Thanks to this tool, you can check that the application of your constraint filtering will not remove any solution. This is very important since removing solutions constitutes a serious problem. 

You can also test the correct application of some propagation algorithms such as Arc Consistency and Bound Consistency. In this case, the tool will check that the application of your filtering implementation will not remove any solution but also that after applying your filtering, the propagation algorithm has been respected.

Finally, for constraints implemented using a trailing algorithm, this tool allows to check the trailing procedure used through the search. Indeed, it can be used to check the application of your filtering implementation during the search and what happens when doing state restoration.

When testing all these stuffs, user reports are created containing statistics of domains variables on which the filtering algorithm does not work correctly. These user reports can be a big help for the user while debugging. 

## How to use this tool?

To know how to use the tool, you can refer to the userGuide at https://github.com/vrombouts/Generic-checker-for-CP-Solver-s-constraints/blob/master/documentation/userGuide.md

## How to build the project?

After having cloned or downloaded the repository, the tool is built using sbt. So, it requires the installation of sbt. Since it is written in Scala, it also requires to install Scala (version 2.11.4). All the other libraries that are used in the project are autoamatically imported during the build phase and so does not require any other installation.

The project will be soon available in a .jar that can be simply added to the user's project as a library.
