# Generic checker for CPSolver's constraints

[![Build Status](https://travis-ci.org/vrombouts/Generic-checker-for-CP-Solver-s-constraints.svg?branch=master)](https://travis-ci.org/vrombouts/Generic-checker-for-CP-Solver-s-constraints)
[![codecov](https://codecov.io/gh/vrombouts/Generic-checker-for-CP-Solver-s-constraints/branch/master/graph/badge.svg)](https://codecov.io/gh/vrombouts/Generic-checker-for-CP-Solver-s-constraints)


In constraint programming, the filtering of constraints are of the upmost importance. 
Therefore, developers are constantly creating filtering algorithms for each of those constraints.
The tool in this repository can be used to test and debug those filtering algorithms for global constraints.
This tool is coded in Scala and can be used to tests filtering algorithms written in Java and Scala.

## Why using this tool?

Thanks to this tool, you can test that the application of some propagation algorithms such as Arc Consistency, Bound Consistency and Range Consistency is correct. For this, the tool will check that the application of the tested filtering implementation will not remove any solution but also that after applying the tested filtering, the propagation algorithm has been respected. This is done by comparing the tested implementation with a bug free one.

You can also check that one filtering algorithm filters less or more than another. This can be very useful, especially if you want to check that the application of a tested filtering will not remove any solution. For this, you can simply compare that the tested algorithm filters less than a bug free algorithm performing Arc Consistency. This is very important since removing solutions constitutes a serious problem. 

Finally, for constraints implemented using a trailing algorithm, this tool allows to check the trailing procedure used through the search. Indeed, it can be used to check the application of a tested filtering implementation during the search and what happens when doing state restoration.

When testing all these stuffs, user reports are created in out/statistics containing statistics of domains variables on which the filtering algorithm does not work correctly. These user reports can be a big help for the user while debugging. 

## How to use this tool?

To know how to use the tool, you can refer to the [userGuide](https://github.com/vrombouts/Generic-checker-for-CP-Solver-s-constraints/blob/master/documentation/userGuide.md)

## How to build the project?

After having cloned or downloaded the repository, the tool is built using sbt. So, it requires the installation of sbt. Since it is written in Scala, it also requires to install Scala (version 2.12.6). All the other libraries that are used in the project are autoamatically imported during the build phase and so does not require any other installation.
