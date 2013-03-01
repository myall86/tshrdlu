# tshrdlu
=======

Author: **Jason Baldridge** (jasonbaldridge@gmail.com)

This is a parent repository for [project](https://github.com/utcompling/applied-nlp/wiki/Course-Project) related code for [Applied NLP course](https://github.com/utcompling/applied-nlp/wiki) being taught by [Jason Baldridge](http://www.jasonbaldridge.com) at [UT Austin](http://www.utexas.edu). This involves creating applications that use Twitter streams and can take automated actions as Twitter users, using natural language processing and machine learning.

The name "tshrdlu" comes from Twitter+[SHRDLU](http://en.wikipedia.org/wiki/SHRDLU).

For more information, updates, etc., follow [@appliednlp](https://twitter.com/appliednlp) on Twitter. The [@tshrdlu](https://twitter.com/tshrdlu) account may also start doing some tweeting of its own soon (by which I mean automated tweeting).

## Requirements

* Version 1.6 of the Java 2 SDK (http://java.sun.com)

## Configuring your environment variables

The easiest thing to do is to set the environment variable `JAVA_HOME`
to the relevant locations on your system. Set `JAVA_HOME` to match the
top level directory containing the Java installation you want to use.

## Adding authentification

    $ mv local.sbt.template local.sbt

Create a new application via
[Twitter](https://dev.twitter.com/apps/new) and copy the auth data
into `local.sbt`.

## Building the system from source

tshrdlu uses SBT (Simple Build Tool) with a standard directory
structure. To build tshrdlu, type:

	$ sbt compile

This will compile the source files and put them in
`./target/classes`. If this is your first time running it, you will see
messages about Scala being downloaded -- this is fine and
expected. Once that is over, the tshrdlu code will be compiled.

To try out other build targets, do:

    $ sbt

This will drop you into the SBT interface. To see the actions that are
possible, hit the TAB key. (In general, you can do auto-completion on
any command prefix in SBT, hurrah!)

To make sure all the tests pass, do:

	$ sbt test

Documentation for SBT is at <http://www.scala-sbt.org/>
