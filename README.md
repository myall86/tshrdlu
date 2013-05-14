# tshrdlu
=======

Author: **Cuong Chau** (ckcuong@cs.utexas.edu)

## Requirements

* Java version 1.7.0_15 (http://java.sun.com)

## Notifications

It only takes a few seconds for the program to reply to user.

When you start running the program, you should wait a few minutes (at least 5 minutes) before you begin chatting with the bot
because the system needs to collect tweets for indexing into Lucene database.
Of course, you can chat immediately if you want, but the responses often bad due to the small number of tweets in Lucene database
(I ran the program two days before asking the evaluators begin chatting with my bot).

If this is the first time you chat with the bot, its responses often bad in a first few tweets (around 10-15) due to lack of information. 
So, it is better if you provide some information about you as well as the topics you wanna talk in a first few tweets. 

## Configuring your environment variables

The easiest thing to do is to set the environment variables `JAVA_HOME`
and `TSHRDLU_DIR` to the relevant locations on your system. Set `JAVA_HOME`
to match the top level directory containing the Java installation you
want to use.

Next, add the directory `TSHRDLU_DIR/bin` to your path. For example, you
can set the path in your `.bashrc` file as follows:

	export PATH=$PATH:$TSHRDLU_DIR/bin

Once you have taken care of these three things, you should be able to
build and use tshrdlu.

If you plan to index and search objects using the provided code based
on Lucene, you can customize the directory where on-disk indexes are
stored (the default is the tempdir, check the directory `tshrdlu`) by
setting the environment variable `TSHRDLU_INDEX_DIR`.


## Building the system from source

tshrdlu uses SBT (Simple Build Tool) with a standard directory
structure.  To build tshrdlu, type (in the `TSHRDLU_DIR` directory):

	$ ./build update compile

This will compile the source files and put them in
`./target/classes`. If this is your first time running it, you will see
messages about Scala being downloaded -- this is fine and
expected. Once that is over, the tshrdlu code will be compiled.

To try out other build targets, do:

	$ ./build

This will drop you into the SBT interface. To see the actions that are
possible, hit the TAB key. (In general, you can do auto-completion on
any command prefix in SBT, hurrah!)

To make sure all the tests pass, do:

	$ ./build test

Documentation for SBT is at <http://www.scala-sbt.org/>

Note: if you have SBT already installed on your system, you can
also just call it directly with "sbt" in `TSHRDLU_DIR`.

## Running the system

To run tshrdlu, type (in the `TSHRDLU_DIR` directory):

	$ bin/tshrdlu bot

# Questions or suggestions?

Email Cuong Chau: <ckcuong@cs.utexas.edu>

