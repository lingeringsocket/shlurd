# SHLURD QA

As a test of SHLURD's language understanding and world modeling
capabilities, we make use of the
[bAbI project](https://research.fb.com/downloads/babi/) from Facebook
Research.  Currently, we test with the QA tasks.  Out of the 20 tasks provided, we currently produce 100% correct results for 15 of them:

* qa1-9
* qa11-4
* qa16-17

The remaining tasks (qa10, qa15, qa18, qa19, qa20) are not yet understood
by SHLURD.

To run the tasks:

1. Download the [QA tasks](http://www.thespermwhale.com/jaseweston/babi/tasks_1-20_v1-2.tar.gz)
1. Unpack the archive, producing `/path/to/tasks_1-20_v1-2`
1. `cd /your/clone/of/shlurd/src/test/qa`
1. `babi.sh /path/to/tasks_1-20_v1-2/en-valid`

This will run through all supported tasks, and should produce no errors.
To run one particular task, supply the filename as an extra argument, e.g.

```
babi.sh /path/to/tasks_1-20_v1-2/en-valid qa4_valid.txt
```

The [babi.sh](babi.sh) script iterates over the task data files,
executing each one via
[SpcInterpretTester](../../main/scala/com/lingeringsocket/shlurd/platonic/SpcInterpretTester.scala).
A [belief set](../resources/expect/babi-qa-beliefs.txt) is loaded
before executing each task in order to define the expected rules of
the world model.

## References

* Jason Weston, Antoine Bordes, Sumit Chopra, Tomas Mikolov, Alexander M. 
  Rush, Bart van Merriënboer, "`Towards AI-Complete Question Answering: A Set of Prerequisite Toy Tasks`__", *arXiv:1502.05698 [cs.AI]*.
