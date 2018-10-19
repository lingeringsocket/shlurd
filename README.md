# shlurd

SHLURD is an attempt to support very limited natural language understanding for small worlds a la [SHRDLU](https://en.wikipedia.org/wiki/SHRDLU).  It's implemented in Scala using [Stanford CoreNLP](https://stanfordnlp.github.io/CoreNLP) as the basis for its parser.  

(So far, SHLURD's understanding is nowhere near as powerful as that of SHRDLU.)

The first practical supported small world domain is
[OpenHAB home automation](openhab/README.md).

We also use the [bAbI tasks from Facebook Research](src/test/qa/README.md) as a QA set.
