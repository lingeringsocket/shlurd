[SHLURD](../README.md) can be deployed as a set of OSGI bundles, e.g. for use in the OpenHab addon.

The build is a little convoluted due to the way the Stanford CoreNLP artifacts are packaged (plus limitations in the sbt-osgi-felix build plugin).  To execute it, just run the [`bin/stageArtifacts` script](bin/stageArtifacts).

A successful run will (re)build SHLURD and package it (together with all dependencies) into OSGI bundle jars in [your local `staging` directory](staging).
