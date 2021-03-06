# Intro

**shlurd-openhab** is an addon for [OpenHAB 2](https://www.openhab.org) which installs a new human language interpreter based on [SHLURD](../README.md).  It allows you to ask questions about the state of your house (e.g. "is the bedroom window open?") as well as give commands (e.g. "turn off all lights in the kitchen").  Since speech-to-text input is often unpunctuated, the addon is designed to automatically distinguish statements, questions, and commands based on syntax alone.  At the moment, the only natural language supported is English.

In order for it to work with your smarthome, it's currently required that you follow the item structure (name+label+group) conventions produced by the [OpenHab Home Builder](https://docs.openhab.org/configuration/homebuilder.html).

For examples of what's supported against the OpenHab demo items, see this [test dialogue](test/demo-out.ref).

Caveat: some known parsing/naming problems are demonstrated in [errata](test/errata-out.ref).  Some of these are due to incorrect parses by Core NLP, some are due to oddities in the demo items definitions, some are due to ambiguities in the English language, and (naturally!) some are due to limitations in SHLURD itself.

# Deploy

1. Browse to your OpenHAB service web UI, e.g.  http://localhost:8080
1. Select **Standard** setup
1. Select **Paper UI**
1. Open the **Add-Ons** menu from the sidebar
1. Select the **Misc** tab
1. Install the **Eclipse IoT Market** addon if you haven't already
1. Select the **Voice** tab
1. Install the **SHLURD Human Language Interpreter** addon

Notes:

* The addon jar and Java heap memory requirements are quite large due to the inclusion of Stanford CoreNLP models, so it may not be suitable for lightweight deployments (e.g. Raspberry Pi).
* The instructions above will only work for OpenHab installations running a version which includes [this Eclipse Smart Home pull request](https://github.com/eclipse/smarthome/pull/5349).  For older versions, either upgrade or use the manual instructions given below.

Manual installation:

1. Download the latest release archive .jar from [the Eclipse IoT marketplace](https://marketplace.eclipse.org/content/shlurd-human-language-interpreter) or from [lingeringsocket.com](https://lingeringsocket.com/com.lingeringsocket.shlurd.openhab).
1. Move the jar into your openhab `addons` directory (e.g. `/usr/share/openhab2/addons`)

# Configure

From the OpenHab Paper UI:

1. **Configuration > System**
1. Scroll down to **Voice** section
1. For **Default Human Language Interpeter**, switch to **SHLURD-based Interpreter**
1. **Save**

As an alternative, you can configure the same setting via a configuration file; edit `runtime.cfg` (maybe in `/etc/openhab2/services`) and change `org.eclipse.smarthome.voice:defaultHLI` to `shlurdhli`

Note that enabling SHLURD will cause openhab startup to take a bit longer, as loading the CoreNLP models into memory eats some time.

# Try It Out

In order to really speak with OpenHAB, you need to set up audio source/sink and speech recognition/generation services, as described in [the OpenHAB multimedia docs](http://docs.openhab.org/configuration/multimedia.html).

You can also use a text interface via [the OpenHAB console](http://docs.openhab.org/administration/console.html).  Try something like:

    smarthome:voice interpret are any lights on?
    
You should get a response such as `Yes, some lights are on.`

# Android

If you want to use your Android phone to send voice commands, add this to your items file:

    String VoiceCommand "Voice Command"                                             
                                                                                
 And then this to your rules file:

    rule InterpretVoice                                                             
    when                                                                            
      Item VoiceCommand received command                                            
    then                                                                            
      var String command = VoiceCommand.state.toString                              
      var String result = interpret(command, "shlurdhli")                           
      say(result)                                                                   
    end                                                                             

This forwarding is necessary because the Android app ignores the setting for **Default Human Language Interpreter**.

# Customize

The addon comes with a default ontology for a few openhab item types (currently only doors, windows, lights, heaters, and switches).  You can extend this with your own types by adding a custom belief file.  For example, suppose you have services running on your home network, e.g. a media service and an alarm service.  Using the [network binding](http://docs.openhab.org/addons/bindings/network/readme.html), you've already defined a Switch item for each service for monitoring purposes.

You can write a custom belief file like this:

    A service must be either on or off.
    A server is a service.
    If a service is up, then equivalently the service is on.
    If a service is down, then equivalently the service is off.
    If a service is available, then equivalently the service is on.
    If a service is unavailable, then equivalently the service  is off.
    If a service is running, then equivalently the service is on.
    If a service is stopped, then equivalently the service is off.

This will allow you to ask questions such as `is the media service up?` or `are all servers running?`

To add your custom beliefs, do the following:

1. Save your custom beliefs in a file on your openhab , e.g. `/path/to/your/custom/beliefs.txt`
1. Browse to **Paper UI**
1. **Configuration > Services**
1. **Voice**
1. For **SHLURD Voice Interpreter**, click **Configure**
1. Fill in the path to your custom beliefs file
1. **Save**

As an alternative, you can customize this setting via a configuration file; in `/etc/openhab2/services`, create a file `shlurdhli.cfg` with the line `org.openhab.shlurdhli:beliefFile=/path/to/your/custom/beliefs.txt`

The default beliefs are defined in [this resource file](com.lingeringsocket.shlurd.openhab/src/main/resources/beliefs.txt).  You'll probably want to copy those into the beginning of your custom beliefs file.

## Logging

To enable some logging from the plugin, you can add these lines to your [org.ops4j.pax.logging.cfg](https://docs.openhab.org/administration/logging.html#config-file) file:

    log4j2.logger.shlurd.name = com.lingeringsocket.shlurd
    log4j2.logger.shlurd.level = INFO


# Eclipse Build

*(these instructions are for the Oxygen release of Eclipse)*

## Prerequisites

1. [Eclipse set up for OpenHab 2 development](http://docs.openhab.org/developers/development/ide.html)
1. [OSGI bundles for SHLURD staged locally](../osgi/README.md)

## Import project into Eclipse

1. In **Package Explorer**, select **Runtime**
1. Context menu **Import...**
1. Under **General**, select **Existing Projects into Workspace**
1. **Next**
1. Browse to `com.lingeringsocket.shlurd.openhab` subdirectory of your git checkout, e.g. `/home/jvs/open/shlurd/openhab/com.lingeringsocket.shlurd.openhab`
1. **OK**, **Finish**
1. *you will get build errors; this is expected until you add the necessary bundles in the next step*

## Add OSGI bundles for SHLURD to target platform

1. Top menu **Window > Preferences**
1. Under **Plug-in Development**, select **Target Platform**
1. select **openHAB Target Platform (Active)**
1. **Edit...**, **Add...**, **Directory**, **Next**
1. navigate to location where you already staged the OSGI bundles, e.g. `/home/jvs/open/shlurd/osgi/staging`
1. **OK**
1. **Finish**
1. **Finish**
1. **Apply and Close**
1. *(after rebuild, all errors should clear up now)*

## Update run configuration

1. In **Package Explorer**, select **openHAB_Runtime.launch** under **Infrastructure**
1. Context menu **Run As > Run Configurations**
1. Select **Plug-Ins** tab
1. Under **Workspace**, check **com.lingeringsocket.shlurd.openhab**
1. **Add Required Plug-ins**
1. Under **Target Platform**, uncheck **com.google.guava(15.0xxx)** and **javax.xml.bind**
1. **Validate Plug-ins**
1. **Apply**

## Try It Out
1. In **Package Explorer**, select **openHAB_Runtime.launch** under **Infrastructure**
1. Context menu **Run As > openHAB_Runtime**
1. After console shows startup complete, browse to http://localhost:8080
1. Select **Standard** setup
1. Select **Paper UI**
1. **Configuration > System**
1. Scroll down to **Voice** section
1. For **Default Human Language Interpeter**, switch to **SHLURD-based Interpreter**
1. **Save**
1. Back in Eclipse runtime console, type `smarthome voice interpret are any doors open`
1. You should see a response such as `Yes, some doors are open`

## Build Addon JAR

1. As a prerequisite, you must have already run the project as described above in order for the classes to be built.
1. Run the script `osgi/bin/packageAddon` ([source](../osgi/bin/packageAddon))
1. The very large addon jar will be produced in `/tmp`, e.g. `/tmp/shlurd-openhab-551a0df.jar` (the git hash is used for the version identifier)

Now you can test it by copying the exported jar to the `addons` directory of a deployed OpenHAB service.
