# Intro

**shlurd-openhab** is an addon for [OpenHAB 2](https://www.openhab.org) which installs a new human language interpreter based on [SHLURD](../README.md).  It allows you to ask questions about the state of your house (e.g. "is the front door open?") as well as give commands (e.g. "turn off all kitchen lights").  At the moment, the only natural language supported is English.

In order for it to work with your smarthome, it's currently required that you follow the item structure (name+label+group) conventions modeled in the [OpenHab demo](https://github.com/openhab/openhab-distro/blob/master/features/distro-resources/src/main/resources/items/demo.items) and defined in [the OpenHab docs](https://github.com/openhab/openhab1-addons/wiki/Controlling-openHAB-with-your-voice).

# Deploy

1. Get the latest release archive from [lingeringsocket.com](https://lingeringsocket.com/com.lingeringsocket.shlurd.openhab).  The zipfile is quite large due to the inclusion of Stanford CoreNLP models, so it may not be suitable for lightweight deployments.
1. Unzip all jars directly into your openhab `addons` directory (e.g. `/usr/share/openhab2/addons`)

If instead you want to keep the SHLURD bundles in a subdirectory of `addons`, so that you can undeploy more easily by just removing that subdirectory, then you need to change the `felix.fileinstall.subdir.mode` property to `recurse` in `/var/lib/openhab2/etc/org.apache.felix.fileinstall-deply.cfg`, or something like that.

# Configure

1. Browse to your OpenHAB service web UI, e.g.  http://localhost:8080
1. Select **Standard** setup
1. Select **Paper UI**
1. **Configuration > System**
1. Scroll down to **Voice** section
1. For **Default Human Language Interpeter**, switch to **SHLURD-based Interpreter**
1. **Save**

As an alternative, you can configure the same setting via a configuration file; edit `runtime.cfg` (maybe in `/etc/openhab2/services`) and change `org.eclipse.smarthome.voice:defaultHLI` to `shlurdhli`

# Try It Out

In order to really speak with OpenHAB, you need to set up audio source/sink and speech recognition/generation services, as described in [the OpenHAB multimedia docs](http://docs.openhab.org/configuration/multimedia.html).

You can also use a text interface via [the OpenHAB console](http://docs.openhab.org/administration/console.html).  Try something like:

    smarthome:voice interpret are any doors open?
    
You should get a response such as `Yes, some doors are open.`

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

The addon comes with a default ontology for a few openhab item types (currently only doors, windows, lights, and switches).  You can extend this with your own types by adding a custom belief file.  For example, suppose you have services running on your home network, e.g. a media service and an alarm service.  Using the [network binding](http://docs.openhab.org/addons/bindings/network/readme.html), you've already defined a Switch item for each service for monitoring purposes.

You can write a custom belief file like this:

    A service must be either on or off.
    A server is a service.
    A service that is up is on.
    A service that is down is off.
    A service that is available is on.
    A service that is unavailable is off.
    A service that is running is on.
    A service that is stopped is off.

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

## Build Plugin JAR

1. In **Package Explorer**, select **com.lingeringsocket.shlurd.openhab** under **Runtime**
1. Context menu **Export > Java > JAR File**
1. **Next**
1. Fill in export destination, e.g. `/home/jvs/open/shlurd/osgi/staging/shlurd-openhab.jar`
1. **Next, Next** (do NOT click **Finish** yet)
1. Select **Use existing manifest from workspace** and browse to `META-INF/MANIFEST.MF` under **com.lingeringsocket.shlurd.openhab**
1. **OK**
1. **Finish**

Now you can test it by copying the exported jar to the `addons` directory of a deployed OpenHAB service.
