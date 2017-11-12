# Eclipse Build

*(these instructions are for the Oxygen release of Eclipse)*

## Prerequisites

1. [Eclipse set up for OpenHab 2 development](http://docs.openhab.org/developers/development/ide.html)

## Import project into Eclipse

1. In **Package Explorer**, select **Runtime**
1. Context menu **Import...**
1. Under **General**, select **Existing Projects into Workspace**
1. **Next**
1. Browse to root directory, e.g. `/home/jvs/open/shlurd/openhab/com.lingeringsocket.shlurd.openhab`
1. **OK**, **Finish**
1. *you will get build errors; this is expected until you add the necessary bundles in the next step*

## Add SHLURD bundles to target platform

1. Top menu **Window > Preferences**
1. Under **Plug-in Development**, select **Target Platform**
1. select **openHAB Target Platform (Active)**
1. **Edit...**, **Add...**, **Directory**, **Next**
1. navigate to location where you already built the OSGI bundles, e.g. `/home/jvs/open/shlurd/osgi/staging`
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
