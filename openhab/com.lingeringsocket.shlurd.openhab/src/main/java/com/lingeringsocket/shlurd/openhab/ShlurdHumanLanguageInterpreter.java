/**
// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
 */
package com.lingeringsocket.shlurd.openhab;

import java.util.Arrays;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.eclipse.smarthome.core.common.registry.RegistryChangeListener;
import org.eclipse.smarthome.core.events.EventPublisher;
import org.eclipse.smarthome.core.items.GroupItem;
import org.eclipse.smarthome.core.items.Item;
import org.eclipse.smarthome.core.items.ItemNotFoundException;
import org.eclipse.smarthome.core.items.ItemRegistry;
import org.eclipse.smarthome.core.items.events.ItemEventFactory;
import org.eclipse.smarthome.core.library.types.OnOffType;
import org.eclipse.smarthome.core.library.types.StringType;
import org.eclipse.smarthome.core.types.Command;
import org.eclipse.smarthome.core.types.State;
import org.eclipse.smarthome.core.voice.text.HumanLanguageInterpreter;
import org.eclipse.smarthome.core.voice.text.InterpretationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.lingeringsocket.shlurd.parser.ShlurdParser$;
import com.lingeringsocket.shlurd.parser.ShlurdSentence;
import com.lingeringsocket.shlurd.world.ShlurdInterpreter;
import com.lingeringsocket.shlurd.world.ShlurdInterpreterParams;
import com.lingeringsocket.shlurd.world.ShlurdInterpreterParams$;
import com.lingeringsocket.shlurd.world.ShlurdOpenhabWorld;
import com.lingeringsocket.shlurd.world.ShlurdPlatonicEntity;
import com.lingeringsocket.shlurd.world.ShlurdPlatonicForm;
import com.lingeringsocket.shlurd.world.ShlurdPlatonicProperty;
import com.lingeringsocket.shlurd.world.ShlurdStateChangeInvocation;

import scala.collection.JavaConverters;
import scala.io.Source$;
import spire.math.Trilean;
import spire.math.Trilean$;

/**
 * A human language command interpretation service based on SHLURD.
 *
 * @author John Sichi
 */
public class ShlurdHumanLanguageInterpreter
    implements HumanLanguageInterpreter
{
    private final Logger logger = LoggerFactory.getLogger(ShlurdHumanLanguageInterpreter.class);

    private ItemRegistry itemRegistry;

    private EventPublisher eventPublisher;

    private final Locale supportedLocale = Locale.ENGLISH;

    private ShlurdOpenhabWorld world = null;

    private RegistryChangeListener<Item> registryChangeListener =
        new RegistryChangeListener<Item>()
        {
            @Override
            public void added(Item element) {
                invalidate();
            }

            @Override
            public void removed(Item element) {
                invalidate();
            }

            @Override
            public void updated(Item oldElement, Item element) {
                invalidate();
            }
        };

    private void invalidate()
    {
        if (world != null) {
            world.clear();
        }
    }

    private void createWorld()
    {
        logger.error("Recreating SHLURD world");
        world = new ShlurdOpenhabWorld() {
            @Override
            public scala.util.Try<Trilean> evaluateState(ShlurdPlatonicEntity entity, String stateName) {
                try {
                    Item item = itemRegistry.getItem(entity.name());
                    State state;
                    ShlurdPlatonicForm form = entity.form();
                    if (isOnOff(form)) {
                        state = item.getStateAs(OnOffType.class);
                    } else {
                        state = item.getState();
                    }
                    Trilean trilean = new Trilean((state == null) ? Trilean$.MODULE$.Unknown()
                            : Trilean$.MODULE$.apply(state.toString().toLowerCase().equals(stateName)));
                    return new scala.util.Success(trilean);
                } catch (Exception ex) {
                    return new scala.util.Failure(ex);
                }
            }
        };
    }

    public void setItemRegistry(ItemRegistry itemRegistry)
    {
        if (this.itemRegistry == null) {
            this.itemRegistry = itemRegistry;
            this.itemRegistry.addRegistryChangeListener(registryChangeListener);
        }

    }

    public void unsetItemRegistry(ItemRegistry itemRegistry)
    {
        if (itemRegistry == this.itemRegistry) {
            this.itemRegistry.removeRegistryChangeListener(registryChangeListener);
            this.itemRegistry = null;
        }
    }

    public void setEventPublisher(EventPublisher eventPublisher)
    {
        if (this.eventPublisher == null) {
            this.eventPublisher = eventPublisher;
        }
    }

    public void unsetEventPublisher(EventPublisher eventPublisher)
    {
        if (eventPublisher == this.eventPublisher) {
            this.eventPublisher = null;
        }
    }

    // TODO conventions
    private static final String BELIEF_FILE_KEY = "beliefFile";

    protected void activate(Map<String, Object> config)
    {
        modified(config);
    }

    protected void modified(Map<String, Object> config)
    {
        createWorld();
        String beliefFile = (String) config.get(BELIEF_FILE_KEY);
        String encoding = "UTF-8";
        if (beliefFile == null) {
            world.loadBeliefs(
                    Source$.MODULE$.fromInputStream(getClass().getResourceAsStream("/beliefs.txt"), encoding));
        } else {
            world.loadBeliefs(Source$.MODULE$.fromFile(beliefFile, encoding));
        }
    }

    @Override
    public String getId()
    {
        return "shlurdhli";
    }

    @Override
    public String getLabel(Locale locale)
    {
        return "SHLURD-based Interpreter";
    }

    private void readItems()
    {
        for (Item item : itemRegistry.getAll()) {
            String label = item.getLabel();
            boolean isGroup = false;
            if (item instanceof GroupItem) {
                GroupItem groupItem = (GroupItem) item;
                isGroup = true;
                if (groupItem.getFunction() != null) {
                    label = null;
                }
            }
            if (label != null) {
                world.addItem(item.getName(), label, isGroup, JavaConverters.iterableAsScalaIterableConverter(item.getGroupNames()).asScala().toSet());
            }
        }
    }

    @Override
    public String interpret(Locale locale, String text) throws InterpretationException
    {
        if (!supportedLocale.getLanguage().equals(locale.getLanguage())) {
            throw new InterpretationException(
                    locale.getDisplayLanguage(Locale.ENGLISH) + " is not supported at the moment.");
        }
        if (world == null) {
            createWorld();
        }
        if (world.getEntities().isEmpty()) {
            readItems();
        }
        // FIXME: need to support non-string commands
        ShlurdSentence sentence = ShlurdParser$.MODULE$.apply(text).parseOne();
        ShlurdInterpreterParams params = ShlurdInterpreterParams$.MODULE$.apply(3);
        ShlurdInterpreter<ShlurdPlatonicEntity, ShlurdPlatonicProperty> interpreter = new ShlurdInterpreter<ShlurdPlatonicEntity, ShlurdPlatonicProperty>(
                world, params) {
            @Override
            public void executeInvocation(ShlurdStateChangeInvocation<ShlurdPlatonicEntity> invocation) {
                JavaConverters.setAsJavaSetConverter(invocation.entities()).asJava().forEach(entity -> {
                    try {
                        Item item = itemRegistry.getItem(entity.name());
                        String upper = invocation.state().inflected().toUpperCase();
                        Command command;
                        if (isOnOff(entity.form())) {
                            command = OnOffType.valueOf(upper);
                        } else {
                            command = new StringType(upper);
                        }
                        eventPublisher.post(ItemEventFactory.createCommandEvent(item.getName(), command));
                    } catch (ItemNotFoundException ex) {
                        // FIXME: log this?
                    }
                });
            }
        };
        String result = interpreter.interpret(sentence);
        return result;
    }

    @Override
    public String getGrammar(Locale locale, String format)
    {
        return null;
    }

    @Override
    public Set<Locale> getSupportedLocales()
    {
        return Collections.singleton(supportedLocale);
    }

    @Override
    public Set<String> getSupportedGrammarFormats()
    {
        return Collections.emptySet();
    }

    private boolean isOnOff(ShlurdPlatonicForm form)
    {
        Set set1 = JavaConverters.setAsJavaSetConverter(form.getProperties().values().head().getStates().keySet())
                .asJava();
        Set set2 = new java.util.HashSet<>(Arrays.asList("on", "off"));
        return set1.equals(set2);
    }
}
