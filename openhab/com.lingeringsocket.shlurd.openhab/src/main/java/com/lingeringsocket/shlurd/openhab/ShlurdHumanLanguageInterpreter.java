/**
// shlurd:  a limited understanding of small worlds
// Copyright 2017-2018 John V. Sichi
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
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

import com.lingeringsocket.shlurd.parser.SprParser$;
import com.lingeringsocket.shlurd.ilang.SilSentence;
import com.lingeringsocket.shlurd.mind.SmcExecutor;
import com.lingeringsocket.shlurd.mind.SmcInterpreterParams;
import com.lingeringsocket.shlurd.mind.SmcInterpreterParams$;
import com.lingeringsocket.shlurd.mind.SmcStateChangeInvocation;
import com.lingeringsocket.shlurd.platonic.SpcOpenhabCosmos;
import com.lingeringsocket.shlurd.platonic.SpcEntity;
import com.lingeringsocket.shlurd.platonic.SpcForm;
import com.lingeringsocket.shlurd.platonic.SpcProperty;
import com.lingeringsocket.shlurd.platonic.SpcInterpreter;
import com.lingeringsocket.shlurd.platonic.SpcMind;

import scala.collection.JavaConverters;
import scala.io.Source$;
import scala.Option;
import scala.util.Try;
import scala.util.Success;
import scala.util.Failure;

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

    private SpcOpenhabCosmos cosmos = null;

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
        cosmos = null;
    }

    private void createCosmos()
    {
        logger.info("SHLURD recreating world...");
        cosmos = new SpcOpenhabCosmos() {
            @Override
            public Try<Option<String>> evaluateEntityProperty(SpcEntity entity, SpcProperty property) {
                try {
                    Item item = itemRegistry.getItem(entity.name());
                    State state;
                    SpcForm form = entity.form();
                    if (isOnOff(form)) {
                        state = item.getStateAs(OnOffType.class);
                    } else {
                        state = item.getState();
                    }
                    if (state == null) {
                        return new Success(Option.empty());
                    } else {
                        return new Success(Option.apply(state.toString().toLowerCase()));
                    }
                } catch (Exception ex) {
                    return new Failure(ex);
                }
            }
        };
        logger.info("SHLURD world recreated");
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
        createCosmos();
        String beliefFile = (String) config.get(BELIEF_FILE_KEY);
        String encoding = "UTF-8";
        if (beliefFile == null) {
            logger.info("SHLURD loading default beliefs...");
            cosmos.loadBeliefs(
                    Source$.MODULE$.fromInputStream(getClass().getResourceAsStream("/beliefs.txt"), encoding));
        } else {
            logger.info("SHLURD loading beliefs from " + beliefFile + "...");
            cosmos.loadBeliefs(Source$.MODULE$.fromFile(beliefFile, encoding));
        }
        logger.info("SHLURD beliefs loaded");
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

    private static class SortableItem implements Comparable<SortableItem>
    {
        String name;
        String label;
        Boolean isGroup;
        List<String> groupNames;

        private String getGroupName()
        {
            if (name.startsWith("g")) {
                return name.substring(1);
            } else {
                return name;
            }
        }

        public int compareTo(SortableItem other)
        {
            if (isGroup && other.isGroup) {
                return getGroupName().compareTo(other.getGroupName());
            } else if (isGroup) {
                return -1;
            } else if (other.isGroup) {
                return 1;
            } else {
                return name.compareTo(other.name);
            }
        }
    }

    private void readItems()
    {
        List<SortableItem> list = new ArrayList<SortableItem>();
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
                SortableItem sortable = new SortableItem();
                sortable.name = item.getName();
                sortable.label = label;
                sortable.isGroup = isGroup;
                sortable.groupNames = item.getGroupNames();
                list.add(sortable);
            }
        }
        Collections.sort(list);
        for (SortableItem sortable : list) {
            cosmos.addItem(sortable.name, sortable.label, sortable.isGroup,
                JavaConverters.iterableAsScalaIterableConverter(sortable.groupNames).asScala());
        }
    }

    @Override
    public String interpret(Locale locale, String text) throws InterpretationException
    {
        if (!supportedLocale.getLanguage().equals(locale.getLanguage())) {
            throw new InterpretationException(
                    locale.getDisplayLanguage(Locale.ENGLISH) + " is not supported at the moment.");
        }
        if (cosmos == null) {
            createCosmos();
            readItems();
        }
        // FIXME: need to support non-string commands
        SilSentence sentence = SprParser$.MODULE$.apply(text).parseOne();
        SmcInterpreterParams params = SmcInterpreterParams$.MODULE$.apply(3);
        SmcExecutor<SpcEntity> executor = new SmcExecutor<SpcEntity> {
            @Override
            public void executeInvocation(SmcStateChangeInvocation<SpcEntity> invocation) {
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

        SpcInterpreter interpreter = new SpcInterpreter(new SpcMind(cosmos), params, executor);
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

    private boolean isOnOff(SpcForm form)
    {
        Set set1 = JavaConverters.setAsJavaSetConverter(form.getProperties().values().head().getStates().keySet())
                .asJava();
        Set set2 = new java.util.HashSet<>(Arrays.asList("on", "off"));
        return set1.equals(set2);
    }
}
