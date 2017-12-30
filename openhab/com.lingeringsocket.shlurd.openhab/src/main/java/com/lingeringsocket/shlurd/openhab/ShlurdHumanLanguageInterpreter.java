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

import java.io.File;
import java.io.StringReader;

import java.util.Locale;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.ListResourceBundle;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.smarthome.core.events.EventPublisher;
import org.eclipse.smarthome.core.voice.text.AbstractRuleBasedInterpreter;
import org.eclipse.smarthome.core.voice.text.HumanLanguageInterpreter;
import org.eclipse.smarthome.core.voice.text.InterpretationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.eclipse.smarthome.core.types.Command;
import org.eclipse.smarthome.core.types.State;

import org.eclipse.smarthome.core.items.Item;
import org.eclipse.smarthome.core.items.GroupItem;
import org.eclipse.smarthome.core.items.GroupFunction;
import org.eclipse.smarthome.core.items.ItemNotFoundException;
import org.eclipse.smarthome.core.items.ItemRegistry;
import org.eclipse.smarthome.core.items.events.ItemEventFactory;

import org.eclipse.smarthome.core.library.types.StringType;
import org.eclipse.smarthome.core.library.types.OnOffType;

import com.lingeringsocket.shlurd.world.*;
import com.lingeringsocket.shlurd.parser.*;

import scala.io.Source$;
import scala.collection.JavaConverters;

import spire.math.Trilean;
import spire.math.Trilean$;

/**
 * A human language command interpretation service based on SHLURD.
 *
 * @author John Sichi
 */
public class ShlurdHumanLanguageInterpreter extends AbstractRuleBasedInterpreter
{
    private final Logger logger = LoggerFactory.getLogger(ShlurdHumanLanguageInterpreter.class);

    private ItemRegistry itemRegistry;

    private EventPublisher eventPublisher;

    private final Locale supportedLocale = Locale.ENGLISH;

    private ShlurdPlatonicWorld world = null;

    private void createWorld()
    {
        world = new ShlurdPlatonicWorld() {
            @Override
            public scala.util.Try<scala.collection.Set<ShlurdPlatonicEntity>> resolveEntity(
                String lemma,
                ShlurdReferenceContext context,
                scala.collection.Set<String> qualifiers)
            {
                String formName = world.getFormSynonyms().resolveSynonym(lemma);
                if (!world.getForms().contains(formName)) {
                    return new scala.util.Failure(new RuntimeException("I don't know the word " + lemma));
                }
                ShlurdPlatonicForm form = world.getForms().apply(formName);
                ResourceBundle language = new ListResourceBundle()
                    {
                        @Override
                        public Locale getLocale()
                        {
                            return supportedLocale;
                        }

                        protected Object[][] getContents() 
                        {
                            return new Object[][]{};
                        }
                    };
                String [] labelFragments = new String[qualifiers.size()];
                qualifiers.copyToArray(labelFragments);
                ArrayList<Item> items =
                    getMatchingItems(language, labelFragments, null);
                scala.collection.mutable.Set<ShlurdPlatonicEntity> set =
                    new scala.collection.mutable.LinkedHashSet<>();
                items.forEach(
                    item -> {
                        if (!(item instanceof GroupItem) &&
                            item.getName().toLowerCase().contains(formName))
                        {
                            addItemToSet(item, set, form, formName);
                        }
                    });
                return new scala.util.Success(set);
            }

            @Override
            public scala.util.Try<Trilean> evaluateEntityPropertyPredicate(
                ShlurdPlatonicEntity entity,
                ShlurdPlatonicProperty property,
                String lemma)
            {
                try {
                    Item item = itemRegistry.getItem(entity.name());
                    State state;
                    ShlurdPlatonicForm form = entity.form();
                    if (isOnOff(form)) {
                        state = item.getStateAs(OnOffType.class);
                    } else {
                        state = item.getState();
                    }
                    String stateName =
                        form.getStateSynonyms().resolveSynonym(lemma);
                    Trilean trilean = new Trilean(
                        (state == null)
                        ? Trilean$.MODULE$.Unknown()
                        : Trilean$.MODULE$.apply(
                            state.toString().toLowerCase().equals(stateName)));
                    return new scala.util.Success(trilean);
                } catch (Exception ex) {
                    return new scala.util.Failure(ex);
                }
            }
        };
    }

    private void addItemToSet(
        Item item,
        scala.collection.mutable.Set<ShlurdPlatonicEntity> set,
        ShlurdPlatonicForm form,
        String formName)
    {
        scala.collection.mutable.Set<String> qset =
            new scala.collection.mutable.LinkedHashSet<String>();
        item.getGroupNames().forEach(
            group -> {
                try {
                    Item groupItem = itemRegistry.getItem(group);
                    if (isQualifierGroup((GroupItem) groupItem)) {
                        convertLabelTokensToQualifiers(
                            groupItem, qset, "");
                    }
                } catch (ItemNotFoundException ex) {
                    throw new RuntimeException(ex);
                }
            });
        convertLabelTokensToQualifiers(item, qset, formName);
        set.add(new ShlurdPlatonicEntity(
                item.getName(), form,
                qset));
    }
    
    private void convertLabelTokensToQualifiers(
        Item item, scala.collection.mutable.Set<String> qualifiers,
        String formName)
    {
        ArrayList<String> tokens =
            tokenize(supportedLocale, item.getLabel());
        if (!formName.isEmpty()) {
            tokens.remove(formName);
        }
        tokens.forEach(
            token -> {
                qualifiers.add(token);
            });
    }

    private boolean isQualifierGroup(GroupItem groupItem)
    {
        String label = groupItem.getLabel();
        if (label == null) {
            return false;
        }
        if (label.contains("Floor")) {
            return false;
        }
        return (groupItem.getFunction() == null);
    }

    @Override
    public void setItemRegistry(ItemRegistry itemRegistry)
    {
        super.setItemRegistry(itemRegistry);
        this.itemRegistry = itemRegistry;
    }

    @Override
    public void unsetItemRegistry(ItemRegistry itemRegistry)
    {
        super.unsetItemRegistry(itemRegistry);
        if (itemRegistry == this.itemRegistry) {
            this.itemRegistry = null;
        }
    }

    @Override
    public void setEventPublisher(EventPublisher eventPublisher)
    {
        super.setEventPublisher(eventPublisher);
        if (this.eventPublisher == null) {
            this.eventPublisher = eventPublisher;
        }
    }

    @Override
    public void unsetEventPublisher(EventPublisher eventPublisher)
    {
        super.unsetEventPublisher(eventPublisher);
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
                Source$.MODULE$.fromInputStream(
                    getClass().getResourceAsStream("/beliefs.txt"),
                    encoding));
        } else {
            world.loadBeliefs(
                Source$.MODULE$.fromFile(beliefFile, encoding));
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

    @Override
    public String interpret(Locale locale, String text)
        throws InterpretationException
    {
        if (!supportedLocale.getLanguage().equals(locale.getLanguage())) {
            throw new InterpretationException(
                locale.getDisplayLanguage(Locale.ENGLISH) + " is not supported at the moment.");
        }
        if (world == null) {
            createWorld();
        }
        // FIXME:  add convenience method to avoid this atrocity
        // FIXME:  non-string commands?
        // FIXME:  protected vs public?
        ShlurdSentence sentence = ShlurdParser$.MODULE$.apply(text).parseOne();
        ShlurdInterpreter<ShlurdPlatonicEntity, ShlurdPlatonicProperty> interpreter = new ShlurdInterpreter<ShlurdPlatonicEntity, ShlurdPlatonicProperty>(world) {
                @Override
                public void executeInvocation(
                    ShlurdStateChangeInvocation<ShlurdPlatonicEntity> invocation)
                {
                    JavaConverters.setAsJavaSetConverter(invocation.entities()).asJava().forEach(
                        entity -> {
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
                                // FIXME:  log this?
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

    @Override
    protected void createRules()
    {
    }

    private boolean isOnOff(ShlurdPlatonicForm form)
    {
        Set set1 = JavaConverters.setAsJavaSetConverter(form.getProperties().values().head().getStates().keySet()).asJava();
        Set set2 = new java.util.HashSet<>(Arrays.asList("on", "off"));
        return set1.equals(set2);
    }
}
