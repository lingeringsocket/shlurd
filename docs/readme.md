# Ontology

I am eager to find out what kinds of things may exist, and how they are
related, and their attributes.

## Taxonomy

You can tell me how things are classified.  Loosely, I follow
[Plato](https://en.wikipedia.org/wiki/Theory_of_forms), so I see the
ideal forms existing in their own plane.  I can display them like
this:

```scala mdoc:renderBelief:assets/taxonomy.png
A pig is a kind of animal.
A mule is a kind of animal.
```

The structure may be a vast web rather than a simple tree:

```scala mdoc:renderBelief:assets/hypernyms.png
A mammal is a kind of animal.
A canine or a feline is a kind of mammal.
Predators and scavengers are kinds of animal.
Dogs and wolves are kinds of canine.
A housecat or a lion is a kind of feline.
A wolf or a lion is a kind of predator.
A dog is a kind of scavenger.
```

As seen above with "a canine or a feline", you can combine your
assertions using conjunctions in cases where it makes sense.  And you
can use equivalent phrasing, so I would understand "canines and
felines are kinds of mammals" as well.  However, I will reject nonsensical
or ambiguous phrasing (e.g. "a canine and a feline is a kind of
animal").  I may also reject phrasing that is too complicated for
me to understand, in which case you can try breaking it down into
multiple simpler thoughts.


## Form Instantiation

You can tell me about particular instances of forms, either named or anonymous:

```scala mdoc:renderBelief:assets/formInstantiation.png
Swords, halberds, and crossbows are kinds of weapon.
Glamdring and Orcrist are swords.
There is a halberd.
A big crossbow exists.
```

Anonymous instances may be qualified (like the big crossbow), but I'm
not yet capable of understanding named objects which are qualified
(e.g. ```Orcrist is an elvish sword```).

Now I can answer questions about what exists:

```scala mdoc:processConversation
> what weapons exist

Glamdring, Orcrist, a halberd, and a big crossbow.

> how many swords are there

Two of them.

> is Orcrist a crossbow

No.

> what is Glamdring

A sword.
```


## Associations

You can instruct me on how things may be associated.  And then I can display
those associations like this:

```scala mdoc:renderBelief:assets/assoc.png
A team's member must be a player.
Seekers and keepers are kinds of player.
```

(Notice that I use different arrow styles to distinguish taxonomy from
association.  Also, I use different shapes to distinguish forms from the
roles they play.)

You can tell me which associations are optional, and which are
mandatory.  You can also constrain their multiplicity.

```scala mdoc:renderBelief:assets/constraints.png
A level's enemy must be a creature.
A level's boss must be a creature.
A level's guide must be a creature.
A level's reward must be an item.
A level must have enemies.
A level must have a boss.
A level may have rewards.
A level may have a guide.
```

### Association Instantiation

You can connect particular instances of forms via their associations:

```scala mdoc:renderBelief:assets/assocInstantiation.png
A nation's capital must be a city.
A nation's province must be a region.
A nation may have provinces.
A nation must have a capital.
Canada and Greece are nations.
Ottawa and Athens are cities.
Quebec and Saskatchewan are regions.
Ottawa is Canada's capital.
Athens is Greece's capital.
Quebec is Canada's province.
Saskatchewan is Canada's province.
```

Now I can answer questions about how things are connected:

```scala mdoc:processConversation
> what is Canada's capital

Ottawa.

> is Ottawa Greece's capital

No.

> which regions are Canada's provinces

Quebec and Saskatchewan.

> what is Athens

Greece's capital.

> what is Greece

A nation.
```


### Inverses

You can tell me that one association is the inverse of another:

```scala mdoc:renderBelief:assets/inverses.png
A student's mentor must be a professor.
A professor's protege must be a student.
A student may have a mentor.
A professor may have proteges.
If a professor is a student's mentor, then equivalently
 the student is the professor's protege.
Aristotle is a professor.
Alexander is a student.
Aristotle is Alexander's mentor.
```

After learning that Aristotle is Alexander's mentor, I am able to
infer automatically that Alexander is Aristotle's protege.

### Refinements

You can tell me how associations may be refined.  For example,
motherhood is a refinement of being a parent.  Here's a complete example:

```scala mdoc:renderBelief:assets/refinement.png
A celebrity is a kind of performer.
A movie's actor must be a performer.
A movie's star is a kind of actor.
A movie's star must be a celebrity.
A movie's extra is a kind of actor.
Deadpool is a movie.
Ryan Reynolds is a celebrity.
Chad Riley is a performer.
Ryan Reynolds is Deadpool's star.
Chad Riley is Deadpool's extra.
```

Now I know how to group together actors generically, or discriminate
them as stars or extras:

```scala mdoc:processConversation
> who are Deadpool's actors

Ryan Reynolds and Chad Riley.

> who are Deadpool's stars

Ryan Reynolds.

> who are Deadpool's extras

Chad Riley.

```

## Scoping

I can distinguish the same role name used in different association contexts:

```scala mdoc:renderBelief:assets/scoping.png
A sailor is a kind of person.
A ship's mate must be a sailor.
A person's mate must be a person.
If a person's mate is another person, then equivalently
 the first person is the second person's mate.
Gilligan is a sailor.
Minnow is a ship.
Gilligan is Minnow's mate.
Adam and Eve are persons.
Adam is Eve's mate.
```

## Synonyms

You can tell me alternate names for things:

```scala mdoc:renderBelief:assets/synonyms.png
Dogs and cats are kinds of animal.
A person's pet must be an animal.
A person's companion is the same as a pet.
A doggie or a doggy is the same as a dog.
A kitty is the same as a cat.
Fido is a doggie.
Felix is a cat.
Oscar is a person.
Fido is Oscar's pet.
```

Then you can use them interchangeably:

```scala mdoc:processConversation
> Is Fido a dog?

Yes.

> Which kitties exist?

Felix.

> What is Oscar's companion?

Fido.

```

## Properties

You can tell me about the properties of objects:

```scala mdoc:renderBelief:assets/properties.png
A beverage's size may be small, medium, or large.
A customer's order must be a beverage.
A customer's catchphrase must be an spc-string.

A cappuccino or a latte is a kind of beverage.
A cappuccino's size must be small or large.
A latte's size may be huge.

There is a cappuccino and a latte.

Luigi and Mario are customers.
Luigi's catchphrase is "Letsa go!"
Mario's catchphrase is "Itsa me, Mario!"

The cappuccino is Luigi's order.
The cappuccino is large.

The latte is Mario's order.
The latte is huge.
```

Then I can answer questions about them:

```scala mdoc:processConversation
// FIXME:  allow for "Which size is Luigi's order"
> What is Luigi's order's size?

Large.

> Is the cappuccino small?

No.

> The cappuccino is medium.

The belief that the cappuccino is medium contradicts the belief that a cappuccino's size must be small or large.

> What is the latte's size?

Huge.

> What is Mario's catchphrase?

"Itsa me, Mario!".

```

## Names

When you try to use normal words as names, I may get confused:

```scala mdoc:processConversation

> Grumpy is a dwarf.

Sorry, I cannot understand what you said.

```

You can help me understand what you're trying to do:

```scala mdoc:processConversation

> "Grumpy" may be a proper noun.

OK.

> Grumpy is a dwarf.

OK.

```

You may also need to do this in cases where you use a name that I haven't
heard of before.

If you tell me that something **may** be a proper noun, then I will preserve
its original meaning and try to disambiguate based on context:


```scala mdoc:processConversation:allowImplicits

> "Grumpy" may be a proper noun.

OK.

> A dwarf is a kind of person.

OK.

> A person's enemy must be a person.

OK.

> There is a queen.

OK.

> A person's mood may be grumpy or happy.

OK.

> Grumpy is a dwarf.

OK.

> The queen's mood is grumpy.

OK.

> What is the queen's mood?

Grumpy.

> Is the queen a dwarf?

No.

> The queen's enemy is Grumpy.

OK.

> Is the queen's enemy a dwarf?

Yes.

> There is a king.

OK.

> The king's enemy is grumpy.

OK.

> Is the king's enemy a dwarf?

No.

```

As you can see above, this may get a bit confusing.  If you'd prefer to completely
take over the word, just tell me that it **must** be a proper noun, and I
will forget about its existing senses:

```scala mdoc:processConversation:allowImplicits

> "Grumpy" must be a proper noun.

OK.

> Grumpy is a dwarf.

OK.

> A person may be grumpy or happy.

Sorry, I cannot understand what you said.

```

You can also tell me about multi-word names, and then I'll do my best to
understand references to them:

```scala mdoc:processConversation

> Clever Hans is a horse.

I am unable to understand the belief that clever Hans is a horse.

> "Clever Hans" is a proper noun.

OK.

> Clever Hans is a horse.

OK.

> Is Hans a horse?

Yes.

```
