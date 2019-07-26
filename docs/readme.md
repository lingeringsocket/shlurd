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
A canine is a kind of mammal.
A feline is a kind of mammal.
A predator is a kind of animal.
A scavenger is a kind of animal.
A dog is a kind of canine.
A wolf is a kind of canine.
A housecat is a kind of feline.
A lion is a kind of feline.
A wolf is a kind of predator.
A lion is a kind of predator.
A dog is a kind of scavenger.
```

## Form Instantiation

You can tell me about particular instances of forms, either named or anonymous:

```scala mdoc:renderBelief:assets/formInstantiation.png
A sword is a kind of weapon.
A halberd is a kind of weapon.
A crossbow is a kind of weapon.
Glamdring is a sword.
Orcrist is a sword.
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
A seeker is a kind of player.
A keeper is a kind of player.
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

## Association Instantiation

You can connect particular instances of forms via their associations:

```scala mdoc:renderBelief:assets/assocInstantiation.png
A nation's capital must be a city.
A nation's province must be a region.
A nation may have provinces.
A nation must have a capital.
Canada is a nation.
Greece is a nation.
Ottawa is a city.
Athens is a city.
Quebec is a region.
Saskatchewan is a region.
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


## Inverses

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

## Refinements

You can tell me how associations may be refined.  For example,
motherhood is a refinement of being a parent.  Here's a complete example:

```scala mdoc:renderBelief:assets/refinement.png
A celebrity is a kind of performer.
A movie's actor must be a performer.
// FIXME order of next two should not matter
A movie's star is a kind of actor.
A movie's star must be a celebrity.
// FIXME we should preserve refined role on edge association
A movie's extra is a kind of actor.
Thor is a movie.
Hemsworth is a celebrity.
Pingle is a performer.
Hemsworth is Thor's star.
Pingle is Thor's extra.
```

Now I know how to group together actors generically, or discriminate
them as stars or extras:

```scala mdoc:processConversation
> who are Thor's actors

Hemsworth and Pingle.

> who are Thor's stars

Hemsworth.

// FIXME:  wrong!

> who are Thor's extras

Hemsworth and Pingle.

```

## Synonyms

You can tell me alternate names for things:

```scala mdoc:renderBelief:assets/synonyms.png
A dog is a kind of animal.
A cat is a kind of animal.
A person's pet must be an animal.
A person's companion is a pet.
A doggie is a dog.
A kitty is a cat.
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

A cappuccino is a kind of beverage.
A cappuccino's size must be small or large.

A latte is a kind of beverage.
A latte's size may be huge.

Luigi is a customer.
There is a cappuccino.
The cappuccino is Luigi's order.
The cappuccino is large.

Mario is a customer.
There is a latte.
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

```
