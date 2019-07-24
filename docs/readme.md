# Ontology

You can teach me what kinds of things may exist, and how they are
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
A nation's province must be a territory.
A nation may have provinces.
A nation must have a capital.
Canada is a nation.
Greece is a nation.
Ottawa is a city.
Athens is a city.
Quebec is a territory.
Saskatchewan is a territory.
Ottawa is Canada's capital.
Athens is Greece's capital.
Quebec is Canada's province.
Saskatchewan is Canada's province.
```

## Inverses

You can tell me that one association is the inverse of another.

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

After learning that Aristotle is Alexander's mentor, I was able to
automatically infer that Alexander is Aristotle's protege.
