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
A dog is a kind of canine.
A wolf is a kind of canine.
A housecat is a kind of feline.
A lion is a kind of feline.
A wolf is a kind of predator.
A lion is a kind of predator.
A dog is a kind of scavenger.
```

## Associations

You can instruct me on how things are related.  And then I can display
them like this:

```scala mdoc:renderBelief:assets/assoc.png
A team's member must be a player.
A seeker is a kind of player.
A keeper is a kind of player.
```

(Notice that I use different arrow styles to distinguish taxonomy from
association.  Also, I use different shapes to distinguish forms from the
roles they play.)

You can tell me which relationships are optional, and which are
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

### Inverses

You can tell me that one relationship is the inverse of another.

```scala mdoc:renderBelief:assets/inverses.png
A student's mentor must be a professor.
A professor's protege must be a student.
A student may have a mentor.
A professor may have proteges.
If a professor is a student's mentor, then equivalently
 the student is the professor's protege.
```

Later, if I learn that one of the relationships holds between a pair
of objects, then I will be able to infer that the inverse relationship
holds as well.
