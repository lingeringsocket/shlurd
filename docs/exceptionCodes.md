# Exceptions

```scala mdoc:renderBelief:assets/exceptions.png
An investigator is a kind of person.
An investigator's mindset may be confident or confused.
Jupiter is an investigator.
Pete is an investigator.
A tribble's baby must be a tribble.
After a tribble eats,
 the tribble has a baby;
 also the tribble's baby eats.
Terence is a tribble.
```

## FailedParse

```scala mdoc:processConversation
> orange the soccer field

Sorry, I cannot understand what you said.
```

## UnknownForm

```scala mdoc:processConversation
> is there a jubjub

Sorry, I don't know about any 'jubjub'.
```

## UnknownState

```scala mdoc:processConversation
> is Jupiter arrogant?

Sorry, I don't know what 'arrogant' means for Jupiter.
```

## NonExistent

```scala mdoc:processConversation
> is the mysterious investigator confused

But I don't know about any such investigator.
```

## NotUnique

```scala mdoc:processConversation
> is the investigator confused

Please be more specific about which investigator you mean.
```

## UnresolvedPronoun

```scala mdoc:processConversation
> who is he

Sorry, when you say 'he' I don't know who or what you mean.
```

## TriggerLimit

```scala mdoc:processConversation
> Terence eats.

Trigger limit exceeded.
```

## IncomprehensibleBelief

FIXME

## InvalidBelief

```scala mdoc:processConversation
> if an investigator fires a gun, then the gun is broken

I am unable to validate the belief that if an investigator fires a gun, then the gun is broken.
```

## BeliefNotYetImplemented

```scala mdoc:processConversation
> a green door must be either open or closed

I am not yet capable of processing the belief that a green door must be either open or closed.
```

## ReferenceNotYetImplemented

```scala mdoc:processConversation
> Pete is a house's owner

I am unable to understand the belief that Pete is a house's owner.
```

## NewBeliefsProhibited

```scala mdoc:processConversation
> A tribble is a kind of reptile

The belief that a tribble is a kind of reptile is prohibited in the given context.
```

## ImplicitIdealsProhibited

```scala mdoc:processConversation:preventImplicits
> an ewok is a kind of animal

The belief that an ewok is a kind of animal is prohibited in the given context.
```

## ImplicitPropertiesProhibited

```scala mdoc:processConversation:preventImplicits
> the tribble is hungry

The belief that the tribble is hungry is prohibited in the given context.
```

## TentativeIdealsProhibited

```scala mdoc:processConversation:preventImplicits
> Bob exists

The belief that Bob exists is prohibited in the given context.
```

## TentativeEntitiesProhibited

```scala mdoc:processConversation:preventImplicits
> Jupiter has an uncle

The belief that Jupiter has an uncle is prohibited in the given context.
```

## AmbiguousInterpretation

```scala mdoc:processConversation
> there is a big door

OK.

> there is a door

Previously I was told that a big door exists.  So there is an ambiguous reference in the belief that there is a door.
```

## CardinalityConstraint

```scala mdoc:processConversation:allowImplicits
> A bloodhound's owner must be an investigator

OK.

> A bloodhound must have an owner

OK.

> McGruff is a bloodhound

OK.

> Pete is McGruff's owner

OK.

> Jupiter is McGruff's owner

Previously I was told that a bloodhound must have one owner and Pete is McGruff's owner.  So it does not add up when I hear that Jupiter is McGruff's owner.
```

## TaxonomyCycle

```scala mdoc:processConversation
> a hamster is a kind of tribble

OK.

> a tribble is a kind of hamster

The belief that a tribble is a kind of hamster contradicts the belief that a hamster is a kind of a tribble.
```

## OverlappingProperties

```scala mdoc:processConversation
> a tribble may be round or square

OK.

> a tribble may be red or blue

OK.

> a tribble may be round or red

I am not yet capable of processing the belief that a tribble may be round or red.
```

## PropertyAlreadyClosed

```scala mdoc:processConversation:allowImplicits
> a portal must be open or closed

OK.

> a door is a kind of portal

OK.

> a door may be open or ajar

The belief that a door may be open or ajar contradicts the belief that a door must be open or closed.
```

## RoleHypernymNonExistent

```scala mdoc:processConversation
> a tribble's owner is a kind of guardian

I am unable to understand the belief that a tribble's owner is a kind of guardian.
```

## RoleHyponymConflictsWithForm

```scala mdoc:processConversation
> a tribble's guardian must be a person

OK.

> an owner is a kind of person

OK.

> a tribble's owner is a kind of guardian

I am unable to understand the belief that a tribble's owner is a kind of guardian.
```

## RoleHyponymAlreadyExists

```scala mdoc:processConversation:allowImplicits
> a tribble may have a guardian

OK.

> a tribble may have a petsitter

OK.

> a tribble's owner is a kind of guardian

OK.

> a tribble's owner is a kind of petsitter

I am unable to understand the belief that a tribble's owner is a kind of petsitter.
```

## RoleTaxonomyIncompatible

```scala mdoc:processConversation:allowImplicits
> a groomer is a kind of caretaker

OK.

> Kirk is a groomer

OK.

> a tribble's custodian must be a caretaker

OK.

> Kirk is Terence's custodian

OK.

> a tribble's custodian must be a chef

The belief that a tribble's custodian must be a chef contradicts the belief that Kirk is a groomer and Kirk is Terence's custodian.
```

## FormTaxonomyIncompatible

```scala mdoc:processConversation
> Pete is a monster

The belief that Pete is a monster contradicts the belief that Pete is an investigator.
```

## FormRoleIncompatible

```scala mdoc:processConversation:allowImplicits
> a tribble must have a groomer

OK.

> a tribble's groomer must be a human

OK.

> Spock is a Vulcan

OK.

> Spock is Terence's groomer

The belief that Spock is Terence's groomer contradicts the belief that a tribble's groomer must be a human.
```

## PropertyDomainIncompatible

```scala mdoc:processConversation:allowImplicits
> a door's state may be either open or closed

OK.

> a door's state must be an spc-string

The belief that a door's state must be an spc-string contradicts the belief that a door's state may be open or closed.
```

## AbsenceConstraint

```scala mdoc:processConversation:allowImplicits
> McGruff is Jupiter's pet

OK.

> Jupiter has no pets

The belief that Jupiter has no pets contradicts the belief that McGruff is Jupiter's pet.
```

## AssertionModifiersIncompatible

* BEFORE+SUBSEQUENTLY
* CONSEQUENTLY+SUBSEQUENTLY
* OTHERWISE+ALSO
* OTHERWISE+EQUIVALENTLY
* ALSO+EQUIVALENTLY
* ALSO on "x can do y"

```scala mdoc:processConversation
> If a tribble eats, equivalently the tribble is subsequently fat.

I am unable to validate the belief that if a tribble eats, equivalently the tribble is fat subsequently.
```

## AssertionModifierSequence

* OTHERWISE/ALSO prohibited on main consequent
* OTHERWISE must be last
* only one OTHERWISE allowed
* OTHERWISE/ALSO required on subsequent consequents

```scala mdoc:processConversation
> After a tribble eats, also the tribble becomes fat.

I am unable to validate the belief that after a tribble eats, then the tribble becomes fat also.
```

## QuantifierNotYetImplemented

```scala mdoc:processConversation
> After a tribble eats, some tribble becomes fat.

I am unable to validate the belief that after a tribble eats, then some tribble becomes fat.
```

## PostConstraintNotYetImplemented

```scala mdoc:processConversation
> After a tribble eats, the tribble must be alone.

I am unable to validate the belief that after a tribble eats, then the tribble must be alone.
```

## AntecedentEventExpected

* not equivalently/consequently

```scala mdoc:processConversation
> After a tribble is happy, the tribble eats.

I am unable to validate the belief that after a tribble is happy, then the tribble eats.
```

## ConsequentConditionExpected

equivalence plus

* become
* action verb
* subsequently or consequently

```scala mdoc:processConversation
> If a tribble eats, equivalently the tribble becomes happy.

I am unable to validate the belief that if a tribble eats, equivalently the tribble becomes happy.
```

## ConsequentEventExpected

antecedent event plus
* stative without subsequently/consequently/equivalently

```scala mdoc:processConversation
> When a tribble eats, the tribble is happy.

I am unable to validate the belief that when a tribble eats, then the tribble is happy.
```

## ConsequentConstraintExpected

* BEFORE/OTHERWISE

```scala mdoc:processConversation
> Before a tribble eats, the tribble is happy.

I am unable to validate the belief that before a tribble eats, then the tribble is happy.
```

## AssertionInvalidAssociation

```scala mdoc:processConversation
> If a tribble is another tribble's baby, then the tribble's parent is the tribble.

I am unable to validate the belief that if a tribble is another tribble's baby, then the tribble's parent is the tribble.
```

## EquivalenceIfExpected

```scala mdoc:processConversation
> After a tribble eats, equivalently the tribble is happy.

I am unable to validate the belief that after a tribble eats, equivalently the tribble is happy.
```

## AssertionModalProhibited

* EQUIVALENTLY
* ALSO

```scala mdoc:processConversation
> If a tribble eats, equivalently the tribble must be happy.

I am unable to validate the belief that if a tribble eats, equivalently the tribble must be happy.
```
