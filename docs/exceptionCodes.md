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

<!---
TODO:  add examples for CausalityViolation and NotYetImplemented
-->
