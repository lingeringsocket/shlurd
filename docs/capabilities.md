# Capabilities

When I learn about [cause and effect](conditionals.md), it is in terms
of verbs.  However, not all verbs can be applied in all circumstances.
You can tell me about the capabilities of different subjects.

```scala mdoc:renderBelief:assets/capabilities.png
Lions and zebras are kinds of animal.
Carrots and trees are kinds of plant.
An animal's health must be healthy, sick, or dead.
After an animal begets another animal, the latter is healthy.
After an animal dies, it is dead.
When an animal eats another animal, the latter dies.
There is a lion and a zebra.
There is a carrot and a tree.
The lion and the zebra are healthy.
```

Implicitly, once a verb has been used in a conditional, it is assumed to
be applicable to any statement which matches the antecedent of the conditional:

```scala mdoc:processConversation
> The lion eats the zebra.

OK.

> Is the zebra dead?

Yes.
```

However, it is not applicable for arbitrary statements involving the verb:

```scala mdoc:processConversation
> The zebra eats.

I'm not sure how to interpret that.
```

In the example above, the verb is used intransitively, but the antecedent
in the conditional specifies an object for the verb (to eat).

If you want to allow unrestricted eating, you can tell me so:

```scala mdoc:processConversation
> An animal can eat.

OK.

> The zebra eats.

OK.
```

You can also tell me more precise rules:

```scala mdoc:processConversation
> An animal can eat.

OK.

> A zebra can't eat an animal.

OK.

> The zebra eats.

OK.

> The zebra eats the lion.

A zebra can not eat an animal.

> The lion eats the zebra.

OK.
```

You may also want to restrict a behavior in general, but allow for exceptions:

```scala mdoc:processConversation
// FIXME:  should prevent the contradiction up front; and
// general/specific evaluation is currently wonky

> A lion can't eat a plant.

OK.

> A lion can eat a plant under a tree.

OK.

> The lion eats the tree.

A lion can not eat a plant.

> The lion eats the carrot.

A lion can not eat a plant.

> The lion eats the carrot under the tree.

A lion can not eat a plant.
```

Oops, but the lion should be able to eat the carrot under the tree.  You
can correct this with the "generally" modifier:

```scala mdoc:processConversation
> Generally a lion can't eat a plant.

OK.

> A lion can eat a plant under a tree.

OK.

> The lion eats the tree.

One does not simply eat a plant.

> The lion eats the carrot.

One does not simply eat a plant.

> The lion eats the carrot under the tree.

OK.
```

With the "generally" modifier, the rule remains open for exceptional
cases defined in more specific rules, whereas without it, the rule is
hard and fast.
