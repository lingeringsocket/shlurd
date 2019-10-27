# Language

I can understand the following kinds of sentences:

* [Beliefs about the nature of things](ontology.md)
* [Beliefs about the rules governing the behavior of things](conditionals.md)
* Questions
* Commands
* Statements of fact

But you will discover that I have many limitations in my understanding of language.

// FIXME words/names, sil structures, narratives, conversations

## Example Beliefs

// FIXME supes as plural of supe

```scala mdoc:renderBelief:assets/languageExample.png
A person is a kind of spc-object.
A place is a kind of spc-object.
A supe is a kind of person.
A supe's mindset must be cynical or idealistic.
A supe may have a nemesis.
A supe's identity must be an spc-string.
If a supe is another supe's nemesis, then equivalently the latter is the former's nemesis.
If a supe opposes another supe, then equivalently the former is the latter's nemesis.
Homelander is a supe.
Homelander's identity is "John".
Homelander's mindset is cynical.
Starlight is a supe.
Starlight's identity is "Annie January".
Starlight is idealistic.
Lamplighter is a supe.
Homelander is Starlight's nemesis.
New York City, Vought HQ, and Central Park are places.
Vought HQ and Central Park are in New York City.
Homelander is in Vought HQ and Starlight is in Central Park.
```

## Questions

### Who

When asked about someone's identity, there may be more than one correct answer,
in which case I will pick the one that seems the most informative.

```scala mdoc:processConversation
> Who is Homelander?

Starlight's nemesis.

> Who is Starlight?

Homelander's nemesis.

> Who is Starlight's nemesis?

Homelander.

> Who is Lamplighter?

A supe.

> Who is Lamplighter's nemesis?

No one.

> Whom does Homelander oppose?

Starlight.

> Who is a supe?

Homelander, Starlight, and Lamplighter.

> Who is cynical?

Homelander.

// FIXME
// > Whose mindset is idealistic?
// 
// Starlight.
```

### What

// FIXME other uses for what

```scala mdoc:processConversation
> What is Starlight's identity?

"Annie January".

> What is Lamplighter?

A supe.

> What is Lamplighter's identity?

I don't know.

> What is Starlight's mindset?

Idealistic.
```

### Which

```scala mdoc:processConversation
> Which supe opposes Starlight?

Homelander.

> Which supe has a nemesis?

Homelander and Starlight.

> Which supe exists?

Homelander, Starlight, and Lamplighter.

> Which supe is idealistic?

Starlight.
```

### How Many

```scala mdoc:processConversation
> How many supe are there?

Three of them.

> How many supe have a nemesis?

Two of them.

> How many supe are cynical?

One of them.
```

### Where

```scala mdoc:processConversation
> Where is Starlight?

Central Park.

> Where is Homelander?

Vought HQ.

> Where is Central Park?

New York City.
```

### Predicates

// FIXME should be able to ask about identity too

```scala mdoc:processConversation
> Is Lamplighter a supe?

Yes.


> Is Homelander cynical?

Yes.

> Is Lamplighter's mindset idealistic?

I don't know.

> Does Lamplighter oppose Homelander?

No.

> Is Starlight Homelander's nemesis?

Yes.

> Is Vought HQ in Central Park?

No.
```

## Statements

How I interpret statements depends on whether I am accepting new beliefs.  
In this example, I ignore new beliefs, and when presented with information
which contradicts my current beliefs, I respond skeptically.

```scala mdoc:processConversation:preventNewBeliefs
> Lamplighter is a supe.

Right.

> Starlight is in Central Park.

Right.

> Homelander is in Central Park.

Oh, really?

> Starlight opposes Homelander.

Right.

> Lamplighter is Lamplighter's nemesis.

Oh, really?

> Who is Lamplighter's nemesis?

No one.
```

But my responses are different if I am accepting new or modified beliefs:

```scala mdoc:processConversation:acceptModifiedBeliefs
> Homelander is in Central Park.

OK.

> Who is in Central Park?

Starlight and Homelander.

> Lamplighter is Lamplighter's nemesis.

OK.

> Who does Lamplighter oppose?

Lamplighter.
```

## Commands

// FIXME
