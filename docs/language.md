# Language

I can understand the following kinds of sentences:

* [Beliefs about the nature of things](ontology.md)
* [Beliefs about the behavior of things](conditionals.md)
* Questions about the world
* Statements about the world
* Commands

But you will discover that I have many limitations in my understanding of language.

// FIXME sil structures, narratives, conversations

## Example Beliefs

```scala mdoc:renderBelief:assets/languageExample.png
A person, place, or thing is a kind of spc-object.
A person is a kind of spc-someone.
A supe is a kind of person.
A supe's mindset must be cynical or idealistic.
A supe may have a nemesis.
A supe's possession must be a thing.
A supe's identity must be an spc-string.
If a supe is another supe's nemesis, then equivalently the latter is the former's nemesis.
If a supe opposes another supe, then equivalently the former is the latter's nemesis.
If a supe carries a thing, then equivalently the thing is the supe's possession.
Homelander, Starlight, and Lamplighter are supes.
Homelander's identity is "John".
Homelander's mindset is cynical.
Starlight's identity is "Annie January".
Starlight is idealistic.
Homelander is Starlight's nemesis.
New York City, Vought HQ, and Central Park are places.
Vought HQ and Central Park are in New York City.
Homelander is in Vought HQ and Starlight is in Central Park.
A staff is a kind of thing.
There is a staff.
Lamplighter carries the staff.
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

> Whose mindset is idealistic?

Starlight's mindset.
```

### What

```scala mdoc:processConversation
> What is Starlight's identity?

"Annie January".

> What is Lamplighter?

A supe.

> What is Lamplighter's identity?

I don't know.

> What is Starlight's mindset?

Idealistic.

> What does Lamplighter carry?

The staff.
```

### Which

```scala mdoc:processConversation
> Which supe opposes Starlight?

Homelander.

> Which supes have a nemesis?

Homelander and Starlight.

> Which supes exist?

Homelander, Starlight, and Lamplighter.

> Which supes are idealistic?

Starlight.
```

### How Many

I can count!

```scala mdoc:processConversation
> How many supes are there?

Three of them.

> How many supes have a nemesis?

Two of them.

> How many supes are cynical?

One of them.
```

### Where

I can answer simple "where is X" questions (but not "where does", "where is so-and-so going", etc):

```scala mdoc:processConversation
> Where is Starlight?

Central Park.

> Where is Homelander?

Vought HQ.

> Where is Central Park?

New York City.
```

### Predicates

I can also understand yes/no questions:

```scala mdoc:processConversation
> Is Lamplighter a supe?

Yes.

> Is Lamplighter's identity "John"?

I don't know.

> Is "Annie January" Starlight's identity?

Yes.

> Is Homelander cynical?

Yes.

> Is Lamplighter's mindset idealistic?

I don't know.

> Does Lamplighter oppose Homelander?

No.

> Does Lamplighter carry the staff?

Yes.

> Is Starlight Homelander's nemesis?

Yes.

> Is Vought HQ in Central Park?

No.
```

## Statements

How I interpret statements depends on whether I am accepting new
beliefs.  In this example, I ignore new beliefs, and when presented
with information which contradicts my current beliefs, I respond
skeptically.

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

> Whom does Lamplighter oppose?

Lamplighter.
```

## Commands

// FIXME
