# Gender and Pronouns

You can teach me about diversity:

```scala mdoc:renderBelief:assets/pronouns.png
// These beliefs are automatic from WordNet, and are only
// included here for completeness:
A man or a woman is a kind of person.
A man is a kind of male.
A woman is a kind of female.
A yeti is a kind of monster.
Masculine, Feminine, and Neuter are kinds of gender.
// sex+gender have default pairings, but these can be overridden
// for individuals or entire subclasses
Masculine is Male's gender.
Feminine is Female's gender.

// Remaining beliefs below are custom

// Allow monsters to be the answers to "who is..." questions
A monster is a kind of spc-someone.

// Introduce a custom gender, with associated pronouns
Spivak is a kind of gender.
"E" is a nominative pronoun.
"Em" is an objective pronoun.
"Eir" is a possessive pronoun.
"Emself" is a reflexive pronoun.
Spivak's spc-pronoun-list is "e, em, eir, emself".

// Every yeti is neuter by default
Neuter is Yeti's gender.

A person's crush must be a person.
Michael and Theseus are men.
Ariadne and Phaedra are women.
Theseus is Ariadne's crush.
Phaedra is Theseus' crush.
Kirby, Harley and Rey are persons.
Harley is Rey's crush.
Harley's crush is Kirby.
Bigfoot is a yeti.
Scylla is a monster.

// Specify "singular they" pronouns for one individual person
Kirby's spc-pronoun-list is "they, them, their, themself".

// Specify custom pronouns for another individual
"Ze" is a nominative pronoun.
"Zir" must be an objective pronoun or a possessive pronoun.
"Zirself" is a reflexive pronoun.
Harley's spc-pronoun-list is "ze, zir, zirself".

// Even though Scylla is a monster, and hence having neuter gender by
// default, we can still specify whatever pronouns we want,
// overriding the gender default
Scylla's spc-pronoun-list is "she, her, herself".

// Likewise, we can override the gender of one man in particular
Michael's gender is Spivak.
```

// FIXME diagram should show links between genders and forms/entities

I have intrinsic knowledge of the pronouns for masculine, feminine, and neuter genders:

```scala mdoc:processConversation:verbose
> Is Ariadne a woman?

Yes, she is a woman.

> Who is her crush?

Her crush is Theseus.

> Who is his crush?

His crush is Phaedra.

> What is Bigfoot?

It is a yeti.
```

And if you teach me custom pronouns for anyone or anything, I will use them:

```scala mdoc:processConversation:verbose
> Who is Harley?

Ze is Rey's crush.

> Who is zir crush?

Zir crush is Kirby.

> Whose crush is Kirby?

They are Harley's crush.

> What is Scylla?

She is a monster.
```

I can also infer pronouns from gender:

```scala mdoc:processConversation:verbose
> Who is Michael?

E is a man.
```

Note that even though in general a man has been defined as having
masculine gender (implying associated masculine pronouns),
Michael's gender has been explicitly set to Spivak, so I use
the corresponding pronouns to refer to em.

Finally, when someone has no custom pronouns declared, I will avoid pronouns
altogether when referring to that individual:

```scala mdoc:processConversation:verbose
> Who is Rey's crush?

Rey's crush is Harley.
```
