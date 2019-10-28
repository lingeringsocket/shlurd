# Gender and Pronouns

You can teach me about diversity:

```scala mdoc:renderBelief:assets/pronouns.png
A monster is a kind of spc-someone.
A person is a kind of spc-someone.
A person may have crushes.
A man or a woman is a kind of person.
A man is a kind of male.
A woman is a kind of female.
A yeti is a kind of monster.
Masculine, Feminine, Neuter, and Spivak are kinds of gender.
Masculine is Male's spc-gender.
Feminine is Female's spc-gender.
Neuter is Yeti's spc-gender.
"E" is a nominative pronoun.
"Em" is an objective pronoun.
"Eir" is a possessive pronoun.
"Emself" is a reflexive pronoun.
Spivak's spc-pronoun-list is "e, em, eir, emself".
Michael and Theseus are men.
Ariadne and Phaedra are women.
Theseus is Ariadne's crush.
Phaedra is Theseus' crush.
Kirby, Harley and Rey are persons.
Michael's spc-gender is Spivak.
Harley is Rey's crush.
Harley's crush is Kirby.
Bigfoot is a yeti.
Scylla is a monster.
Kirby's spc-pronoun-list is "they, them, their, themself".
"Ze" is a nominative pronoun.
"Zir" must be an objective pronoun or a possessive pronoun.
"Zirself" is a reflexive pronoun.
Harley's spc-pronoun-list is "ze, zir, zirself".
Scylla's spc-pronoun-list is "she, her, herself".
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
