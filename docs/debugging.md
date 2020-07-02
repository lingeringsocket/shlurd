# Phlebotinum Debugging

To see what's going on under the covers, use the ```debug``` verb:

```
> debug

OK.

> s
DIRECTIVE the game-turn advances
TRIGGER all game-characters begin the game-turn
TRIGGER the player-character has no visible-items
TRIGGER the player-character surveys the player-character's container
TRIGGER the player-character perceives the player-character and the player-character's stuff
TRIGGER the player-character becomes the player-character's container's place-discoverer
TRIGGER the player-character perceives the meadow
TRIGGER the player-character observes the meadow's contained-objects except the player-character
TRIGGER the player-character surveys the meadow's place-exits and the meadow's place-entrances
TRIGGER the player-character perceives the tree and the rock
TRIGGER the player-character sees the tree and the rock
TRIGGER the tree and the rock are the player-character's visible-items
TRIGGER the player-character observes E_wnf-hayfield-1_835969_place-exit__835997 and E_wnf-hayfield-1_835969_place-exit__836041 and E_wnf-tree-1_835957_place-exit__836006 and E_wnf-barn-1_835973_place-exit__836050 E_wnf-hayfield-1_835969_place-exit__835997's and E_wnf-hayfield-1_835969_place-exit__836041's and E_wnf-tree-1_835957_place-exit__836006's and E_wnf-barn-1_835973_place-exit__836050's exit-portals, and E_wnf-hayfield-1_835969_place-exit__835997's and E_wnf-hayfield-1_835969_place-exit__836041's and E_wnf-tree-1_835957_place-exit__836006's and E_wnf-barn-1_835973_place-exit__836050's containers
TRIGGER the player-character perceives E_wnf-hayfield-1_835969_place-exit__836041 and E_wnf-tree-1_835957_place-exit__836006 and the zenith and the nadir and E_wnf-barn-1_835973_place-exit__836050 and the south and the grassy path and the north and E_wnf-hayfield-1_835969_place-exit__835997
TRIGGER the player-character sees E_wnf-hayfield-1_835969_place-exit__836041 and E_wnf-tree-1_835957_place-exit__836006 and the zenith and the nadir and E_wnf-barn-1_835973_place-exit__836050 and the south and the grassy path and the north and E_wnf-hayfield-1_835969_place-exit__835997
TRIGGER the grassy path is the player-character's visible-items
TRIGGER the player-character surveys the sky
TRIGGER the player-character perceives the sky
TRIGGER the player-character observes the sky's contained-objects except the player-character
TRIGGER the player-character surveys the sky's place-exits and the sky's place-entrances
TRIGGER the player-character perceives the cloud
TRIGGER the player-character sees the cloud
TRIGGER the cloud is the player-character's visible-items
TRIGGER the child has no visible-items
TRIGGER the child perceives the child, all haylofts, all sprites, and the player-character's heart
TRIGGER the child surveys the child's container
TRIGGER the child perceives the barn
TRIGGER the child observes the barn's contained-objects except the child
TRIGGER the child surveys the barn's place-exits and the barn's place-entrances
TRIGGER the child observes E_wnf-barn-1_835973_place-exit__836050 and E_wnf-barn-1_835973_place-exit__836083 and E_wnf-hayfield-1_835969_place-exit__836041 and E_wnf-hayloft-1_835977_place-exit__836092 E_wnf-barn-1_835973_place-exit__836050's and E_wnf-barn-1_835973_place-exit__836083's and E_wnf-hayfield-1_835969_place-exit__836041's and E_wnf-hayloft-1_835977_place-exit__836092's exit-portals, and E_wnf-barn-1_835973_place-exit__836050's and E_wnf-barn-1_835973_place-exit__836083's and E_wnf-hayfield-1_835969_place-exit__836041's and E_wnf-hayloft-1_835977_place-exit__836092's containers
TRIGGER the child perceives E_wnf-hayloft-1_835977_place-exit__836092 and E_wnf-hayfield-1_835969_place-exit__836041 and the zenith and E_wnf-barn-1_835973_place-exit__836083 and the nadir and E_wnf-barn-1_835973_place-exit__836050 and the south and the grassy path and the north
TRIGGER the child sees E_wnf-hayloft-1_835977_place-exit__836092 and E_wnf-hayfield-1_835969_place-exit__836041 and the zenith and E_wnf-barn-1_835973_place-exit__836083 and the nadir and E_wnf-barn-1_835973_place-exit__836050 and the south and the grassy path and the north
TRIGGER the grassy path is the child's visible-items
TRIGGER the player-character perceives the player-character's stuff's contained-objects
TRIGGER the player-character perceives the player-character's heart
TRIGGER the player-character perceives the player-character's memory
TRIGGER the player-character perceives the player-character's wife
COMMAND s
EXPANDED go south
PARSED
SilPredicateSentence(
  SilActionPredicate(
    SilPronounReference(
      PERSON_SECOND(),
      GENDER_SOMEONE(),
      COUNT_SINGULAR(),
      DISTANCE_UNSPECIFIED()):151,
    SilSimpleWord("go", "go", ""),
    Some(SilNounReference(SilSimpleWord("south", "south", "")):153),
    Nil),
  SilTamImmutable(
    MOOD_IMPERATIVE(),
    POLARITY_POSITIVE(),
    MODAL_NEUTRAL(),
    ASPECT_SIMPLE(),
    TENSE_PRESENT()),
  SilFormality(FORCE_NEUTRAL()))
RESTATED I go south.
TRIGGER the player-character heads to the south
VERIFY the player-character's container's place-exit is in the south
TRIGGER the player-character traverses the player-character's container's place-exit in the south
VERIFY E_wnf-hayfield-1_835969_place-exit__836041's exit-portal is open
TRIGGER the player-character relocates to E_wnf-hayfield-1_835969_place-exit__836041's target-place
TRIGGER the player-character is in the barn
TRIGGER the player-character has no visible-items
TRIGGER the player-character surveys the barn
TRIGGER the player-character perceives the player-character
TRIGGER the player-character perceives the barn
TRIGGER the player-character observes the barn's contained-objects except the player-character
TRIGGER the player-character surveys the barn's place-exits and the barn's place-entrances
TRIGGER the player-character perceives the child
TRIGGER the player-character sees the child
TRIGGER the child is the player-character's visible-items
TRIGGER the player-character observes E_wnf-barn-1_835973_place-exit__836050 and E_wnf-barn-1_835973_place-exit__836083 and E_wnf-hayfield-1_835969_place-exit__836041 and E_wnf-hayloft-1_835977_place-exit__836092 E_wnf-barn-1_835973_place-exit__836050's and E_wnf-barn-1_835973_place-exit__836083's and E_wnf-hayfield-1_835969_place-exit__836041's and E_wnf-hayloft-1_835977_place-exit__836092's exit-portals, and E_wnf-barn-1_835973_place-exit__836050's and E_wnf-barn-1_835973_place-exit__836083's and E_wnf-hayfield-1_835969_place-exit__836041's and E_wnf-hayloft-1_835977_place-exit__836092's containers
TRIGGER the player-character perceives E_wnf-hayloft-1_835977_place-exit__836092 and E_wnf-hayfield-1_835969_place-exit__836041 and the zenith and E_wnf-barn-1_835973_place-exit__836083 and the nadir and E_wnf-barn-1_835973_place-exit__836050 and the south and the grassy path and the north
TRIGGER the player-character sees E_wnf-hayloft-1_835977_place-exit__836092 and E_wnf-hayfield-1_835969_place-exit__836041 and the zenith and E_wnf-barn-1_835973_place-exit__836083 and the nadir and E_wnf-barn-1_835973_place-exit__836050 and the south and the grassy path and the north
TRIGGER the grassy path is the player-character's visible-items
VERIFY player-character is the barn's place-discoverer
TRIGGER the player-character discovers the barn
TRIGGER the player-character appraises the barn
TRIGGER the player-character asks, "where am I"
TRIGGER the game-interpreter recites the barn's place-description
TRIGGER the player-character discerns the player-character's visible-items
TRIGGER the player-character asks, "which visible-objects do I see"
TRIGGER the player-character asks, "which visible-fixtures do I see"
VERIFY the player-character is the barn's place-discoverer

OK.
COMMAND where am I

You are in a barn.
COMMAND which visible-objects do I see

You see a child.
COMMAND which visible-fixtures do I see

You see the grassy path to the north.
```

Here's how to interpret the trace:

* ```DIRECTIVE``` indicates an action that takes place automatically (such as the game turn advancing)
* ```TRIGGER``` indicates an action that takes place as an effect of some other action
* ```COMMAND``` indicates parser input (either from the player, or internally, e.g. via an action which causes the player character to automatically ask a question such as "what do I see?")
* ```EXPANDED``` shows how abbreviations in input from the player get expanded
* ```PARSED``` shows the result of parsing the expanded input from the player
* ```RESTATED``` shows how a parsed command is restated as a first-person action
* ```VERIFY``` indicates that some conditional expression is being evaluated

Objects in the trace may be referred to in obvious terms ("the barn"),
or via their internal true names
("E_wnf-hayloft-1_835977_place-exit").

You can toggle debugging off by issuing the ```debug``` command again.

## Parsed Sentences

The ```PARSED``` information can be particularly useful when a
sentence is not being interpreted in the way you expect:

```
COMMAND put the rock in the tree
PARSED 
SilPredicateSentence(
  SilActionPredicate(
    SilPronounReference(
      PERSON_SECOND(),
      GENDER_SOMEONE(),
      COUNT_SINGULAR(),
      DISTANCE_UNSPECIFIED()):665,
    SilSimpleWord("put", "put", ""),
    Some(
      SilDeterminedReference(
        SilStateSpecifiedReference(
          SilNounReference(SilSimpleWord("rock", "rock", "")):675,
          SilAdpositionalState(
            SilAdposition(SilSimpleWord("in", "in", "")),
            SilDeterminedReference(
              SilNounReference(SilSimpleWord("tree", "tree", "")):680,
              DETERMINER_DEFINITE()):682)):688,
        DETERMINER_DEFINITE()):693),
    Nil),
  SilTamImmutable(
    MOOD_IMPERATIVE(),
    POLARITY_POSITIVE(),
    MODAL_NEUTRAL(),
    ASPECT_SIMPLE(),
    TENSE_PRESENT()),
  SilFormality(FORCE_NEUTRAL()))

But I don't know about any such rock.
```

The parser got confused and thought "in the tree" was a restrictive
qualifier for "the rock", where in fact it was supposed to be
modifying the verb "put" with a destination.  Since there is no rock
already in the tree, the parser complains that "the rock in the tree"
doesn't exist.
