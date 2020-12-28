# tamil-verbs

Conjugate Tamil verbs in various forms for different subjects, or generate quiz
questions to practice conjugations.

## Examples

### Requesting conjugations of verbs

There are two ways to request conjugations of verbs. One way is to enter
interactive mode by simply running `tamil-verbs` with no arguments. A prompt
will open where you can request different conjugations. Another way is to
request a conjugation from the command line through `tamil-verbs conjugate`.
Both Tamil verbs and English definitions can be used to find a verb. For a list
of all possible conjugations, see `tamil-verbs conjugate --help`.

```
> vaa past avan
வந்தான் (vandhaan)
```

```
> paar future
பார்ப்பேன் (paarppEn)
```

```
> sollu
சொன்னேன் (sonnEn)
சொல்கிறேன் (solgiREn)
சொல்வேன் (solvEn)
சொல்லி (solli)
சொல்ல (solla)
```

```
> go negative respectful command
செல் (sel):
  செல்லாதீர்கள் (sellaadheergaL)
போ (pO) - go, leave:
  போகாதீர்கள் (pOgaadheergaL)
```

```
> sel past relative avan
சென்றவன் (sendRavan)
```

```
> become adhu
ஆனது (aanadhu), ஆயிற்று (aayitRu)
ஆகிறது (aagiRadhu)
ஆகும் (aagum), ஆம் (aam)
```

```
> koNDuvaa guess conditional
கொண்டுவந்தால் (koNDuvandhaal)
```

### Generating quiz questions

There is also a command to generate quiz questions for conjugations of verbs.
The basic command is `tamil-verbs learn`, but there are many flags that can be
used to configure the generation of questions. See `tamil-verbs learn --help`
for more information.

#### Practice with only past tense `adhu` and adverbs in Latin letters

```
$ tamil-verbs learn --latin 'af/p/a'
```

#### Generate 100 questions and store them in `quiz.txt` and `quiz_key.txt`

```
$ tamil-verbs learn --count 100 --output 'quiz.txt'
```

#### Generate 25 questions on Class 3 verbs and output to `stdout` and `stderr`

```
$ grep ^3 verbs.txt | tamil-verbs learn --list '-' --count 25
```

## Verb lists

There is a built-in verb list with some very basic verbs, but if you want to
use your own verb list, you can load a custom list of verbs by passing a path
to the file as a command-line argument with the option `--list`, by setting the
`TAMIL_VERB_LIST` environment variable, or by using `:load <file>` in
interactive mode (see `:help` for a list of all commands). This repository also
contains an extended verb list called [verbs.txt](verbs.txt) with some more
advanced verbs.

The basic format of a verb entry is:

```
CLASS [PREFIX] VERB. DEFINITION, ...
```

- `CLASS` is one of `1W`, `1S`, `2W`, `2S`, or `3`
- `PREFIX` is an optional prefix to add before the conjugated verb
  (e.g. for compound verbs)
- `VERB` is the verb in either Latin or Tamil letters
- `DEFINITION` is an English definition for the verb (there may be multiple)

Definitions can also end with a note in parentheses that will be displayed
whenever that specific definition is used. This can help clarify ambiguous
definitions such as `break (transitive)` and `break (intransitive)`. Also, if
a particular definition can only be used with inanimate objects, then an
asterisk can be placed at the end to indicate it. This helps avoid conjugations
such as "I happened" or "I was completed" which would be meaningless. For
instance, `naDa` would be defined as `walk, happen*` to indicate that the
seconnd definition can only be used with `adhu`. In order to apply a note or
asterisk to a whole verb instead of just definitions, see `note` and `inanim`
below.

There are also some special flags for irregular verbs which can be added after
all definitions. These are separated by periods (`.`) like the other sections.

- `note` sets a note to be displayed whenever the verb is looked up by any
  definition
- `defect` makes the verb "defective" in that it will default to only
  conjugating for `adhu` in the future unless another subject or tense is
  explicitly requested (e.g. for `pOdhu` since the only common form is `pOdhum`)
- `inanim` makes the verb default to conjugating for `adhu` instead of `naan`
  when no subject is explicitly given (e.g. for `teri` and `puri`)
- `adv V` makes `V` be the adverb form of the verb (e.g. `vandhu` for `vaa`)
- `stem S` makes `S` be the stem for present and future tense of the verb
  (e.g. `varu` for `vaa`)
- `adhu A` makes `A` be the future tense for `adhu` and `avai`
- `inf I` makes `I` be the infinitive form of the verb
- `resp R` makes `R` be the respectful command for the verb
  (e.g. `vaarungaL` for `vaa`)

Any Tamil parts of the verb definition can be given in either Latin or Tamil
letters. In general, Latin letters are preferred since they use less storage
space in vocab lists, and since they are better supported by command-line
interfaces. In order to confirm that verbs are valid Tamil, some additional
checking is done if they are given in Latin letters to make sure that all
sequences of consonants are valid by Tamil grammar rules. If you need to create
a verb with an invalid sequence of consonants, then you should use Tamil letters
directly in the verb entry.

Putting it all together, the entry for the compound irregular verb கொண்டுவா
(`koNDuvaa`) looks like:

```
2W koNDu vaa. bring. adv vandhu. stem varu. resp vaarungaL
```

## Verb classes

The verb classification this program uses has 5 classes. Each of these classes
differ in many ways, but the differences are easiest to see for the past tense
and the future tense. There are also a few special cases in each class depending
on the ending of the verb root.

### 1 Weak

| Special Case | Example | Past | Past Example | Future | Future Example |
| --- | --- | --- | --- | --- | --- |
| -டு | விடு | ட்ட்- | விட்டேன் | -வ்- | விடுவேன் |
| -று |  பெறு | ற்ற்- | பெற்றேன் | -வ்- | பெறுவேன் |
| *other* | செய் | -த்- | செய்தேன் | -வ்- | செய்வேன் |

### 1 Strong

| Special Case | Example | Past | Past Example | Future | Future Example |
| --- | --- | --- | --- | --- | --- |
| -ள் | கேள் | ட்ட்- | கேட்டேன் | ட்ப்- | கேட்பேன் |
| -ல் | கல் | ற்ற்- | கற்றேன் | ற்ப்- | கற்பேன் |
| *other* | பார் | -த்த்- | பார்த்தேன் | -ப்ப்- | பார்ப்பேன் |

### 2 Weak

| Special Case | Example | Past | Past Example | Future | Future Example |
| --- | --- | --- | --- | --- | --- |
| -ண் | உண் | ண்ட்- | உண்டேன் | -ப்- | உண்பேன் |
| -ன் | தின் | ன்ற்- | தின்றேன் | -ப்- | தின்பேன் |
| -ள் | கொள் | ண்ட்- | கொண்டேன் | -வ்- | கொள்வேன் |
| -ல் | செல் | ன்ற்- | சென்றேன் | -வ்- | செல்வேன் |
| *other* | உட்கார் | -ந்த்- | உட்கார்ந்தேன் | -வ்- | உட்கார்வேன் |

### 2 Strong

| Special Case | Example | Past | Past Example | Future | Future Example |
| --- | --- | --- | --- | --- | --- |
| -ல் | நில் | ன்ற்- | நின்றேன் | ற்ப்- | நிற்பேன் |
| *other* | இரு | -ந்த்- | இருந்தேன் | -ப்ப்- | இருப்பேன் |

### 3 (Weak)

| Special Case | Example | Past | Past Example | Future | Future Example |
| --- | --- | --- | --- | --- | --- |
| -ல் *(short)* | சொல் | ன்ன்- | சொன்னேன் | -வ்- | சொல்வேன் |
| -*(long vowel)* | ஆ (ஆகு) | -ன்- | ஆனேன் | -வ்- | ஆவேன் |
| *other* | பேசு | -இன்- | பேசினேன் | -வ்- | பேசுவேன் |

## Transliteration

This program uses a custom style of transliteration. Capital letters are treated
differently than lowercase letters to allow for more distinctions than are
present in English. There are sometimes multiple ways to write the same letter
to allow for more natural spellings. Most of these are fairly obvious, but due
to the limited number of symbols, some concessions had to be made. I tried to
make the strange spellings limited to uncommon words.

### Vowels

Short vowels can be written very simply:

| அ | இ | உ | எ | ஒ |
| --- | --- | --- | --- | --- |
| a | i | u | e | o |

But there are multiple ways to write long vowels:

| ஆ | ஈ | ஊ | ஏ | ஓ | ஐ | ஔ |
| --- | --- | --- | --- | --- | --- | --- |
| aa | ee | oo | ae | oh | ai | au |
| A | I | U | E | O | | |

### Consonants

There are lots of ways to write hard consonants due to their differences in
pronunciation depending on position in the word. Each one can also optionally be
followed by `h` (`kh`, `ch`, `th`, `bh`, etc).

| க் | ச் | ட் | த் | ப் | ற் |
| --- | --- | --- | --- | --- | --- |
| k | c | T | t | p | R\*\* |
| g | s | D | d | b | |
| h\* | j | | | | |

\**only between vowels or after medium consonants other than `zh`*

\*\**can be written as `tR` or `tr` when doubled instead of `RR`*

Soft consonants have some unique ways of writing each letter:

| ங் | ஞ் | ண் | ந் | ம் | ன் |
| --- | --- | --- | --- | --- | --- |
| ng~ | ny | N | n\* | m | n |

\**must be written as `nh` when not the first letter in a word*

But there are also some combined forms for the common case of the soft consonant
being followed by its corresponding hard consonant:

| ங்க் | ஞ்ச் | ண்ட் | ந்த் | ம்ப் | ன்ற் |
| --- | --- | --- | --- | --- | --- |
| ng | nj | ND | ndh / nd | mb | ndR / ndr |

Medium consonants are the simplest consonants:

| ய் | ர் | ல் | வ் | ழ் | ள் |
| --- | --- | --- | --- | --- | --- |
| y / Y | r | l | v / w | zh / z | L |

### Less Common Symbols

The aaydham (ஃ) is written as `K` giving `eKgu` for எஃகு. Non-Tamil sounds that
aren't common in verbs can also be used, but these require uppercase letters:

| ஜ் | ஸ் | ஹ் | ஷ் | ஶ் | ஃப் | ஃஜ் | ஃஸ் | க்ஷ் | ஸ்ரீ |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| J | S | H | Sh | SSh | F | Z | X | kSh | Sree / SShree |

