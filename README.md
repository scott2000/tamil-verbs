# tamil-verbs

Conjugate tamil verbs in various forms for different subjects

### Examples

```
> vaa past avan
வந்தான் (vandhaan)
```

```
> பார் future
பார்ப்பேன் (paarppEn)
```

```
> go negative respectful command
செல் (sel): செல்லாதீர்கள் (sellaadheergaL)
போ (pO): போகாதீர்கள் (pOgaadheergaL)
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
> become adhu
ஆனது (aanadhu), ஆயது (aayadhu), ஆயிற்று (aayitRu)
ஆகிறது (aagiRadhu)
ஆம் (aam), ஆகும் (aagum)
```

```
> koNDuvaa guess habitual negative
கொண்டுவருவதில்லை (koNDuvaruvadhillai)
```

### Valid conjugation arguments

- `past`: past tense
- `present`: present tense
- `future`/`habitual`: future tense
- `infinitive`: infinitive
- `adverb`: adverb or AvP
- `adjective`: verbal adjective
- `noun`: verbal noun
- `command`: imperative command
- `negative`: make the conjugation negative
- `respectful`: make the conjugation respectful (for `command`)
- `guess`: if there is no known definition, guess how to conjugate the verb
  based on how it looks and based on similarities to other verbs
- `alternatives`: show alternative conjugations which are valid but less common
- `tamil`: only show the result in Tamil letters
- `latin`/`english`: only show the result in Latin letters

*There are also many abbreviated forms of these arguments which are accepted.*

### Extra features

Many alternative conjugations are supported which can be shown using
`alternative`. Additionally, verbs and subjects can be given using either Latin
or Tamil letters. Arguments can also be separated by hyphens or commas.

```
> be-known
தெரியும் (teriyum)
```

```
> paDi present-nee-latin-alt
paDikkiRaay; paDikkindRaay
```

```
> ஆ past அது tamil alternative
ஆனது, ஆயது, ஆயிற்று; ஆயினது, ஆகினது, ஆயியது, ஆகியது, ஆகிற்று
```

### Vocab lists

There is a built-in vocab list with some very basic verbs, but if you want to
use your own vocab list, you can load a custom list of verbs by either passing a
path to the file as a command-line argument, or by using `:load <file>`. This
repository also contains an extended vocab list called [verbs.txt](verbs.txt)
with some more advanced verbs.

The basic format of a verb entry is:

```
CLASS [PREFIX] VERB. DEFINITION, DEFINITION, ...
```

- `CLASS` is one of `1W`, `1S`, `2W`, `2S`, or `3`
- `PREFIX` is an optional prefix to add before the conjugated verb
  (e.g. for compound verbs)
- `VERB` is the verb in either Latin or Tamil letters
- `DEFINITION` is an English definition for the verb

There are also some special flags for irregular verbs which can be added after
all definitions. These are separated by periods (`.`) like the other sections.

- `defect` makes the verb "defective" in that it will default to only
  conjugating for adhu in the future unless another subject or tense is
  explicitly requested (e.g. for teri since teriyum should be the default)
- `past P` makes `P` be the past tense stem of the verb (e.g. vandh for vaa)
- `stem S` makes `S` be the stem for present and future tense
  (e.g. varu for vaa)
- `future F` makes `F` be the future tense stem for rational nouns
- `adhu A` makes `A` be the future tense stem for adhu and avai
- `inf I` makes `I` be the infinitive root to which -a is added to make the
  infinitive (e.g. pOg for pO)
- `resp R` makes `R` be the respectful command (e.g. vaarungaL for vaa)

Putting it all together, the entry for the compound irregular verb கொண்டுவா
(koNDuvaa) looks like:

```
2W koNDu vaa. bring. past vandh. stem varu. resp vaarungaL
```

### Verb classes

The verb classification this program uses has 5 classes. Each of these classes
differ in many ways, but the differences are easiest to see for the past tense
and the infinitive. There are also a few special cases in each class depending
on the ending of the verb root.

#### 1 Weak

| Special Case | Past | Infinitive | Example |
| --- | --- | --- | --- |
| -டு, -ள் | ட்ட்- | -அ | விடு: விட்டேன், விட |
| -று, -ல் | ற்ற்- | -அ |  பெறு: பெற்றேன், பெற |
| *other* | -த்- | -அ | செய்: செய்தேன், செய்ய |

#### 1 Strong

| Special Case | Past | Infinitive | Example |
| --- | --- | --- | --- |
| -ள் | ட்ட்- | ட்க | கேள்: கேட்டேன், கேட்க |
| -ல் | ற்ற்- | ற்க | கல்: கற்றேன், கற்க
| *other* | -த்த்- | -க்க | பார்: பார்த்தேன், பார்க்க |

#### 2 Weak

| Special Case | Past | Infinitive | Example |
| --- | --- | --- | --- |
| -ள், -ண் | ண்ட்- | -அ | கொள்: கொண்டேன், கொள்ள |
| -ல், -ன் | ன்ற்- | -அ | செல்: சென்றேன், செல்ல |
| *other* | -ந்த்- | -அ | உட்கார்: உட்கார்ந்தேன், உட்கார |

#### 2 Strong

| Special Case | Past | Infinitive | Example |
| --- | --- | --- | --- |
| -ல் | ன்ற்- | ற்க | நில்: நின்றேன், நிற்க |
| *other* | -ந்த்- | -க்க | இரு: இருந்தேன், இருக்க |

#### 3 (Weak)

| Special Case | Past | Infinitive | Example |
| --- | --- | --- | --- |
| -ல் | ன்ன்- | -அ | சொல்: சொன்னேன், சொல்ல |
| *other* | -இன் | -அ | பேசு: பேசினேன், பேச |

