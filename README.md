# tamil-verbs

Conjugate tamil verbs in various forms for different subjects

## Examples

```
> vaa past avan
வந்தான் (vandhaan)
```

```
> paar future
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
> sel past relative avan
சென்றவன் (sendRavan)
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

## Valid conjugation arguments

- `past`: past tense
- `present`: present tense
- `future`/`habitual`: future tense
- `infinitive`: infinitive
- `adverb`: adverb or AvP
- `adjective`: verbal adjective
- `relative`: relative noun referring to someone who does the action
- `noun`: verbal noun referring to the action of the verb
- `conditional`/`if`: conditional clauses like "if" in English
- `command`: imperative command
- `negative`: make the conjugation negative
- `respectful`: make the conjugation respectful (for `command`)
- `guess`: if there is no known definition, guess how to conjugate the verb
  based on how it looks and based on similarities to other verbs
- `alternative`: show alternative conjugations which are valid but less common
- `tamil`: only show the result in Tamil letters
- `latin`/`english`: only show the result in Latin letters

*There are also many abbreviated forms of these arguments which are accepted.*

## Extra features

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
> சொல் past அது tamil alternative
சொன்னது, சொல்லியது, சொல்லிற்று; சொல்லினது
```

## Vocab lists

There is a built-in vocab list with some very basic verbs, but if you want to
use your own vocab list, you can load a custom list of verbs by either passing a
path to the file as a command-line argument, or by using `:load <file>`. This
repository also contains an extended vocab list called [verbs.txt](verbs.txt)
with some more advanced verbs.

The basic format of a verb entry is:

```
CLASS [PREFIX] VERB. DEFINITION, ...
```

- `CLASS` is one of `1W`, `1S`, `2W`, `2S`, or `3`
- `PREFIX` is an optional prefix to add before the conjugated verb
  (e.g. for compound verbs)
- `VERB` is the verb in either Latin or Tamil letters
- `DEFINITION` is an English definition for the verb (there may be multiple)

There are also some special flags for irregular verbs which can be added after
all definitions. These are separated by periods (`.`) like the other sections.

- `defect` makes the verb "defective" in that it will default to only
  conjugating for `adhu` in the future unless another subject or tense is
  explicitly requested (e.g. for `teri` since `teriyum` should be the default)
- `adv V` makes `V` be the adverb form of the verb (e.g. `vandhu` for `vaa`)
- `stem S` makes `S` be the stem for present and future tense of the verb
  (e.g. `varu` for `vaa`)
- `adhu A` makes `A` be the future tense for `adhu` and `avai`
- `inf I` makes `I` be the infinitive form of the verb
- `resp R` makes `R` be the respectful command for the verb
  (e.g. `vaarungaL` for `vaa`)

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

