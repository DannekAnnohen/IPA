module IPA_Test

  (
  test1, test2

  )

where

import IPA2

test0 = test1++test2++test1++test2

test1 = parseIPA nwus_dt
test2 = parseIPA nwus_en

nwus_dt = "aɪns ˈʃtʁɪtn̩ zɪç ˈnɔɐ̯tvɪnt ʊntˈz̥ɔnə | veːɐ̯ fɔn ˈiːnː ˈbaɪdn̩ voːl deːɐ̯ ˈʃtɛɐ̯kəʁə ˈvɛːʁə | als aɪn ˈvandəʁɐ | deːɐ̯ ɪn ˈaɪnː ˈvaːɐ̯mn̩ ˈmantl̩ gəˈhʏlt vaːɐ̯ | dəs ˈveːgəs daˈheːɐ̯kaːm ‖ ziː ˈvʊɐ̯dn̩ ˈaɪnɪç | das ˈdeːɐ̯jeːnɪgə fʏɐ̯ deːn ˈʃtɛɐ̯kəʁən ˈgɛltn̩ ˈzɔltə | deːɐ̯ deːn ˈvandəʁɐ ˈtsvɪŋ̍ ˈvʏɐ̯də | ˈzaɪnː ˈmantl̩ ˈaptsʊˌneːmː ‖ deːɐ̯ ˈnɔɐ̯tvɪnt bliːs mɪt ˈalɐ maxt | ˈaːbɐ jeˈmeːɐ̯ eːɐ̯ bliːs | ˈdɛsto ˈfɛstɐ ˈhʏltə zɪç deːɐ̯ ˈvandəʁɐ ɪn ˈzaɪnː ˈmantl̩ aɪn ‖ ˈɛntlɪç gaːp deːɐ̯ ˈnɔɐ̯tvɪnt deːn kampf aʊf ‖ nuːn ɛɐ̯ˈvɛɐ̯mtə diː ˈzɔnə diː lʊft mɪt ˈiːɐ̯n̩ ˈfʁɔɪntlɪçn̩ ˈʃtʁaːln̩ | ʊnt ʃoːnaːx ˈveːnɪgŋ̍ aʊgŋ̍ˈblɪkŋ̍ tsoːk deːɐ̯ ˈvandəʁɐ ˈzaɪnː ˈmantl̩ aʊs ‖ daː ˈmʊstə deːɐ̯ ˈnɔɐ̯tvɪnt ˈtsuːgeːbm̩ | das diː ˈzɔnə fɔn ˈiːnː ˈbaɪdn̩ deːɐ̯ ˈʃtɛɐ̯kəʁə vaːɐ̯"
nwus_en = "ðə ˈnɔɹθ ˌwɪnd ən ə ˈsʌn wɚ dɪsˈpjuɾɪŋ ˈwɪtʃ wəz ðə ˈstɹɑŋɡɚ wɛn ə ˈtɹævlɚ ˌkem əˈlɑŋ ˈɹæpt ɪn ə ˈwɔɹm ˈklok ðe əˈɡɹid ðət ðə ˈwʌn hu ˈfɚst səkˈsidəd ɪn ˈmekɪŋ ðə ˈtɹævlɚ ˈtek ɪz ˈklok ˌɑf ʃʊd bi kənˈsɪdɚd ˈstɹɑŋɡɚ ðən ðɪ ˈʌðɚ ðɛn ðə ˈnɔɹθ ˌwɪnd ˈblu əz ˈhɑɹd əz hi ˈkʊd bət ðə ˈmɔɹ hi ˈblu ðə ˈmɔɹ ˈklosli dɪd ðə ˈtɹævlɚ ˈfold hɪz ˈklok əˈɹaʊnd hɪm ˌæn ət ˈlæst ðə ˈnɔɹθ ˌwɪnd ˌɡev ˈʌp ði əˈtɛmpt ˈðɛn ðə ˈsʌn ˈʃaɪnd ˌaʊt ˈwɔɹmli ənd ɪˈmidiətli ðə ˈtɹævlɚ ˈtʊk ˌɑf ɪz klok ən ˈso ðə ˈnɔɹθ ˌwɪnd wəz əˈblaɪʒ tɪ kənˈfɛs ðət ðə ˈsʌn wəz ðə ˈstɹɑŋɡɚ əv ðə ˈtu"

special = "ᵈ\865n ɚ\825\865ɝʷ"


-- only pre-effects parsers
test3 = parseIPA "ɚ\825ᵇ\865g"
test5 = parseIPA "ɚ\825ᵇ\865g"


--fails
test4 = parseIPA "ɚ\825\865ᵇ\865g"
test6 = parseIPA "a\865ᵇ\865g"

--ambiguous, could be a\865a with h attached to first or a a with h preattached to second
test7 = parseIPA "aʰ\865a"
