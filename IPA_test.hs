module IPA_test

where

import IPA

test0 = test1++test2++test1++test2

test1 = parseIPA nwus_dt
test2 = parseIPA nwus_en
testthai = parseIPA (alpha_thai++opec_thai)
testthaiopec = parseIPA opec_thai

testaffricate = "tʰ\865tʰ"

nwus_dt = "aɪns ˈʃtʁɪtn̩ zɪç ˈnɔɐ̯tvɪnt ʊntˈz̥ɔnə | veːɐ̯ fɔn ˈiːnː ˈbaɪdn̩ voːl deːɐ̯ ˈʃtɛɐ̯kəʁə ˈvɛːʁə | als aɪn ˈvandəʁɐ | deːɐ̯ ɪn ˈaɪnː ˈvaːɐ̯mn̩ ˈmantl̩ gəˈhʏlt vaːɐ̯ | dəs ˈveːgəs daˈheːɐ̯kaːm ‖ ziː ˈvʊɐ̯dn̩ ˈaɪnɪç | das ˈdeːɐ̯jeːnɪgə fʏɐ̯ deːn ˈʃtɛɐ̯kəʁən ˈgɛltn̩ ˈzɔltə | deːɐ̯ deːn ˈvandəʁɐ ˈtsvɪŋ̍ ˈvʏɐ̯də | ˈzaɪnː ˈmantl̩ ˈaptsʊˌneːmː ‖ deːɐ̯ ˈnɔɐ̯tvɪnt bliːs mɪt ˈalɐ maxt | ˈaːbɐ jeˈmeːɐ̯ eːɐ̯ bliːs | ˈdɛsto ˈfɛstɐ ˈhʏltə zɪç deːɐ̯ ˈvandəʁɐ ɪn ˈzaɪnː ˈmantl̩ aɪn ‖ ˈɛntlɪç gaːp deːɐ̯ ˈnɔɐ̯tvɪnt deːn kampf aʊf ‖ nuːn ɛɐ̯ˈvɛɐ̯mtə diː ˈzɔnə diː lʊft mɪt ˈiːɐ̯n̩ ˈfʁɔɪntlɪçn̩ ˈʃtʁaːln̩ | ʊnt ʃoːnaːx ˈveːnɪgŋ̍ aʊgŋ̍ˈblɪkŋ̍ tsoːk deːɐ̯ ˈvandəʁɐ ˈzaɪnː ˈmantl̩ aʊs ‖ daː ˈmʊstə deːɐ̯ ˈnɔɐ̯tvɪnt ˈtsuːgeːbm̩ | das diː ˈzɔnə fɔn ˈiːnː ˈbaɪdn̩ deːɐ̯ ˈʃtɛɐ̯kəʁə vaːɐ̯"
nwus_en = "ðə ˈnɔɹθ ˌwɪnd ən ə ˈsʌn wɚ dɪsˈpjuɾɪŋ ˈwɪtʃ wəz ðə ˈstɹɑŋɡɚ wɛn ə ˈtɹævlɚ ˌkem əˈlɑŋ ˈɹæpt ɪn ə ˈwɔɹm ˈklok ðe əˈɡɹid ðət ðə ˈwʌn hu ˈfɚst səkˈsidəd ɪn ˈmekɪŋ ðə ˈtɹævlɚ ˈtek ɪz ˈklok ˌɑf ʃʊd bi kənˈsɪdɚd ˈstɹɑŋɡɚ ðən ðɪ ˈʌðɚ ðɛn ðə ˈnɔɹθ ˌwɪnd ˈblu əz ˈhɑɹd əz hi ˈkʊd bət ðə ˈmɔɹ hi ˈblu ðə ˈmɔɹ ˈklosli dɪd ðə ˈtɹævlɚ ˈfold hɪz ˈklok əˈɹaʊnd hɪm ˌæn ət ˈlæst ðə ˈnɔɹθ ˌwɪnd ˌɡev ˈʌp ði əˈtɛmpt ˈðɛn ðə ˈsʌn ˈʃaɪnd ˌaʊt ˈwɔɹmli ənd ɪˈmidiətli ðə ˈtɹævlɚ ˈtʊk ˌɑf ɪz klok ən ˈso ðə ˈnɔɹθ ˌwɪnd wəz əˈblaɪʒ tɪ kənˈfɛs ðət ðə ˈsʌn wəz ðə ˈstɹɑŋɡɚ əv ðə ˈtu"
opec_thai = "klùm pràtʰêːt̚ pʰûː sòŋ ʔɔ̀ːk nám man rɯ̌ː ʔoːpèːk̚   t͡ɕàʔ̚ prà  t͡ɕʰum tʰîː kruŋ wiːannaː ráwàːŋ wan tʰîː sìp̚ kâːw tʰɯ̌ŋ jîː sìp̚ kanjaːjon pen kaːn prà  t͡ɕʰum taːm pòkkàtìʔ̚ tɛ̀ː dâːj ráp̚ kʰwaːm sǒn t͡ɕaj t͡ɕàːk̚ t͡ɕʰaːw lôːk̚ jàːŋ mâːk̚ pʰrɔ́ʔ̚ raːkʰaː nám man tʰîː kʰɯ̂n sǔːŋ naj pàt̚ t͡ɕùban"
alpha_thai = "i iː ɯ ɯː u uː e eː ɤ ɤː o oː ɛ ɛː ɔ ɔː a aː p pʰ b t tʰ d k kʰ ʔ m n ŋ f s h r j w l p̚ t̚ k̚ ʔ̚ t͡ɕ  t͡ɕʰ  aː͡j  aː͡w  iː͡a  ɯː͡a  uː͡a  uː͡j  eː͡w  ɛː͡w  ɣː͡j  oː͡j  ɔː͡j a͡j  a͡w  i͡a  ɯ͡a  u͡a  u͡j  e͡w  ɛ͡w  ɣ͡j  o͡j  ɔ͡j iː͡a͡w ɯː͡a͡j uː͡a͡j "
-- Die Organisation Erdöl exportierender Länder (OPEC) wird vom 19. bis 20. September ein reguläres Treffen in Wien abhalten, das von der Weltöffentlichkeit aufgrund der derzeit hohen Erdölpreise aufmerksam verfolgt werden wird. 

