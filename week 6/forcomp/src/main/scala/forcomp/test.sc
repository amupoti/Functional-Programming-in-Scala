val text = "Some string"
val map = text.toLowerCase.filter(c=>c.isLetter).groupBy(c=>c).mapValues(v => v.length).toList.sortBy(_._1)

import forcomp.Anagrams._



combinations(wordOccurrences("some entity"))
