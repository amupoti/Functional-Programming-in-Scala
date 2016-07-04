val text = "Some string"
val map = text.toLowerCase.filter(c=>c.isLetter).groupBy(c=>c).mapValues(v => v.length).toList.sortBy(_._1)

import forcomp.Anagrams._



combinations(wordOccurrences("cada"))


List(1,2,3).toSet[Int].subsets.map(_.toList).toList

