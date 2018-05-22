package com.spiceworks

import scala.io.Source
import scala.collection.mutable.Map

// the complexity of our algorithm is n-squared.
object FindPerson extends App {
    val fileName = "input.csv"
    // if this map becomes too big, move it in a distributed cache
    var seen = Map[String, Boolean]()
    val lines = Source.fromResource(fileName)
        .getLines
        .filterNot(line => line.replace(" ", "") == ",,")
    val listOfSet = groupLocalIds(lines)
    listOfSet.zipWithIndex.foreach{case (set, i) =>
        println(s"""person id ${i + 1} local ids ${set.mkString(",")}""")
    }

    def groupLocalIds(lines: Iterator[String]) : List[Set[String]] = {
        lines.foldLeft(List.empty[Set[String]]){ case (listOfSet, line) => 
            // add to cache because we have already process these local ids.
            val tokens = tokenize(line)
            // if there is a single token in the line which we haven't seen before, then we need to 
            // process the line
            if (tokens.exists(t => seen.get(t).isEmpty)) {
                val result = findMatches(tokens, fileName) 
                addToCache(result)
                result :: listOfSet
            } else listOfSet        
        }
    }

    def addToCache(tokens: Set[String]) : Unit = {
        tokens.foreach{ token => 
            if (seen.get(token).isEmpty) {
                seen += ((token, true))
            }
        }        
    }

    def findMatches(inputSet: Set[String], fileName: String) : Set[String] = {
        Source
            .fromResource(fileName)
            .getLines
            .foldLeft(inputSet){case (acc, line) =>
                val tokensSet = tokenize(line)
                if (acc.intersect(tokensSet).nonEmpty) {
                    acc.union(tokensSet)
                } else acc
            }
    }

    def tokenize(line: String) : Set[String] = line.split(",").toSet.filterNot(_ == "")
}