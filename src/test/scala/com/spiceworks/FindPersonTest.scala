package com.spiceworks

import org.scalatest._
import scala.io.Source

class FindPersonTest extends FunSuite {
    test("should be able to incremently build the set as it finds matches") {
        val expected = Set("stu", "vxy", "z12", "345")
        assert(FindPerson.findMatches(Set("stu"), "test.csv") == expected)
    }
    test("should tokenize string correctly") {
        assert(FindPerson.tokenize("a,,") == Set("a"))
    }
}