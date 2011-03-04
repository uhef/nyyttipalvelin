package fi.akisaarinen

import org.scalatra._
import org.scalatra.test.scalatest._
import org.scalatest.matchers._

class AlgoholismFilterSuite extends ScalatraFunSuite with ShouldMatchers {
  addFilter(classOf[AlgoholismFilter], "/*")

  test("GET / returns status 200") {
    get("/") { 
      status should equal (200)
    }
  }

  test("GET /foo returns status 404") {
    get("/foo") {
      status should equal (404)
    }
  }

  test("POST / returns status 200 with empty body") {
    post("/") { 
      status should equal (200)
      body should equal("{}\n")
    }
  }

  test("POST / returns knapsack with item that fits") {
    post("/", """{ "name" : "Large contents", "timeout" : 60000, "contents":[{"id":"1","weight":[8180,9604],"value":441},{"id":"2","weight":[349,463],"value":400},{"id":"3","weight":[9534,9462],"value":440}],"capacity":[2000,2000] }""", Map[String,String]()) {
      status should equal (200)
      body should equal("""["2"]""" + "\n")
    }
  }

  test("POST / returns knapsack with several items that fit together") {
    post("/", """{ "name" : "Large contents", "timeout" : 60000, "contents":[{"id":"1","weight":[8180,9604],"value":441},{"id":"2","weight":[349,463],"value":400},{"id":"3","weight":[123,98],"value":440}],"capacity":[2000,2000] }""", Map[String,String]()) {
      status should equal (200)
      body should equal("""["3","2"]""" + "\n")
    }
  }

	test("POST / returns empty solution with empty contents") {
    post("/", """{ "name" : "Empty contents" }""", Map[String,String]()) { 
      status should equal (200)
      body should equal("{}\n")
    }		
	}

  test("POST / returns compacted JSON with line change with wrong value of 'a'") {
    val testJson = """{ "a" : "not_funny" }"""
    post("/", testJson, Map[String,String]()) { 
      status should equal (200)
      body should equal("{}\n")
    }
  }

  test("POST / returns compacted JSON with line change for random json") {
    val testJson = """{ "foo" : "bar" }"""
    post("/", testJson, Map[String,String]()) { 
      status should equal (200)
      body should equal("{}\n")
    }
  }

  test("POST /foo returns status 404") {
    post("/foo") {
      status should equal (404)
    }
  }

  test("Testing") {
    val foo = 1
    expect(2) {
      foo * 2
    }
    foo * 2 should equal (2)
  }

}
