package chapter13

import chapter13.Free._
import chapter3.List
import org.scalatest.FunSuite

class BMICalculator2Test extends FunSuite {


  test("single run") {
    val res: (Unit, Buffers) = runConsoleState(BMICalculator2.bmiProgram()).run(Buffers(List("", "70", "178", "q"), List()))
    assert(res._2.out.reverse() == List(
      BMICalculator2.helpstring,
      "q to quit, any other key continue...",
      "Please enter your weight: ",
      "Please enter your height (cm): ",
      "Your have normal weight and your bmi is: 22.093170054286073",
      "q to quit, any other key continue..."
    ))
  }

  test("multiple runs") {
    val res: (Unit, Buffers) = runConsoleState(BMICalculator2.bmiProgram()).run(Buffers(List("", "70", "178", "", "90", "178", "", "50", "178", "q"), List()))
    assert(res._2.out.reverse() == List(
      BMICalculator2.helpstring,
      "q to quit, any other key continue...",
      "Please enter your weight: ",
      "Please enter your height (cm): ",
      "Your have normal weight and your bmi is: 22.093170054286073",
      "q to quit, any other key continue...",
      "Please enter your weight: ",
      "Please enter your height (cm): ",
      "Your overweight, your bmi is: 28.40550435551067",
      "q to quit, any other key continue...",
      "Please enter your weight: ",
      "Please enter your height (cm): ",
      "Sorry, your underweight, your bmi is: 15.780835753061481",
      "q to quit, any other key continue..."
    ))
  }

  test("wrong input") {
    val res: (Unit, Buffers) = runConsoleState(BMICalculator2.bmiProgram()).run(Buffers(List("", "", "a", "70", "", "b", "178", "q"), List()))
    assert(res._2.out.reverse() == List(
      BMICalculator2.helpstring,
      "q to quit, any other key continue...",
      "Please enter your weight: ",
      "Please enter your weight: ",
      "Please enter your weight: ",
      "Please enter your height (cm): ",
      "Please enter your height (cm): ",
      "Please enter your height (cm): ",
      "Your have normal weight and your bmi is: 22.093170054286073",
      "q to quit, any other key continue..."
    ))
  }

}
