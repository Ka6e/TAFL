Feature: Geometric mean calculation
  Computes the geometric mean of two numbers.

  Scenario: User enters two values and program prints their geometric mean
    Given I enter into the console:
      | Value |
      | 4.0     |
      | 9.0     |
    When I execute the program:
      """
      module Main
      let x: float = readNumber();
      let y: float = readNumber();
      let mean: float = (x * y) ** 0.5;
      print(mean);
      """
    Then I should get the output:
      """
      6
      """
