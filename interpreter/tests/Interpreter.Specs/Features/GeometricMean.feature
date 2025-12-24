Feature: Geometric mean calculation
  Computes the geometric mean of two numbers.

  Scenario: User enters two values and program prints their geometric mean
    Given I enter into the console:
      | Value |
      | 4     |
      | 9     |
    When I execute the program:
      """
      let x: int = readNumber();
      let y: int = readNumber();
      let mean: int = (x * y) ** 0.5;
      print(mean);
      """
    Then I should get the output:
      """
      6
      """
