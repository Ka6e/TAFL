Feature: Sum of two numbers

  Scenario: Program reads two integers and prints their sum
    Given I enter into the console:
      | Value |
      | 5     |
      | 7     |
    When I execute the program:
      """
      let a: int = readNumber();
      let b: int = readNumber();
      let sum: int = a + b;
      print(sum);
      """
    Then I should get the output:
      """
      12
      """
