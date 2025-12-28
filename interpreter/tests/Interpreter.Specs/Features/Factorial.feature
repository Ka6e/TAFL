Feature: Factorial calculation
  Calculates factorial.

  Scenario: User enters a number
    Given I enter into the console:
      | Value |
      | 5   |
    When I execute the program:
      """
      module Main
      var n = readNumber();
      var result = 1;
      for i = 1, i <= n, i = i + 1 in {
        result = result * i;
      }
      print(result);
      """
    Then I should get the output:
      """
      120
      """
