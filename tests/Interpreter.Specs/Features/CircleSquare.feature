Feature: Circle area calculation
  Calculates the area of a circle using radius input and built-in constants.

  Scenario: User enters radius and program prints computed area
    Given I enter into the console:
      | Value |
      | 5.0   |
    When I execute the program:
      """
      module Main
      let Pi: float = 3.14;
      let r: float = readNumber();
      let area: float = Pi * r ** 2.0;
      print(area);
      """
    Then I should get the output:
      """
      78.50
      """
