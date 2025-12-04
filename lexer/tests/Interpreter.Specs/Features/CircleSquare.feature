Feature: Circle area calculation
  Calculates the area of a circle using radius input and built-in constants.

  Scenario: User enters radius and program prints computed area
    Given I enter into the console:
      | Value |
      | 5     |
    When I execute the program:
      """
      let r: int = readNumber();
      let area: int = Pi * r ** 2;
      print(area);
      """
    Then I should get the output:
      """
      78.53982
      """
