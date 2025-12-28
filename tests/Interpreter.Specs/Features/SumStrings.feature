Feature: Sum strings
  Concatinate strings.

  Scenario: User enters strings
    Given I enter into the console:
      | Value |
      | Hello     |
      | World     |
    When I execute the program:
      """
      module Main

      func sum(a: string, b:string): string {
        return a + b;
      }
      var a = readString();
      var b = readString();
      var strings = sum(a,b);
      print(strings);
      """
    Then I should get the output:
      """
      HelloWorld
      """
