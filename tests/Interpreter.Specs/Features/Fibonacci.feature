Feature: Fibonacii calculation
  Calculates fibonacci.

  Scenario: User enters a number
    Given I enter into the console:
      | Value |
      | 10   |
    When I execute the program:
      """
      module Main
      func fib(n: int): int {
      if  n < 0 then {
        return 0;
      }
      if n <= 1 then {
        return n;
      }

        return fib(n - 1) + fib(n - 2);
      }

      var input = readNumber();
      let result: int = fib(input);
      print(result);
      """
    Then I should get the output:
      """
      55
      """
